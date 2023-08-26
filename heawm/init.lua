#!/usr/bin/luajit
local uv = require('luv')

local _ = require('ffi')
local ffi_cast, ffi_copy, ffi_gc, ffi_new, ffi_sizeof, ffi_string, ffi_typeof =
	_.cast, _.copy, _.gc, _.new, _.sizeof, _.string, _.typeof

local _ = require('bit')
local band, bor, bnot, lshift, rshift, bxor =
	_.band, _.bor, _.bnot, _.lshift, _.rshift, _.bxor

local c = require('heawm.ffi')
local Rect = require('heawm.rect')

local M = {}
M.__index = M

M.config = setmetatable({}, {
	__index = function(_, k)
		error(string.format("invalid key '%s'", k))
	end,
})

M.config.label_font_family = 'monospace'
M.config.label_font_size = 21

function M:set_tree_outputs(tree, outputs)
	tree.outputs = outputs
	self:dirty_tree_layout(tree)
end

function M:send_window_net_wm_state(window, hidden, focused)
	local list = {}

	if hidden then
		table.insert(list, self.atoms._NET_WM_STATE_HIDDEN)
	end

	if focused then
		table.insert(list, self.atoms._NET_WM_STATE_FOCUSED)
	end

	c.xcb_change_property(
		self.conn,
		c.XCB_PROP_MODE_REPLACE,
		window.window_id,
		self.atoms._NET_WM_STATE,
		self.atoms.ATOM,
		32,
		#list,
		ffi_new('xcb_atom_t[?]', #list, unpack(list))
	):discard_check(self.conn)
end

function M:set_window_focused(window, focused)
	if window.focused == focused then
		return
	end
	window.focused = focused

	self:dirty_tree_layout(window.tree)
end

function M:set_window_bell(window, bell)
	if window.bell == bell then
		return
	end
	window.bell = bell

	self:dirty_tree_layout(window.tree)
end

function M:window_history(window)
	return not window.console
		and (
			window.net_wm_window_type._NET_WM_WINDOW_TYPE_NORMAL
			or window.net_wm_window_type._NET_WM_WINDOW_TYPE_DIALOG
		)
end

function M:window_label_text_color()
	return 0xffff00
end

function M:window_label_background_color(window)
	if window.focused then
		return 0xff0000
	end
	return -1
end

function M:window_border(window)
	if window.bell then
		return 2, 0xffff00
	elseif window.focused then
		return 2, 0xff0000
	else
		return 1, 0x000000
	end
end

function M:window_gap(compact)
	if compact then
		return 0, 0
	end
	return 2, 3
end

function M:apply_window_layout_tiled(window, geom)
	self:set_window_mapped(window, true)
	self:set_window_geom(window, geom)
	self:set_window_border(window, self:window_border(window))

	self:set_label_mapped(window.label, true)
	self:set_label_text_color(window.label, self:window_label_text_color(window))
	self:set_label_background_color(
		window.label,
		self:window_label_background_color(window)
	)
end

function M:apply_window_layout_fullscreen(window, geom)
	self:set_window_mapped(window, true)
	self:set_window_geom(window, geom)
	self:set_window_border(window, 0)

	self:set_label_mapped(window.label, false)
end

function M:apply_window_layout_console(window, parent_geom)
	local width = parent_geom.width / 1.6
	local height = parent_geom.height / 1.6 / 1.6
	local geom = {
		x = (parent_geom.width - width) / 2,
		y = 50,
		height = height,
		width = width,
	}

	self:apply_window_layout_tiled(window, geom)
	self:set_label_mapped(window.label, false)
end

function M:window_focused(window)
	for _, user in ipairs(self.users) do
		if user.focused_window == window then
			return true
		end
	end
	return false
end

function M:update_window_focused(window)
	self:set_window_focused(window, self:window_focused(window))
end

function M:get_console()
	for _, window in pairs(self.windows_by_id) do
		if window.console then
			return window
		end
	end
end

function M:restore_user_focused_window(user, opts)
	local window = user.prev_focused_window
	if window then
		self:set_user_focused_window(user, window, opts)
	end
end

function M:set_user_focused_window(user, window, opts)
	local prev = user.focused_window
	local curr = window

	if curr == prev then
		return
	end

	user.focused_window = curr
	user.focus_opts = opts or {}

	if curr then
		user.focused_id = curr.window_id
	else
		user.focused_id = self.input_window_id
	end

	local history = false
	if prev then
		self:update_window_focused(prev)

		history = self:window_history(prev)
		if history then
			user.prev_focused_window = prev
		end
	end

	if curr then
		self:update_window_focused(curr)
		self:update_user_window_bindings(user, curr)

		if history and not curr.opener_window then
			curr.opener_window = prev
		end
	end

	self:update_user_keymap(user)

	self:dirty_user(user)
end

-- Use "setxkbmap -print" to obtain keymap.
function M:set_user_keymap(user, keymap)
	if user.keymap == keymap then
		return
	end

	user.keymap = keymap

	self:dirty_user(user)
end

function M:get_user_virtual_input(user)
	return require('heawm.virtual-input'):new(
		self.conn,
		user.master_keyboard,
		user.master_pointer
	)
end

function M:find_window_by_class(tree, class)
	for _, window in ipairs(tree.window_stack) do
		if window.wm_class.class == class then
			return window
		end
	end
end

function M:find_window_by_label(tree, label)
	for _, window in ipairs(tree.window_stack) do
		if window.callsign == label then
			return window
		end
	end
end

function M:send_user_focus(user)
	c.xcb_input_xi_set_focus(
		self.conn,
		user.focused_id,
		c.XCB_CURRENT_TIME,
		user.master_keyboard.id
	)
		:discard_check(self.conn)

	c.xcb_input_xi_set_client_pointer(
		self.conn,
		user.focused_id,
		user.master_pointer.id
	)
		:discard_check(self.conn)
end

do
	local xcb_icccm_wm_state_data_t = ffi_typeof([[
		struct {
			uint32_t state;
			xcb_window_t icon;
		}
	]])

	function M:send_window_icccm_wm_state(window_id, state)
		-- http://tronche.com/gui/x/icccm/sec-4.html#s-4.1.3.1
		if state == c.XCB_ICCCM_WM_STATE_WITHDRAWN then
			c.xcb_delete_property(self.conn, window_id, self.atoms.WM_STATE)
				:discard_check(self.conn)
		else
			c.xcb_change_property(
				self.conn,
				c.XCB_PROP_MODE_REPLACE,
				window_id,
				self.atoms.WM_STATE,
				self.atoms.WM_STATE,
				32,
				ffi_sizeof(xcb_icccm_wm_state_data_t) / 4,
				ffi_new(xcb_icccm_wm_state_data_t, {
					state = state,
					icon = c.XCB_WINDOW_NONE,
				})
			):discard_check(self.conn)
		end
	end
end

local function split_modifiers(s)
	local mods_table = {}
	while true do
		local mod_name, rest = string.match(s, '^([^+]+)%+(.+)')
		if not mod_name then
			return mods_table, s
		end
		mods_table[mod_name] = true
		s = rest
	end
end

local XKB_MOD_NAME_TO_XCB_MOD_MASK = {
	Shift = c.XCB_MOD_MASK_SHIFT,
	Lock = c.XCB_MOD_MASK_LOCK,
	Control = c.XCB_MOD_MASK_CONTROL,
	Mod1 = c.XCB_MOD_MASK_1,
	Mod2 = c.XCB_MOD_MASK_2,
	Mod3 = c.XCB_MOD_MASK_3,
	Mod4 = c.XCB_MOD_MASK_4,
	Mod5 = c.XCB_MOD_MASK_5,
}

local function to_xcb_mod_mask(mods_table, name_to_mod_mask)
	local result = 0
	for name, enabled in pairs(mods_table) do
		if enabled then
			local mod_mask = assert(
				XKB_MOD_NAME_TO_XCB_MOD_MASK[name] or name_to_mod_mask[name],
				string.format("invalid modifier '%s'", name)
			)
			result = bor(result, mod_mask)
		end
	end
	return result
end

function M:receive_device_keymap(device_id)
	local modifiers_cookie =
		c.xcb_input_get_device_modifier_mapping_unchecked(self.conn, device_id)
	local keymap = ffi_gc(
		c.xkb_x11_keymap_new_from_device(
			self.xkb_context,
			self.conn,
			device_id,
			c.XKB_KEYMAP_COMPILE_NO_FLAGS
		),
		c.xkb_keymap_unref
	)
	local modifiers_reply = modifiers_cookie:unsafe_reply(self.conn)
	if keymap == nil or modifiers_reply == nil then
		return
	end

	local buffer = ffi_new('char[?]', 128)
	local keysyms = ffi_new('const xkb_keysym_t *[1]')

	local keycode_to_mod_mask = {}
	local keymaps = modifiers_reply:keymaps()
	for i = 0, 7 do
		local mod_mask = lshift(1, i)
		local keycodes = keymaps + i * modifiers_reply.keycodes_per_modifier
		for i = 0, modifiers_reply.keycodes_per_modifier - 1 do
			local keycode = keycodes[i]
			if keycode ~= c.XCB_NO_SYMBOL then
				keycode_to_mod_mask[keycode] =
					bor(keycode_to_mod_mask[keycode] or 0, mod_mask)
			end
		end
	end

	local name_to_mod_mask = {}
	local mod_mask_to_name = {}
	local name_to_desc = {}
	local keysym_to_name = {}

	-- xkb_state_key_get_layout(struct xkb_state *state, xkb_keycode_t key);

	local mask_to_mods_table = {}
	local num_mods = keymap:num_mods()

	local masks_size = 16
	local masks = ffi_new('xkb_mod_mask_t[?]', masks_size)

	for keycode = keymap:min_keycode(), keymap:max_keycode() do
		local num_layouts = keymap:num_layouts_for_key(keycode)
		for layout = 0, num_layouts - 1 do
			local num_levels = keymap:num_levels_for_key(keycode, layout)
			for level = 0, num_levels - 1 do
				local num_syms =
					keymap:key_get_syms_by_level(keycode, layout, level, keysyms)
				for i = 0, num_syms - 1 do
					local keysym = keysyms[0][i]
					local len = c.xkb_keysym_get_name(keysym, buffer, ffi_sizeof(buffer))
					assert(len > 0)
					local name = ffi_string(buffer, len)

					local mod_mask = keycode_to_mod_mask[keycode]
					if mod_mask then
						local mod_mask = bor(name_to_mod_mask[name] or 0, mod_mask)
						name_to_mod_mask[name] = mod_mask
						local mod_name = string.match(name, '(.-)_?[LR]?$')
						name_to_mod_mask[mod_name] = mod_mask
						mod_mask_to_name[mod_mask] = name
					end

					local num_masks = tonumber(
						keymap:key_get_mods_for_level(keycode, 0, level, masks, masks_size)
					)
					assert(num_masks < masks_size)

					for i = 0, num_masks - 1 do
						local mask = masks[i]

						local mods_table = mask_to_mods_table[mask]
						if not mods_table then
							mods_table = {}
							mask_to_mods_table[mask] = mods_table

							for i = 0, num_mods - 1 do
								if band(mask, lshift(1, i)) ~= 0 then
									local mod_name = ffi_string(keymap:mod_get_name(i))
									mods_table[mod_name] = true
								end
							end
						end

						name_to_desc[name] = name_to_desc[name] or {}
						table.insert(name_to_desc[name], {
							mods_table = mods_table,
							code = keycode,
							layout = layout,
						})
						keysym_to_name[keysym] = name
					end
				end
			end
		end
	end

	return {
		name_to_mod_mask = name_to_mod_mask,
		mod_mask_to_name = mod_mask_to_name,
		name_to_desc = name_to_desc,
		keysym_to_name = keysym_to_name,
	}
end

function M:set_user_bindings(user, window_id, bindings)
	if user.bindings[window_id] == bindings then
		return
	end

	user.bindings[window_id] = bindings

	if not user.dirty_bindings then
		user.dirty_bindings = {}
	end
	user.dirty_bindings[window_id] = bindings or {}
	self:dirty_user(user)
end

local BUTTON_CODES = {
	BUTTON_LEFT = 1,
	BUTTON_MID = 2,
	BUTTON_RIGHT = 3,
	WHEEL_UP = 4,
	WHEEL_DOWN = 5,
}

local function parse_numeric_button(detail_name)
	return tonumber(string.match('^BUTTON_(%d+)$', detail_name))
end

local function parse_numeric_key(detail_name)
	local code = tonumber(string.match('^KEY_(%d+)$', detail_name))
	if code then
		return {
			{
				code = code,
				mods_table = {},
			},
		}
	end
end

local function clear_device_bindings_helper(self, device, window_id, grab_type)
	device.bindings[window_id] = {}

	c.xcb_input_xi_passive_ungrab_device(
		self.conn,
		window_id,
		c.XCB_GRAB_ANY,
		device.id,
		1,
		grab_type,
		ffi_new('uint32_t[1]', c.XCB_INPUT_MODIFIER_MASK_ANY)
	):discard_check(self.conn)
end

local function clear_device_button_bindings(self, device, window_id)
	clear_device_bindings_helper(
		self,
		device,
		window_id,
		c.XCB_INPUT_GRAB_TYPE_BUTTON
	)
end

local function clear_device_key_bindings(self, device, window_id)
	clear_device_bindings_helper(
		self,
		device,
		window_id,
		c.XCB_INPUT_GRAB_TYPE_KEYCODE
	)
end

local function commit_device_button_binding(
	self,
	pointer,
	keyboard,
	window_id,
	mod_mask,
	detail_name,
	callback
)
	local window_bindings = pointer.bindings[window_id]

	local code = BUTTON_CODES[detail_name]
		or parse_numeric_button(detail_name)
		or error(string.format("invalid button: '%s'", detail_name))

	window_bindings[code] = window_bindings[code] or {}
	table.insert(window_bindings[code], {
		mod_mask = mod_mask,
		callback = callback,
	})

	c.xcb_input_xi_passive_grab_device_unchecked(
		self.conn,
		c.XCB_CURRENT_TIME,
		window_id,
		c.XCB_CURSOR_NONE,
		code,
		pointer.id,
		2,
		1,
		c.XCB_INPUT_GRAB_TYPE_BUTTON,
		c.XCB_INPUT_GRAB_MODE_22_ASYNC,
		c.XCB_INPUT_GRAB_MODE_22_ASYNC,
		c.XCB_INPUT_GRAB_OWNER_NO_OWNER,
		ffi_new(
			'uint32_t[1]',
			bor(
				c.XCB_INPUT_XI_EVENT_MASK_MOTION,
				c.XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS,
				c.XCB_INPUT_XI_EVENT_MASK_BUTTON_RELEASE
			)
		),
		ffi_new(
			'uint32_t[2]',
			mod_mask,
			bor(keyboard.keymap.name_to_mod_mask['Num_Lock'] or 0, mod_mask)
		)
	):discard_reply(self.conn)
end

local function commit_device_key_binding(
	self,
	keyboard,
	window_id,
	base_mod_mask,
	detail_name,
	callback
)
	local window_bindings = keyboard.bindings[window_id]

	for _, desc in
		ipairs(
			keyboard.keymap.name_to_desc[detail_name]
				or parse_numeric_key(detail_name)
				or error(string.format("invalid key '%s'", detail_name))
		)
	do
		local mod_mask = bor(base_mod_mask, to_xcb_mod_mask(desc.mods_table, {}))

		window_bindings[desc.code] = window_bindings[desc.code] or {}
		table.insert(window_bindings[desc.code], {
			mod_mask = mod_mask,
			layout = desc.layout,
			callback = callback,
		})

		c.xcb_input_xi_passive_grab_device_unchecked(
			self.conn,
			c.XCB_CURRENT_TIME,
			window_id,
			c.XCB_CURSOR_NONE,
			desc.code,
			keyboard.id,
			2,
			1,
			c.XCB_INPUT_GRAB_TYPE_KEYCODE,
			c.XCB_INPUT_GRAB_MODE_22_ASYNC,
			c.XCB_INPUT_GRAB_MODE_22_ASYNC,
			c.XCB_INPUT_GRAB_OWNER_NO_OWNER,
			ffi_new(
				'uint32_t[1]',
				bor(
					c.XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
					c.XCB_INPUT_XI_EVENT_MASK_KEY_RELEASE
				)
			),
			ffi_new(
				'uint32_t[2]',
				mod_mask,
				bor(keyboard.keymap.name_to_mod_mask['Num_Lock'] or 0, mod_mask)
			)
		):discard_reply(self.conn)
	end
end

local function get_binding_detail_type(detail_name)
	if
		string.match(detail_name, '^BUTTON_')
		or string.match(detail_name, '^WHEEL_')
	then
		return 'button'
	else
		return 'key'
	end
end

local function commit_user_window_bindings(self, user, window_id, bindings)
	local keyboard = user.master_keyboard
	local pointer = user.master_pointer

	if not keyboard.keymap then
		keyboard.keymap = self:receive_device_keymap(keyboard.id)
	end
	if not keyboard.keymap then
		return
	end

	clear_device_button_bindings(self, pointer, window_id)
	clear_device_key_bindings(self, keyboard, window_id)

	for lhs, callback in pairs(bindings or {}) do
		local mods_table, detail_name = split_modifiers(lhs)
		local mod_mask =
			to_xcb_mod_mask(mods_table, keyboard.keymap.name_to_mod_mask)

		local detail_type = get_binding_detail_type(detail_name)
		if detail_type == 'button' then
			commit_device_button_binding(
				self,
				pointer,
				keyboard,
				window_id,
				mod_mask,
				detail_name,
				callback
			)
		elseif detail_type == 'key' then
			commit_device_key_binding(
				self,
				keyboard,
				window_id,
				mod_mask,
				detail_name,
				callback
			)
		else
			assert(false)
		end
	end
end

function M:manage_randr_monitors(root_id)
	if not self.randr_ext then
		return
	end

	local cookie = c.xcb_randr_select_input_checked(
		self.conn,
		root_id,
		c.XCB_RANDR_NOTIFY_MASK_SCREEN_CHANGE
	)

	self:query_randr_monitors(root_id)

	cookie:assert_check(self.conn)

	self.trees_by_id[root_id].has_xrandr = true
end

function M:force_randr_configuration_up_to_date(root_id)
	c.xcb_randr_get_screen_resources_unchecked(self.conn, root_id)
		:discard_reply(self.conn)
end

function M:query_randr_monitors(root_id)
	self:force_randr_configuration_up_to_date(root_id)

	local outputs = {}

	local reply = c.xcb_randr_get_monitors(self.conn, root_id, false)
		:assert_reply(self.conn)

	for monitor in reply:monitors() do
		local output = {
			geom = {
				x = monitor.x,
				y = monitor.y,
				width = monitor.width,
				height = monitor.height,
			},
			primary = monitor.primary ~= 0,
			name = self.atoms[monitor.name],
		}
		table.insert(outputs, output)
	end

	local tree = self.trees_by_id[root_id]
	self:set_tree_outputs(tree, outputs)
end

local function should_ignore_event(event)
	local mode = event.mode
	return (
		mode ~= c.XCB_INPUT_NOTIFY_MODE_NORMAL
		and mode ~= c.XCB_INPUT_NOTIFY_MODE_WHILE_GRABBED
	)
end

function M:get_label_key(label)
	return string.format(
		'%s,%s,%s',
		label.text,
		label.text_color,
		label.background_color
	)
end

function M:get_label_pixmap(label)
	local key = self:get_label_key(label)
	local cache = label.tree.pixmap_cache
	local pixmap = cache[key]

	if not pixmap then
		pixmap = self:create_label_pixmap(label)
		cache[key] = pixmap
	end

	return pixmap
end

function M:create_label_pixmap(label)
	local tree = label.tree

	local pixmap_id = self.conn:generate_id()
	do
		c.xcb_create_pixmap(self.conn, 24, pixmap_id, tree.window_id, 100, 100)
			:discard_check(self.conn)

		local surface = ffi_gc(
			c.cairo_xcb_surface_create(self.conn, pixmap_id, tree.visual, 100, 100),
			c.cairo_surface_destroy
		)
		assert(surface ~= nil)

		local cr = ffi_gc(c.cairo_create(surface), c.cairo_destroy)
		assert(cr ~= nil)

		self:render_label_pixmap(label, cr, false)
	end

	local shape_id = self.conn:generate_id()
	do
		c.xcb_create_pixmap(self.conn, 1, shape_id, label.window_id, 100, 100)
			:discard_check(self.conn)

		local surface = ffi_gc(
			c.cairo_xcb_surface_create_for_bitmap(
				self.conn,
				tree.screen,
				shape_id,
				100,
				100
			),
			c.cairo_surface_destroy
		)
		assert(surface ~= nil)

		local cr = ffi_gc(c.cairo_create(surface), c.cairo_destroy)
		assert(cr ~= nil)

		self:render_label_pixmap(label, cr, true)
	end

	return {
		pixmap_id = pixmap_id,
		shape_id = shape_id,
	}
end

local function color_to_floats(c)
	return rshift(c, 16) / 0xff,
		band(rshift(c, 8), 0xff) / 0xff,
		band(c, 0xff) / 0xff
end

function M:render_label_pixmap(label, cr, shape)
	local font_size = self.config.label_font_size

	cr:set_antialias(c.CAIRO_ANTIALIAS_NONE)
	cr:select_font_face(
		self.config.label_font_family,
		c.CAIRO_FONT_SLANT_NORMAL,
		c.CAIRO_FONT_WEIGHT_BOLD
	)
	cr:set_font_size(font_size)

	if shape then
		cr:set_operator(c.CAIRO_OPERATOR_CLEAR)
		cr:rectangle(0, 0, 100, 100)
		cr:fill()
	end

	cr:translate(100 / 2, 100 / 2)

	local te = ffi_new('cairo_text_extents_t[1]')
	cr:text_extents(label.text, te)
	local te = te[0]

	cr:arc(0, 0, font_size * 1.31 / 2, 0, 2 * math.pi)

	if shape then
		cr:set_operator(c.CAIRO_OPERATOR_OVER)
	end

	if label.background_color >= 0 then
		cr:set_source_rgb(color_to_floats(label.background_color))
		cr:fill()
	else
		cr:new_path()
	end

	cr:set_antialias(c.CAIRO_ANTIALIAS_BEST)

	local x = -(te.width + te.x_bearing) / 2
	local y = -te.y_bearing - te.height / 2

	if shape then
		cr:set_operator(c.CAIRO_OPERATOR_OVER)
	else
		cr:set_source_rgb(0, 0, 0)
	end

	cr:move_to(x, y)
	cr:text_path(label.text)
	cr:set_line_width(2.5)
	cr:stroke()

	if not shape then
		cr:set_source_rgb(color_to_floats(label.text_color))
	end
	cr:move_to(x, y)
	cr:show_text(label.text)

	c.cairo_surface_flush(cr:get_target())
end

function M:handle_label_expose(label, event)
	c.xcb_clear_area(
		self.conn,
		0,
		label.window_id,
		event.x,
		event.y,
		event.width,
		event.height
	)
		:discard_check(self.conn)
end

local function execute_device_bindings(self, event, event_type)
	local device = self.devices_by_id[event.deviceid]

	local window_bindings = device.bindings[event.event]
	if not window_bindings then
		return
	end

	local bindings = window_bindings[event.detail]
	if not bindings then
		return
	end

	local tree = self.trees_by_id[event.event]
	local user = device.user

	local arg = {
		tree = tree,
		device = device,
		user = user,
		event = event_type,
	}

	local keyboard = user.master_keyboard
	local mod_ignore = keyboard.keymap.name_to_mod_mask['Num_Lock'] or 0
	local mod_mask = band(event.mods.effective, bnot(mod_ignore))

	for _, binding in ipairs(bindings) do
		if binding.mod_mask == mod_mask then
			binding.callback(self, arg)
		end
	end
end

local function handle_input_key_press(self, event)
	local event = ffi_cast('xcb_input_key_press_event_t &', event)
	local repeats = band(event.flags, c.XCB_INPUT_KEY_EVENT_FLAGS_KEY_REPEAT) ~= 0
	execute_device_bindings(self, event, repeats and 'repeat' or 'press')
end

local function handle_input_key_release(self, event)
	local event = ffi_cast('xcb_input_key_release_event_t &', event)
	execute_device_bindings(self, event, 'release')
end

local function handle_input_button_press(self, event)
	local event = ffi_cast('xcb_input_button_press_event_t &', event)
	execute_device_bindings(self, event, 'press')
end

local function handle_input_button_release(self, event)
	local event = ffi_cast('xcb_input_button_release_event_t &', event)
	execute_device_bindings(self, event, 'release')
end

local function handle_input_focus_in(self, event)
	local event = ffi_cast('xcb_input_focus_in_event_t &', event)
	if should_ignore_event(event) then
		return
	end

	-- FIXME: Use XCB_INPUT_GRAB_TYPE_FOCUS_IN.
	local user = self.devices_by_id[event.deviceid].user
	if event.event ~= user.focused_id then
		return self:send_user_focus(user)
	end
end

local function handle_input_hierarchy(self)
	self:query_inputs()
end

local function parse_property_array(data, length)
	if length ~= 1 then
		local result = {}
		for i = 0, length - 1 do
			table.insert(result, data[i])
		end
		return result
	else
		return data[0]
	end
end

local function parse_input_property_data(data, len, type, format)
	if len == 0 then
		return {}
	elseif type == 'INTEGER' and format == 8 then
		return parse_property_array(ffi_cast('int8_t *', data), len)
	elseif type == 'INTEGER' and format == 32 then
		return parse_property_array(ffi_cast('int32_t *', data), len)
	elseif type == 'CARDINAL' and format == 32 then
		return parse_property_array(ffi_cast('uint32_t *', data), len)
	elseif type == 'FLOAT' and format == 32 then
		return parse_property_array(ffi_cast('float *', data), len)
	elseif type == 'STRING' and format == 8 then
		return ffi_string(data, len)
	end
end

local function serialize_property_array(data, ctype)
	if type(data) == 'table' then
		return #data, ffi_new(ctype, #data, unpack(data))
	else
		return 1, ffi_new(ctype, 1, data)
	end
end

local function serialize_input_property_data(data, type, format)
	if data == nil then
		return 0, nil
	elseif type == 'INTEGER' and format == 8 then
		return serialize_property_array(data, 'int8_t[?]')
	elseif type == 'INTEGER' and format == 32 then
		return serialize_property_array(data, 'int32_t[?]')
	elseif type == 'CARDINAL' and format == 32 then
		return serialize_property_array(data, 'uint32_t[?]')
	elseif type == 'FLOAT' and format == 32 then
		return serialize_property_array(data, 'float[?]')
	elseif type == 'STRING' and format == 8 then
		return #data, data
	end
	assert(false)
end

function M:send_device_property(device, schema, value)
	local num_items, items =
		serialize_input_property_data(value, schema.type, schema.format)

	c.xcb_input_change_device_property(
		self.conn,
		self.atoms[schema.name],
		self.atoms[schema.type],
		device.id,
		schema.format,
		c.XCB_PROP_MODE_REPLACE,
		num_items,
		items
	):discard_check(self.conn)
end

function M:handle_input_property_reply(device, property, reply)
	local name = self.atoms[property]

	if reply == nil or reply.type == 0 then
		device.properties[name] = nil
		return
	end

	assert(reply.bytes_after == 0)

	local type = self.atoms[reply.type]
	local format = reply.format

	local property = {
		name = name,
		type = type,
		format = format,
		data = parse_input_property_data(
			reply:items(),
			reply.num_items,
			type,
			format
		),
	}

	device.properties[name] = property
end

local function handle_input_property(self, event)
	local event = ffi_cast('xcb_input_property_event_t &', event)

	local device = self.devices_by_id[event.deviceid]
	if not device then
		return
	end

	local reply = c.xcb_input_xi_get_property_unchecked(
		self.conn,
		device.id,
		false,
		event.property,
		c.XCB_ATOM_ANY,
		0,
		-1
	):unsafe_reply(self.conn)

	self:handle_input_property_reply(device, event.property, reply)
end

local function handle_input_enter(self, event)
	local event = ffi_cast('xcb_input_enter_event_t &', event)
	if should_ignore_event(event) then
		return
	end

	local user = self.devices_by_id[event.deviceid].user
	local window = self.windows_by_id[event.event]

	if window then
		self:set_user_focused_window(user, window, {
			restore_pointer = false,
		})
	end
end

local function handle_error(self, event)
	local event = ffi_cast('xcb_generic_error_t &', event)
	print(event)
end

local function handle_expose(self, event)
	local event = ffi_cast('xcb_expose_event_t &', event)
	if event.count > 0 then
		return
	end

	local label = self.labels_by_id[event.window]
	if label then
		self:handle_label_expose(label, event)
		return
	end
end

local function handle_configure_notify(self, event)
	local event = ffi_cast('xcb_configure_notify_event_t &', event)

	local tree = self.trees_by_id[event.event]
	if not tree then
		return
	end

	if not tree.has_xrandr then
		local output = {
			geom = {
				x = 0,
				y = 0,
				width = event.width,
				height = event.height,
			},
		}
		self:set_tree_outputs(tree, { output })
	end
end

local function handle_randr_screen_change(self, event)
	local event = ffi_cast('xcb_randr_screen_change_notify_event_t &', event)
	self:query_randr_monitors(event.root)
end

local function handle_xkb_new_keyboard_notify(self, event)
	local event = ffi_cast('xcb_xkb_new_keyboard_notify_event_t &', event)
	local device = self.devices_by_id[event.deviceID]

	device.keymap = nil
	device.bindings = {}

	if device.user and device.user.master_keyboard == device then
		-- FIXME: Force set set_user_keymap().
		self:clear_user_bindings(device.user)
		self:update_user_bindings(device.user)
	end
end

local function handle_map(self, event)
	local event = ffi_cast('xcb_map_request_event_t &', event)
	local tree = self.trees_by_id[event.parent]

	if tree then
		self:manage_window(event.window, tree)
	end
end

local function handle_unmap(self, event)
	local event = ffi_cast('xcb_unmap_notify_event_t &', event)

	-- Ignore reparenting related events.
	if event.event ~= event.window then
		return
	end

	self:unmanage_window(event.window)
end

local function handle_configure_request(self, event)
	local event = ffi_cast('xcb_configure_request_event_t &', event)
	local window = self.windows_by_id[event.window]

	if window then
		self:send_window_configure_notify(window)
	end
end

local function handle_property_notify(self, event)
	local event = ffi_cast('xcb_property_notify_event_t &', event)

	local handler = self.property_handlers[event.atom]
	if not handler then
		return
	end

	local window = self.windows_by_id[event.window]
	if not window then
		return
	end

	local reply
	if event.state ~= c.XCB_PROPERTY_DELETE then
		-- FIXME: Do not query property immediately but use __index magic.
		reply = c.xcb_get_property_unchecked(
			self.conn,
			false,
			event.window,
			event.atom,
			c.XCB_ATOM_ANY,
			0,
			-1
		)
			:unsafe_reply(self.conn)
	end

	handler(self, window, reply)

	if window.focused then
		for _, user in ipairs(self.users) do
			self:update_user_keymap(user)
			self:update_user_window_bindings(user, window)
		end
	end
end

local function dirty(self)
	self.should_commit = true
end

function M:dirty_window(window)
	window.should_commit = true
	dirty(self)
end

function M:dirty_window_visibility(window)
	self.did_windows_visibility_change = true
	self:dirty_window(window)
end

function M:dirty_user(user)
	user.should_commit = true
	dirty(self)
end

function M:dirty_label(label)
	label.should_commit = true
	dirty(self)
end

function M:dirty_tree(tree)
	tree.should_commit = true
	dirty(self)
end

function M:dirty_tree_layout(tree)
	tree.should_update_layout = true
	self:dirty_tree(tree)
end

function M:dirty_tree_outputs(tree)
	tree.should_update_outputs = true
	self:dirty_tree(tree)
end

function M:outputs(_config) end

function M:dirty_outputs()
	for _, tree in pairs(self.trees_by_id) do
		self:dirty_tree_outputs(tree)
	end
end

function M:tree_outputs(_tree, config)
	return self:outputs(config)
end

function M:update_tree_outputs(tree)
	tree.should_update_outputs = false

	if tree.has_xrandr then
		self:set_randr_config(tree.window_id, function(config)
			return self:tree_outputs(tree, config)
		end)
	end
end

local function commit_tree(self, tree)
	if not tree.should_commit then
		return
	end
	tree.should_commit = false

	if tree.should_update_layout then
		self:update_tree_layout(tree)
	end

	if tree.should_update_outputs then
		self:update_tree_outputs(tree)
	end
end

local function commit_label(self, label)
	if not label.should_commit then
		return
	end
	label.should_commit = false

	if label.mapped ~= label.committed_mapped then
		label.committed_mapped = label.mapped

		if label.mapped then
			c.xcb_map_window(self.conn, label.window_id):discard_check(self.conn)
		else
			c.xcb_unmap_window(self.conn, label.window_id):discard_check(self.conn)
		end
	end

	if not label.mapped then
		return
	end

	local pixmap = self:get_label_pixmap(label)

	if pixmap ~= label.committed_pixmap then
		label.committed_pixmap = pixmap

		c.xcb_change_window_attributes(
			self.conn,
			label.window_id,
			c.XCB_CW_BACK_PIXMAP,
			ffi_new('uint32_t[1]', pixmap.pixmap_id)
		):discard_check(self.conn)

		c.xcb_shape_mask(
			self.conn,
			c.XCB_SHAPE_SO_SET,
			c.XCB_SHAPE_SK_BOUNDING,
			label.window_id,
			0,
			0,
			pixmap.shape_id
		):discard_check(self.conn)

		c.xcb_clear_area(self.conn, 0, label.window_id, 0, 0, 0, 0)
			:discard_check(self.conn)
	end
end

local function save_user_window_pointer(self, user, window)
	local reply = c.xcb_input_xi_query_pointer_unchecked(
		self.conn,
		window.window_id,
		user.master_pointer.id
	)
		:unsafe_reply(self.conn)

	if
		reply ~= nil
		and (reply.win_x >= 0 and reply.win_x < lshift(window.geom.width, 16))
		and (reply.win_y >= 0 and reply.win_y < lshift(window.geom.height, 16))
	then
		window.saved_pointer_x = reply.win_x
		window.saved_pointer_y = reply.win_y
	else
		window.saved_pointer_x = nil
		window.saved_pointer_y = nil
	end
end

local function commit_user_pre(self, user)
	if not user.should_commit then
		return
	end

	local prev = user.committed_focused_window
	if prev and user.focused_window ~= prev then
		save_user_window_pointer(self, user, prev)
	end
end

local xcb_input_event_mask1_t = ffi_typeof([[
struct {
	xcb_input_event_mask_t head;
	xcb_input_xi_event_mask_t mask[1];
}
]])

local function send_window_select_input_events_factory(enter)
	local mask = ffi_new(xcb_input_event_mask1_t, {
		head = {
			deviceid = c.XCB_INPUT_DEVICE_ALL,
			mask_len = 1,
		},
		mask = {
			bor(
				c.XCB_INPUT_XI_EVENT_MASK_FOCUS_IN,
				enter and c.XCB_INPUT_XI_EVENT_MASK_ENTER or 0
			),
		},
	})

	return function(self, window)
		c.xcb_input_xi_select_events(self.conn, window.window_id, 1, mask.head)
			:discard_check(self.conn)
	end
end

local send_window_select_input_events_pre =
	send_window_select_input_events_factory(false)
local send_window_select_input_events_post =
	send_window_select_input_events_factory(true)

local function commit_window(self, window)
	if not window.should_commit then
		return
	end
	window.should_commit = false

	if
		window.mapped ~= window.committed_mapped
		or window.focused ~= window.committed_focused
	then
		window.committed_focused = window.focused

		if window.mapped ~= window.committed_mapped then
			window.committed_mapped = window.mapped

			self:send_window_icccm_wm_state(
				window.window_id,
				window.mapped and c.XCB_ICCCM_WM_STATE_NORMAL
					or c.XCB_ICCCM_WM_STATE_ICONIC
			)

			if window.mapped then
				c.xcb_map_window(self.conn, window.frame_id):discard_check(self.conn)
			else
				c.xcb_unmap_window(self.conn, window.frame_id):discard_check(self.conn)
			end
		end

		self:send_window_net_wm_state(window, not window.mapped, window.focused)

		-- FIXME: Use proper stack manipulation.
		if window.mapped and window.focused then
			c.xcb_configure_window(
				self.conn,
				window.frame_id,
				c.XCB_CONFIG_WINDOW_STACK_MODE,
				ffi_new('uint32_t[1]', c.XCB_STACK_MODE_ABOVE)
			):discard_check(self.conn)
		end
	end

	if not window.mapped then
		return
	end

	local geom = window.geom

	if
		not Rect.eq(geom, window.committed_geom)
		or window.border ~= window.committed_border
	then
		Rect.assign(window.committed_geom, geom)
		window.committed_border = window.border

		c.xcb_configure_window(
			self.conn,
			window.window_id,
			bor(
				c.XCB_CONFIG_WINDOW_X,
				c.XCB_CONFIG_WINDOW_Y,
				c.XCB_CONFIG_WINDOW_WIDTH,
				c.XCB_CONFIG_WINDOW_HEIGHT
			),
			ffi_new('uint32_t[4]', 0, 0, geom.width, geom.height)
		):discard_check(self.conn)

		c.xcb_configure_window(
			self.conn,
			window.frame_id,
			bor(
				c.XCB_CONFIG_WINDOW_X,
				c.XCB_CONFIG_WINDOW_Y,
				c.XCB_CONFIG_WINDOW_WIDTH,
				c.XCB_CONFIG_WINDOW_HEIGHT,
				c.XCB_CONFIG_WINDOW_BORDER_WIDTH
			),
			ffi_new(
				'uint32_t[5]',
				geom.x - window.border,
				geom.y - window.border,
				geom.width,
				geom.height,
				window.border
			)
		):discard_check(self.conn)

		self:send_window_configure_notify(window)
	end

	if
		window.border > 0 and window.border_color ~= window.committed_border_color
	then
		window.committed_border_color = window.border_color

		c.xcb_change_window_attributes(
			self.conn,
			window.frame_id,
			c.XCB_CW_BORDER_PIXEL,
			ffi_new('uint32_t[1]', window.border_color)
		):discard_check(self.conn)
	end
end

local function restore_user_window_pointer(self, user, window)
	local x, y = window.saved_pointer_x, window.saved_pointer_y

	if
		not x
		or not y
		or x >= lshift(window.geom.width, 16)
		or y >= lshift(window.geom.height, 16)
	then
		x = lshift(window.geom.width, 15)
		y = lshift(window.geom.height, 15)
	end

	c.xcb_input_xi_warp_pointer(
		self.conn,
		c.XCB_WINDOW_NONE,
		window.window_id,
		0,
		0,
		0,
		0,
		x,
		y,
		user.master_pointer.id
	):discard_check(self.conn)
end

local function commit_user(self, user)
	if not user.should_commit then
		return
	end
	user.should_commit = false

	if user.focused_id ~= user.committed_focused_id then
		user.committed_focused_id = user.focused_id

		self:send_user_focus(user)
	end

	local curr = user.focused_window
	if curr ~= user.committed_focused_window then
		user.committed_focused_window = curr

		if curr then
			if curr.wm_protocols.WM_TAKE_FOCUS then
				self:send_window_take_focus(curr)
			end

			if user.focus_opts.restore_pointer ~= false then
				restore_user_window_pointer(self, user, curr)
			end
		end
	end

	if user.keymap ~= user.committed_keymap then
		user.committed_keymap = user.keymap

		if user.keymap then
			self:send_device_keymap(user.master_keyboard, user.keymap)
		end
	end

	if user.dirty_bindings then
		for window_id, bindings in pairs(user.dirty_bindings) do
			commit_user_window_bindings(self, user, window_id, bindings)
		end
		user.dirty_bindings = nil
	end
end

local function commit(self)
	if not self.should_commit then
		return
	end

	-- May dirty windows.
	for _, tree in pairs(self.trees_by_id) do
		commit_tree(self, tree)
	end

	for _, label in pairs(self.labels_by_id) do
		commit_label(self, label)
	end

	-- Ignore XCB_INPUT_ENTER during reconfiguration; so it is fired only when
	-- pointer moves to a new window but not when a window moves under the
	-- cursor.
	if self.did_windows_visibility_change then
		for _, window in pairs(self.windows_by_id) do
			if window.mapped or window.committed_mapped then
				send_window_select_input_events_pre(self, window)
			end
		end
	end

	for _, user in ipairs(self.users) do
		commit_user_pre(self, user)
	end

	for _, window in pairs(self.windows_by_id) do
		commit_window(self, window)
	end

	for _, user in ipairs(self.users) do
		commit_user(self, user)
	end

	if self.did_windows_visibility_change then
		for _, window in pairs(self.windows_by_id) do
			if window.mapped then
				send_window_select_input_events_post(self, window)
			end
		end
	end

	-- Above commit*()s may call dirty*() but they must be executed in an order
	-- that ensures that everything is clean by now and a second commit() would
	-- be a no-op.
	self.should_commit = false
	self.did_windows_visibility_change = false
end

function M:start_ipc()
	local methods = {
		set_console_window = function(client, window_id)
			local window = self.windows_by_id[window_id]

			window.console = true
			self:dirty_tree_layout(window.tree)
		end,
		focus_window = function(client, user_id, window_id)
			local user = self.devices_by_id[user_id].user
			local window = self.windows_by_id[window_id]

			self:set_user_focused_window(user, window)
		end,
		focus_console = function(client, user_id)
			local user = self.devices_by_id[user_id].user

			self:set_user_focused_window(user, self:get_console())
		end,
		list_users = function(client, tree_id)
			local result = {}
			for _, user in ipairs(self.users) do
				table.insert(result, {
					id = user.master_pointer.id,
					name = user.name,
				})
			end
			return result
		end,
		list_trees = function(client, tree_id)
			local result = {}
			for _, tree in pairs(self.trees_by_id) do
				table.insert(result, {
					id = tree.window_id,
				})
			end
			return result
		end,
		list_windows = function(client, tree_id)
			local result = {}
			for _, window in ipairs(self.trees_by_id[tree_id].window_stack) do
				if not window.console then
					table.insert(result, {
						id = window.window_id,
						title = window.title,
						class = window.wm_class.class,
						instance = window.wm_class.instance,
						focused = window.focused,
					})
				end
			end
			return result
		end,
	}

	local path = string.format('/tmp/heawm-%s.sock', os.getenv('DISPLAY'))

	local server = uv.new_pipe(false)

	os.remove(path)
	server:bind(path)

	for _, tree in pairs(self.trees_by_id) do
		c.xcb_change_property(
			self.conn,
			c.XCB_PROP_MODE_REPLACE,
			tree.window_id,
			self.atoms._HEAWM_SOCKET,
			self.atoms.STRING,
			8,
			#path,
			path
		):discard_check(self.conn)
	end

	local JsonRpc = require('heawm.json-rpc')
	local clients = {}

	local function on_client_close(client)
		clients[client] = nil
	end

	server:listen(4, function()
		local client = uv.new_pipe(false)
		server:accept(client)

		local rpc_client = JsonRpc:new(client)
		clients[rpc_client] = true
		rpc_client:start(methods, on_client_close)
	end)

	function self:broadcast(method, ...)
		local params = { ... }
		for client in pairs(clients) do
			client:send_request(method, params)
		end
	end
end

function M:start()
	local conn = self.conn

	local xinput_handlers = {
		[c.XCB_INPUT_KEY_PRESS] = handle_input_key_press,
		[c.XCB_INPUT_KEY_RELEASE] = handle_input_key_release,
		[c.XCB_INPUT_BUTTON_PRESS] = handle_input_button_press,
		[c.XCB_INPUT_BUTTON_RELEASE] = handle_input_button_release,
		[c.XCB_INPUT_FOCUS_IN] = handle_input_focus_in,
		[c.XCB_INPUT_ENTER] = handle_input_enter,
		[c.XCB_INPUT_HIERARCHY] = handle_input_hierarchy,
		[c.XCB_INPUT_PROPERTY] = handle_input_property,
	}

	local generic_event_handlers = {
		[self.xi_ext.major_opcode] = xinput_handlers,
	}

	local function handle_generic_event(self, event)
		local event = ffi_cast('xcb_ge_generic_event_t &', event)
		local handlers = generic_event_handlers[event.extension]

		if handlers then
			local handler = handlers[event.event_type]
			if handler then
				handler(self, event)
				return
			end
		end

		print('Unhandled generic event', event.extension, event.event_type)
	end

	local handlers = {
		[0] = handle_error,
		[c.XCB_EXPOSE] = handle_expose,
		[c.XCB_CONFIGURE_NOTIFY] = handle_configure_notify,
		[c.XCB_CONFIGURE_REQUEST] = handle_configure_request,
		[c.XCB_MAP_REQUEST] = handle_map,
		[c.XCB_UNMAP_NOTIFY] = handle_unmap,
		[c.XCB_PROPERTY_NOTIFY] = handle_property_notify,
		[c.XCB_GE_GENERIC] = handle_generic_event,
		[self.randr_ext and self.randr_ext.first_event + c.XCB_RANDR_SCREEN_CHANGE_NOTIFY or false] = handle_randr_screen_change,
		[self.xkb_ext.first_event + c.XCB_XKB_NEW_KEYBOARD_NOTIFY] = handle_xkb_new_keyboard_notify,
		[false] = nil,
	}

	local function handle_event(self, event)
		local event_type = band(event.response_type, c.XCB_EVENT_RESPONSE_TYPE_MASK)
		local handler = handlers[event_type]
		if handler then
			handler(self, event)
			return
		end
		-- print('Unhandled event', event)
	end

	self.uv_poll = uv.new_poll(conn:file_descriptor())
	self.uv_poll:start('r', function(err, events)
		assert(not err, err)
		for event in conn.poll_for_event, conn do
			handle_event(self, event)
		end
	end)

	self.uv_prepare = uv.new_prepare()
	self.uv_prepare:start(function(err)
		assert(not err, err)

		repeat
			local should_flush = false

			commit(self)

			if c.xcb_flush(conn) <= 0 then
				self:stop()
				return
			end

			for event in conn.poll_for_queued_event, conn do
				should_flush = true
				handle_event(self, event)
			end
		until not should_flush

		-- print(
		-- 	'xcb_total_written',
		-- 	tonumber(c.xcb_total_written(conn)) .. ' bytes'
		-- )
	end)

	do
		local signal = uv.new_signal()
		uv.unref(signal)
		uv.signal_start(signal, 'sigpipe', function() end)
	end
end

function M:stop()
	self.uv_poll:stop()
	self.uv_prepare:stop()
end

local function instantiate_config(cls)
	return setmetatable({}, {
		__index = cls.config,
		__newindex = function(self, k, v)
			if rawget(cls.config, k) == nil then
				error(string.format("invalid key '%s'", k))
			end
			rawset(self, k, v)
		end,
	})
end

function M.new(cls, opts)
	opts = opts or {}

	local conn = c.Connection.new(opts.display)

	local err = conn:has_error()
	if err ~= 0 then
		local msg = ({
			[c.XCB_CONN_ERROR] = 'stream error',
			[c.XCB_CONN_CLOSED_MEM_INSUFFICIENT] = 'cannot allocate memory',
			[c.XCB_CONN_CLOSED_PARSE_ERR] = 'malformed display string',
			[c.XCB_CONN_CLOSED_INVALID_SCREEN] = 'invalid screen',
		})[err] or 'unknown error'
		error(msg)
	end

	local self = setmetatable({
		conn = conn,
		atoms = require('heawm.atom-registry').new(conn),
		windows_by_id = {},
		labels_by_id = {},
		trees_by_id = {},
		devices_by_id = {},
		devices_by_name = {},
		users = {},
		users_by_name = {},
		config = instantiate_config(cls),
	}, cls)

	self:setup_extensions()
	self:setup_input_window()
	self:setup_property_handlers()

	return self
end

function M:setup_extensions()
	c.xcb_prefetch_extension_data(self.conn, c.xcb_shape_id)
	c.xcb_prefetch_extension_data(self.conn, c.xcb_randr_id)
	c.xcb_prefetch_extension_data(self.conn, c.xcb_xfixes_id)
	c.xcb_prefetch_extension_data(self.conn, c.xcb_input_id)
	c.xcb_prefetch_extension_data(self.conn, c.xcb_xkb_id)

	self.shape_ext = c.xcb_get_extension_data(self.conn, c.xcb_shape_id)
	assert(self.shape_ext.present ~= 0, 'Shape extension missing')

	self.randr_ext = c.xcb_get_extension_data(self.conn, c.xcb_randr_id)
	if self.randr_ext.present == 0 then
		self.randr_ext = nil
	end

	self.xfixes_ext = c.xcb_get_extension_data(self.conn, c.xcb_xfixes_id)
	assert(self.xfixes_ext.present ~= 0, 'XFixes extension missing')

	c.xcb_xfixes_query_version(
		self.conn,
		c.XCB_XFIXES_MAJOR_VERSION,
		c.XCB_XFIXES_MINOR_VERSION
	)
		:discard_reply(self.conn)

	self.xi_ext = c.xcb_get_extension_data(self.conn, c.xcb_input_id)
	assert(self.xi_ext.present ~= 0, 'XInput extension missing')

	self.xkb_context = c.xkb_context_new(c.XKB_CONTEXT_NO_FLAGS)
	assert(self.xkb_context ~= nil)

	self.xkb_ext = c.xcb_get_extension_data(self.conn, c.xcb_xkb_id)
	assert(self.xkb_ext.present ~= 0, 'XKB extension missing')

	c.xcb_xkb_use_extension(
		self.conn,
		c.XCB_XKB_MAJOR_VERSION,
		c.XCB_XKB_MINOR_VERSION
	)
		:assert_reply(self.conn)
end

function M:setup_input_window()
	self.input_window_id = self:create_input_window()
end

function M:create_input_window()
	local window_id = self.conn:generate_id()

	local any_root
	for screen in self.conn:setup():roots() do
		any_root = screen.root
		break
	end

	c.xcb_create_window_checked(
		self.conn,
		c.XCB_COPY_FROM_PARENT,
		window_id,
		any_root,
		0,
		0,
		1,
		1,
		0,
		c.XCB_WINDOW_CLASS_INPUT_ONLY,
		c.XCB_COPY_FROM_PARENT,
		c.XCB_CW_OVERRIDE_REDIRECT,
		ffi_new('uint32_t[1]', true)
	):discard_check(self.conn)

	c.xcb_map_window(self.conn, window_id):discard_check(self.conn)

	return window_id
end

function M:create_dummy_pixmap(drawable)
	local pixmap_id = self.conn:generate_id()

	c.xcb_create_pixmap(self.conn, 1, pixmap_id, drawable, 1, 1)
		:discard_check(self.conn)

	return pixmap_id
end

function M:setup_property_handlers()
	self.property_handlers = self.atoms({
		_NET_WM_NAME = function(self, window, reply)
			window.title = self:parse_reply_string(reply, 'UTF8_STRING') or ''
		end,
		_NET_WM_PID = function(self, window, reply)
			window.net_wm_pid = self:parse_reply_cardinal(reply)
		end,
		_NET_WM_WINDOW_TYPE = function(self, window, reply)
			window.net_wm_window_type = self:parse_reply_atoms(reply)
				or {
					_NET_WM_WINDOW_TYPE_NORMAL = true,
				}
		end,
		WM_CLASS = function(self, window, reply)
			window.wm_class = self:parse_reply_wm_class(reply) or {}
		end,
		WM_CLIENT_LEADER = function(self, window, reply)
			window.wm_client_leader = self:parse_reply_window(reply)
		end,
		WM_CLIENT_MACHINE = function(self, window, reply)
			window.wm_client_machine = self:parse_reply_string(reply, 'STRING')
		end,
		WM_HINTS = function(self, window, reply)
			window.wm_hints = self:parse_reply_wm_hints(reply) or {}
			self:set_window_bell(window, window.wm_hints.urgency)
		end,
		WM_NORMAL_HINTS = function(self, window)
			-- Notify client that hints were ignored and sizes remained the same.
			self:send_window_configure_notify(window)
		end,
		WM_PROTOCOLS = function(self, window, reply)
			window.wm_protocols = self:parse_reply_atoms(reply) or {}
		end,
		WM_TRANSIENT_FOR = function(self, window, reply)
			window.wm_transient_for = self:parse_reply_window(reply)
		end,
		WM_WINDOW_ROLE = function(self, window, reply)
			window.wm_window_role = self:parse_reply_string(reply, 'STRING')
		end,
	})

	setmetatable(self.property_handlers, {
		__newindex = function(handlers, name, callback)
			rawset(handlers, self.atoms[name], callback)
		end,
	})
end

function M:manage_inputs(root_id)
	local cookie = c.xcb_input_xi_select_events_checked(
		self.conn,
		root_id,
		1,
		ffi_new(xcb_input_event_mask1_t, {
			head = {
				deviceid = c.XCB_INPUT_DEVICE_ALL,
				mask_len = 1,
			},
			mask = {
				bor(
					c.XCB_INPUT_XI_EVENT_MASK_HIERARCHY,
					c.XCB_INPUT_XI_EVENT_MASK_FOCUS_IN,
					c.XCB_INPUT_XI_EVENT_MASK_ENTER,
					c.XCB_INPUT_XI_EVENT_MASK_PROPERTY
				),
			},
		}).head
	)

	self:query_inputs()

	cookie:assert_check(self.conn)
end

function M:setup_keyboard(device)
	local required_events = bor(
		c.XCB_XKB_EVENT_TYPE_NEW_KEYBOARD_NOTIFY,
		c.XCB_XKB_EVENT_TYPE_MAP_NOTIFY
	)

	local required_map_parts = bor(
		c.XCB_XKB_MAP_PART_KEY_TYPES,
		c.XCB_XKB_MAP_PART_KEY_SYMS,
		c.XCB_XKB_MAP_PART_MODIFIER_MAP,
		c.XCB_XKB_MAP_PART_EXPLICIT_COMPONENTS,
		c.XCB_XKB_MAP_PART_KEY_ACTIONS,
		c.XCB_XKB_MAP_PART_KEY_BEHAVIORS,
		c.XCB_XKB_MAP_PART_VIRTUAL_MODS,
		c.XCB_XKB_MAP_PART_VIRTUAL_MOD_MAP
	)

	c.xcb_xkb_select_events(
		self.conn,
		device.id,
		required_events,
		0,
		required_events,
		required_map_parts,
		required_map_parts,
		nil
	):discard_check(self.conn)

	local client_flags = c.XCB_XKB_PER_CLIENT_FLAG_DETECTABLE_AUTO_REPEAT

	c.xcb_xkb_per_client_flags_unchecked(
		self.conn,
		device.id,
		client_flags,
		client_flags,
		0,
		0,
		0
	)
		:discard_reply(self.conn)
end

function M:send_device_properties(device, props)
	for name, data in pairs(props) do
		local prop = device.properties[name]
		if prop then
			self:send_device_property(device, prop, data)
		end
	end
end

function M:send_device_keymap(device, keymap)
	c.xcb_xkb_get_kbd_by_name_ext_unchecked(
		self.conn,
		device.id,
		keymap,
		0,
		0,
		true
	)
		:discard_reply(self.conn)
end

function M:setup_inputs() end

function M:query_inputs()
	local TYPE_NAME = {
		[c.XCB_INPUT_DEVICE_TYPE_MASTER_POINTER] = 'pointer',
		[c.XCB_INPUT_DEVICE_TYPE_MASTER_KEYBOARD] = 'keyboard',
		[c.XCB_INPUT_DEVICE_TYPE_SLAVE_POINTER] = 'pointer',
		[c.XCB_INPUT_DEVICE_TYPE_SLAVE_KEYBOARD] = 'keyboard',
		[c.XCB_INPUT_DEVICE_TYPE_FLOATING_SLAVE] = 'unknown',
	}

	local users = {}
	local devices_by_id = {}
	local devices_by_name = {}
	local users_by_md = {}
	local users_by_name = {}

	local reply = c.xcb_input_xi_query_device(self.conn, c.XCB_INPUT_DEVICE_ALL)
		:assert_reply(self.conn)

	local property_list_queue = {}

	for info in reply:infos() do
		local slave = (
			info.type == c.XCB_INPUT_DEVICE_TYPE_SLAVE_POINTER
			or info.type == c.XCB_INPUT_DEVICE_TYPE_SLAVE_KEYBOARD
		)

		local device = {
			id = info.deviceid,
			name = info:name(),
			type = TYPE_NAME[info.type],
			master = slave and devices_by_id[info.attachment] or nil,
			bindings = {},
			properties = {},
		}
		devices_by_id[device.id] = device
		devices_by_name[device.name] = device

		local cookie =
			c.xcb_input_xi_list_properties_unchecked(self.conn, device.id)
		table.insert(property_list_queue, { device, cookie })
	end

	local property_cookie_queue = {}

	for _, x in ipairs(property_list_queue) do
		local device, cookie = unpack(x)
		local reply = cookie:unsafe_reply(self.conn)

		if reply ~= nil then
			for _, atom in reply:atoms() do
				local cookie = c.xcb_input_xi_get_property_unchecked(
					self.conn,
					device.id,
					false,
					atom,
					c.XCB_ATOM_ANY,
					0,
					-1
				)
				table.insert(property_cookie_queue, { device, atom, cookie })
			end
		end
	end

	for _, x in ipairs(property_cookie_queue) do
		local device, atom, cookie = unpack(x)
		local reply = cookie:unsafe_reply(self.conn)

		self:handle_input_property_reply(device, atom, reply)
	end

	for info in reply:infos() do
		if info.type == c.XCB_INPUT_DEVICE_TYPE_MASTER_POINTER then
			local keyboard = devices_by_id[info.attachment]
			local pointer = devices_by_id[info.deviceid]
			local name = string.sub(pointer.name, 1, -9)

			local user = {
				master_keyboard = keyboard,
				master_pointer = pointer,
				devices = {},
				name = name,
				focused_id = self.input_window_id,
				bindings = {},
			}
			table.insert(users, user)

			users_by_md[keyboard.id] = user
			users_by_md[pointer.id] = user
			users_by_name[name] = user
		end

		local device = devices_by_id[info.deviceid]

		local user = users_by_md[info.attachment]
		-- Not slave.
		if user then
			user.devices[device.id] = device
			device.user = user
		end

		if info.type == c.XCB_INPUT_DEVICE_TYPE_MASTER_KEYBOARD then
			self:setup_keyboard(device)
		end
	end

	for _, old_user in ipairs(self.users) do
		local new_user = users_by_md[old_user.master_pointer.id]
		if new_user then
			new_user.saved_focused_window = old_user.focused_window
			new_user.bindings = old_user.bindings
			new_user.dirty_bindings = old_user.dirty_bindings
		end

		self:set_user_focused_window(old_user, nil)
	end

	self.devices_by_id = devices_by_id
	self.devices_by_name = devices_by_name
	self.users = users
	self.users_by_name = users_by_name

	for _, user in ipairs(self.users) do
		self:dirty_user(user)

		self:set_user_focused_window(user, user.saved_focused_window)
		user.saved_focused_window = nil

		self:update_user_keymap(user)
		self:clear_user_bindings(user)
		self:update_user_bindings(user)
	end

	for _, device in pairs(self.devices_by_id) do
		self:update_device_properties(device)
	end

	self:setup_inputs()
end

function M:device_properties(_device)
	return {}
end

function M:update_device_properties(device)
	self:send_device_properties(device, self:device_properties(device))
end

function M:clear_user_bindings(user)
	if not user.dirty_bindings then
		user.dirty_bindings = {}
	end
	for window_id in pairs(user.bindings) do
		user.dirty_bindings[window_id] = {}
	end
	user.bindings = {}
	self:dirty_user(user)
end

function M:user_global_bindings(_user, _tree) end

function M:user_window_bindings(_user, _window) end

function M:update_user_global_bindings(user)
	for tree_id, tree in pairs(self.trees_by_id) do
		self:set_user_bindings(user, tree_id, self:user_global_bindings(user, tree))
	end
end

function M:update_user_window_bindings(user, window)
	self:set_user_bindings(
		user,
		window.window_id,
		self:user_window_bindings(user, window)
	)
end

function M:update_user_windows_bindings(user)
	for _, window in pairs(self.windows_by_id) do
		self:update_user_window_bindings(user, window)
	end
end

function M:update_user_bindings(user)
	self:update_user_global_bindings(user)
	self:update_user_windows_bindings(user)
end

function M:update_window_bindings(window)
	for _, user in ipairs(self.users) do
		self:update_user_window_bindings(user, window)
	end
end

function M:user_keymap(_user, _window)
	return nil
end

function M:update_user_keymap(user)
	self:set_user_keymap(user, self:user_keymap(user, user.focused_window))
end

function M:unmanage_window(window_id)
	local window = self.windows_by_id[window_id]
	if not window then
		return
	end

	self:destroy_window(window)
end

function M:destroy_window(window)
	self.windows_by_id[window.window_id] = nil

	if window.window_id then
		self:send_window_icccm_wm_state(
			window.window_id,
			c.XCB_ICCCM_WM_STATE_WITHDRAWN
		)

		c.xcb_change_window_attributes(
			self.conn,
			window.window_id,
			c.XCB_CW_EVENT_MASK,
			ffi_new('uint32_t[1]', 0)
		):discard_check(self.conn)

		c.xcb_shape_select_input(self.conn, window.window_id, false)
			:discard_check(self.conn)

		c.xcb_input_xi_select_events(self.conn, window.window_id, 0, nil)
			:discard_check(self.conn)

		c.xcb_unmap_window(self.conn, window.window_id):discard_check(self.conn)

		c.xcb_reparent_window(
			self.conn,
			window.window_id,
			window.tree.screen.root,
			0,
			0
		)
			:discard_check(self.conn)

		c.xcb_change_save_set(self.conn, c.XCB_SET_MODE_DELETE, window.window_id)
			:discard_check(self.conn)
	end

	if window.frame_id then
		c.xcb_destroy_window(self.conn, window.frame_id):discard_check(self.conn)
	end

	if window.label then
		self:destroy_label(window.label)
	end

	for _, user in ipairs(self.users) do
		if user.focused_window == window then
			self:set_user_focused_window(user, window.opener_window)
		end

		if user.prev_focused_window == window then
			user.prev_focused_window = nil
		end

		if user.committed_focused_window == window then
			user.committed_focused_window = nil
		end

		user.bindings[window.window_id] = nil
		if user.dirty_bindings then
			user.dirty_bindings[window.window_id] = nil
		end
	end

	for _, other in pairs(self.windows_by_id) do
		if other.opener_window == window then
			other.opener_window = nil
		end
	end

	for i, x in ipairs(window.tree.window_stack) do
		if x == window then
			table.remove(window.tree.window_stack, i)
			break
		end
	end
	self:dirty_tree_layout(window.tree)

	for _, device in pairs(self.devices_by_id) do
		device.bindings[window.window_id] = nil
	end
end

function M:manage_window(window_id, tree)
	if self.windows_by_id[window_id] then
		return
	end

	local window = {
		tree = tree,
		geom = {},
		committed_geom = {},
		window_id = window_id,
		frame_id = self.conn:generate_id(),
		geom = {
			x = -1,
			y = -1,
			width = 1,
			height = 1,
		},
		border = 0,
	}

	local geom_cookie = c.xcb_get_geometry_unchecked(self.conn, window.window_id)

	local property_cookies = {}

	for atom in pairs(self.property_handlers) do
		property_cookies[atom] = c.xcb_get_property_unchecked(
			self.conn,
			false,
			window_id,
			atom,
			c.XCB_ATOM_ANY,
			0,
			-1
		)
	end

	c.xcb_create_window(
		self.conn,
		c.XCB_COPY_FROM_PARENT,
		window.frame_id,
		tree.window_id,
		window.geom.x,
		window.geom.y,
		window.geom.width,
		window.geom.height,
		window.border,
		c.XCB_WINDOW_CLASS_INPUT_OUTPUT,
		c.XCB_COPY_FROM_PARENT,
		bor(c.XCB_CW_OVERRIDE_REDIRECT, c.XCB_CW_EVENT_MASK),
		ffi_new('uint32_t[2]', true, c.XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT)
	):discard_check(self.conn)

	-- FIXME: Use proper stack manipulation.
	c.xcb_configure_window(
		self.conn,
		window.frame_id,
		c.XCB_CONFIG_WINDOW_STACK_MODE,
		ffi_new('uint32_t[1]', c.XCB_STACK_MODE_BELOW)
	):discard_check(self.conn)

	c.xcb_change_property(
		self.conn,
		c.XCB_PROP_MODE_REPLACE,
		window.window_id,
		self.atoms._NET_FRAME_EXTENTS,
		self.atoms.CARDINAL,
		32,
		4,
		ffi_new('uint32_t[4]')
	):discard_check(self.conn)

	c.xcb_change_save_set(self.conn, c.XCB_SET_MODE_INSERT, window.window_id)
		:discard_check(self.conn)

	local reparent_cookie = (
		c.xcb_reparent_window_checked(
			self.conn,
			window.window_id,
			window.frame_id,
			0,
			0
		)
	)

	c.xcb_map_window(self.conn, window.window_id):discard_check(self.conn)

	c.xcb_change_window_attributes(
		self.conn,
		window.window_id,
		c.XCB_CW_EVENT_MASK,
		ffi_new(
			'uint32_t[1]',
			bor(c.XCB_EVENT_MASK_PROPERTY_CHANGE, c.XCB_EVENT_MASK_STRUCTURE_NOTIFY)
		)
	):discard_check(self.conn)

	c.xcb_shape_select_input(self.conn, window.window_id, true)
		:discard_check(self.conn)

	local geom = geom_cookie:unsafe_reply(self.conn)
	if geom ~= nil then
		window.float_geom = {
			x = geom.x,
			y = geom.y,
			width = geom.width,
			height = geom.height,
		}
	end

	for atom, cookie in pairs(property_cookies) do
		local handler = self.property_handlers[atom]
		local reply = cookie:unsafe_reply(self.conn)
		handler(self, window, reply)
	end

	if reparent_cookie:unsafe_check(self.conn) ~= nil then
		self:destroy_window(window)
		return
	end

	self.windows_by_id[window_id] = window

	window.label = self:create_label(window.frame_id, tree)

	self:setup_window_layout(window)
	table.insert(window.tree.window_stack, window)
	self:dirty_tree_layout(tree)

	self:update_window_bindings(window)

	for _, user in ipairs(self.users) do
		if self:should_user_focus_window(user, window) then
			self:set_user_focused_window(user, window)
		end
	end
end

function M:swap_user_windows(user, opts)
	local x = user.focused_window
	local y = user.prev_focused_window

	if not x or not y then
		return
	end

	local tree = x.tree
	if y.tree ~= tree then
		return
	end

	for i, window in ipairs(tree.window_stack) do
		if window == x then
			tree.window_stack[i] = y
		elseif window == y then
			tree.window_stack[i] = x
		end
	end

	x.layout, y.layout = y.layout, x.layout

	self:set_user_focused_window(user, y, opts)
end

function M:should_user_focus_window(user, window)
	if
		not window.net_wm_window_type._NET_WM_WINDOW_TYPE_NORMAL
		and not window.net_wm_window_type._NET_WM_WINDOW_TYPE_DIALOG
	then
		return false
	end

	local curr = user.focused_window

	if not curr then
		return true
	end

	if window.wm_transient_for == curr.window_id then
		return true
	end

	if
		window.wm_client_leader
		and window.wm_client_leader == curr.wm_client_leader
	then
		return true
	end

	return false
end

function M:destroy_label(label)
	if label.window_id then
		c.xcb_destroy_window(self.conn, label.window_id):discard_check(self.conn)

		self.labels_by_id[label.window_id] = nil
	end
end

function M:create_label(parent_id, tree)
	local label = {
		tree = tree,
	}

	if not self:setup_label(label, parent_id) then
		self:destroy_label(label)
		return
	end

	self.labels_by_id[label.window_id] = label

	return label
end

function M:setup_label(label, parent_id)
	label.window_id = self.conn:generate_id()

	c.xcb_create_window(
		self.conn,
		c.XCB_COPY_FROM_PARENT,
		label.window_id,
		parent_id,
		-100 + 1 + 20,
		0 - 20,
		100,
		100,
		0,
		c.XCB_WINDOW_CLASS_INPUT_OUTPUT,
		c.XCB_COPY_FROM_PARENT,
		bor(
			c.XCB_CW_BIT_GRAVITY,
			c.XCB_CW_WIN_GRAVITY,
			c.XCB_CW_OVERRIDE_REDIRECT,
			c.XCB_CW_SAVE_UNDER,
			c.XCB_CW_EVENT_MASK
		),
		ffi_new(
			'uint32_t[5]',
			c.XCB_GRAVITY_STATIC,
			c.XCB_GRAVITY_NORTH_EAST,
			true,
			true,
			c.XCB_EVENT_MASK_EXPOSURE
		)
	):discard_check(self.conn)

	c.xcb_shape_mask(
		self.conn,
		c.XCB_SHAPE_SO_SET,
		c.XCB_SHAPE_SK_INPUT,
		label.window_id,
		-1,
		-1,
		label.tree.dummy_pixmap
	):discard_check(self.conn)

	return label
end

function M:manage_screens()
	for screen in self.conn:setup():roots() do
		self:manage_screen(screen)
	end
end

function M:manage_screen(screen)
	local cursors = require('heawm.cursor-registry').new(self.conn, screen)
	local root_id = screen.root

	c.xcb_change_window_attributes(
		self.conn,
		root_id,
		c.XCB_CW_CURSOR,
		ffi_new('uint32_t[1]', cursors.default)
	)
		:discard_check(self.conn)

	self:manage_tree(root_id)
	self:manage_inputs(root_id)
	self:manage_randr_monitors(root_id)
end

function M:send_net_wm_name(window_id, name)
	c.xcb_change_property(
		self.conn,
		c.XCB_PROP_MODE_REPLACE,
		window_id,
		self.atoms._NET_WM_NAME,
		self.atoms.UTF8_STRING,
		8,
		#name,
		name
	):discard_check(self.conn)
end

function M:send_net_supported(window_id, atoms)
	c.xcb_change_property(
		self.conn,
		c.XCB_PROP_MODE_REPLACE,
		window_id,
		self.atoms._NET_SUPPORTED,
		self.atoms.ATOM,
		32,
		#atoms,
		ffi_new('xcb_atom_t[?]', #atoms, unpack(atoms))
	):discard_check(self.conn)
end

function M:send_net_supporting_wm_check(window_id, target_id)
	c.xcb_change_property(
		self.conn,
		c.XCB_PROP_MODE_REPLACE,
		window_id,
		self.atoms._NET_SUPPORTING_WM_CHECK,
		self.atoms.WINDOW,
		32,
		1,
		ffi_new('xcb_window_t[1]', target_id)
	):discard_check(self.conn)
end

function M:setup_tree_ewmh(tree)
	local parent_id = tree.window_id
	local window_id = self.conn:generate_id()

	local create_cookie = c.xcb_create_window_checked(
		self.conn,
		c.XCB_COPY_FROM_PARENT,
		window_id,
		parent_id,
		0,
		0,
		1,
		1,
		0,
		c.XCB_WINDOW_CLASS_INPUT_ONLY,
		c.XCB_COPY_FROM_PARENT,
		0,
		nil
	)

	self:send_net_wm_name(window_id, 'LG3D')

	self:send_net_supported(
		parent_id,
		self.atoms({
			'_NET_SUPPORTED',
			'_NET_WM_NAME',
			'_NET_WM_STATE',
			'_NET_WM_STATE_FOCUSED',
			'_NET_WM_STATE_HIDDEN',
			'_NET_FRAME_EXTENTS',
			'_NET_SUPPORTING_WM_CHECK',
		})
	)

	self:send_net_supporting_wm_check(window_id, window_id)
	self:send_net_supporting_wm_check(parent_id, window_id)

	create_cookie:assert_check(self.conn)

	tree.ewmh_window_id = window_id
end

local function find_screen_visual(screen)
	for depth in screen:allowed_depths() do
		for visual in depth:visuals() do
			if screen.root_visual == visual.visual_id then
				return visual
			end
		end
	end
end

function M:find_screen_by_root(root_id)
	for screen in self.conn:setup():roots() do
		if screen.root == root_id then
			return screen
		end
	end
end

function M:manage_tree(window_id)
	local cookie = c.xcb_change_window_attributes_checked(
		self.conn,
		window_id,
		c.XCB_CW_EVENT_MASK,
		ffi_new(
			'uint32_t[1]',
			bor(
				c.XCB_EVENT_MASK_STRUCTURE_NOTIFY,
				c.XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
			)
		)
	)
	local children_cookie = c.xcb_query_tree(self.conn, window_id)
	local geom_cookie = c.xcb_get_geometry_unchecked(self.conn, window_id)

	local children = children_cookie:assert_reply(self.conn)
	local geom = geom_cookie:unsafe_reply(self.conn)
	cookie:assert_check(self.conn)

	local root_id = children.root
	local screen = assert(self:find_screen_by_root(root_id))

	self:send_window_back_pixel(window_id, screen.black_pixel)

	local tree = {
		window_id = window_id,
		screen = screen,
		visual = assert(find_screen_visual(screen)),
		outputs = {},
		pixmap_cache = {},
		dummy_pixmap = self:create_dummy_pixmap(window_id),
		window_stack = {},
	}

	self:setup_tree_ewmh(tree)

	self.trees_by_id[window_id] = tree
	self:setup_tree(tree)

	if geom ~= nil then
		local output = {
			geom = {
				x = 0,
				y = 0,
				width = geom.width,
				height = geom.height,
			},
		}

		self:set_tree_outputs(tree, { output })
	end
	self:dirty_tree_outputs(tree)

	local cookie_queue = {}

	for _, child_id in children:ichildren() do
		local cookie = c.xcb_get_window_attributes_unchecked(self.conn, child_id)
		table.insert(cookie_queue, { child_id, cookie })
	end

	for _, x in ipairs(cookie_queue) do
		local child_id, cookie = unpack(x)
		local reply = cookie:unsafe_reply(self.conn)

		if
			reply ~= nil
			and reply.override_redirect == 0
			and reply.map_state == c.XCB_MAP_STATE_VIEWABLE
		then
			self:manage_window(child_id, tree)
		end
	end
end

function M:send_window_back_pixel(window_id, pixel)
	c.xcb_change_window_attributes(
		self.conn,
		window_id,
		c.XCB_CW_BACK_PIXEL,
		ffi_new('uint32_t[1]', pixel)
	)
		:discard_check(self.conn)

	c.xcb_clear_area(self.conn, 0, window_id, 0, 0, 0, 0):discard_check(self.conn)
end

local function new_send_event(ctype, init)
	return ffi_new(
		ffi_typeof(
			string.format('union { %s data; char as_send_event[32]; }', ctype)
		),
		{ data = init }
	)
end

do
	local event

	function M:send_window_take_focus(window)
		if not event then
			event = new_send_event('xcb_client_message_event_t', {
				response_type = c.XCB_CLIENT_MESSAGE,
				type = self.atoms.WM_PROTOCOLS,
				format = 32,
				data = {
					data32 = {
						self.atoms.WM_TAKE_FOCUS,
						c.XCB_CURRENT_TIME,
					},
				},
			})
		end

		event.data.window = window.window_id

		c.xcb_send_event(
			self.conn,
			false,
			window.window_id,
			c.XCB_EVENT_MASK_NO_EVENT,
			event.as_send_event
		):discard_check(self.conn)
	end
end

do
	local event

	function M:send_window_delete(window)
		if not event then
			event = new_send_event('xcb_client_message_event_t', {
				response_type = c.XCB_CLIENT_MESSAGE,
				type = self.atoms.WM_PROTOCOLS,
				format = 32,
				data = {
					data32 = {
						self.atoms.WM_DELETE_WINDOW,
					},
				},
			})
		end

		event.data.window = window.window_id

		c.xcb_send_event(
			self.conn,
			false,
			window.window_id,
			c.XCB_EVENT_MASK_NO_EVENT,
			event.as_send_event
		):discard_check(self.conn)
	end
end

do
	local event

	function M:send_window_configure_notify(window)
		if not event then
			event = new_send_event('xcb_configure_notify_event_t', {
				response_type = c.XCB_CONFIGURE_NOTIFY,
				above_sibling = c.XCB_WINDOW_NONE,
				override_redirect = false,
			})
		end

		local geom = window.geom
		event.data.event = window.window_id
		event.data.window = window.window_id
		event.data.x = geom.x
		event.data.y = geom.y
		event.data.width = geom.width
		event.data.height = geom.height
		event.data.border_width = window.border

		c.xcb_send_event(
			self.conn,
			false,
			window.window_id,
			c.XCB_EVENT_MASK_STRUCTURE_NOTIFY,
			event.as_send_event
		):discard_check(self.conn)
	end
end

function M:set_label_text(label, text)
	if label.text == text then
		return
	end

	label.text = text

	self:dirty_label(label)
end

function M:set_label_text_color(label, color)
	if label.text_color == color then
		return
	end

	label.text_color = color

	self:dirty_label(label)
end

function M:set_label_background_color(label, color)
	if label.background_color == color then
		return
	end

	label.background_color = color

	self:dirty_label(label)
end

function M:set_label_mapped(label, mapped)
	if label.mapped == mapped then
		return
	end

	label.mapped = mapped

	self:dirty_label(label)
end

function M:set_window_mapped(window, mapped)
	if window.mapped == mapped then
		return
	end

	window.mapped = mapped

	self:dirty_window_visibility(window)
end

function M:set_window_geom(window, geom)
	if Rect.eq(window.geom, geom) then
		return
	end

	Rect.assign(window.geom, geom)

	self:dirty_window_visibility(window)
end

function M:set_window_border(window, border, border_color)
	if window.border == border and window.border_color == border_color then
		return
	end

	window.border = border
	window.border_color = border_color

	self:dirty_window(window)
end

function M:send_window_kill(window)
	c.xcb_kill_client(self.conn, window.window_id):discard_check(self.conn)
end

function M:close_window(window)
	if window.wm_protocols.WM_DELETE_WINDOW then
		self:send_window_delete(window)
	else
		self:send_window_kill(window)
	end
end

function M:parse_reply_type(reply, type)
	if reply ~= nil and reply.type == self.atoms[type] then
		return c.xcb_get_property_value(reply),
			c.xcb_get_property_value_length(reply)
	end
end

function M:parse_reply_struct(reply, type, ctype)
	local data, size = self:parse_reply_type(reply, type)
	if size == ffi_sizeof(ctype) then
		return ffi_cast(ctype .. '*', data)
	end
end

function M:parse_reply_string(reply, type)
	local data, size = self:parse_reply_type(reply, type)
	if data then
		return ffi_string(data, size)
	end
end

function M:parse_reply_wm_class(reply)
	local data = self:parse_reply_string(reply, 'STRING')
	if data then
		local result = {}
		result.instance, result.class =
			string.match(data, '^(.*)[^\x01-\xff](.*)[^\x01-\xff]$')
		return result
	end
end

function M:parse_reply_wm_hints(reply)
	local data =
		self:parse_reply_struct(reply, 'WM_HINTS', 'xcb_icccm_wm_hints_t')
	if data then
		return {
			urgency = band(data.flags, c.XCB_ICCCM_WM_HINT_X_URGENCY) ~= 0,
		}
	end
end

function M:parse_reply_window(reply)
	local data = self:parse_reply_struct(reply, 'WINDOW', 'xcb_window_t')
	if data then
		return data[0]
	end
end

function M:parse_reply_cardinal(reply)
	local data = self:parse_reply_struct(reply, 'CARDINAL', 'uint32_t')
	if data then
		return data[0]
	end
end

function M:parse_reply_atoms(reply)
	local data, size = self:parse_reply_type(reply, 'ATOM')
	if data then
		local result = {}
		local data = ffi_cast('xcb_atom_t *', data)
		for i = 0, size / 4 - 1 do
			result[self.atoms[data[i]]] = true
		end
		return result
	end
end

function M:has_windows()
	for _ in pairs(self.windows_by_id) do
		return true
	end
	return false
end

local function parse_randr_rotation(bits)
	return {
		rotate = ({ [1] = 0, [2] = 90, [4] = 180, [8] = 270 })[band(bits, 15)],
		reflect_x = band(bits, 16) ~= 0,
		reflect_y = band(bits, 32) ~= 0,
	}
end

local function serialize_randr_rotation(t)
	return (
		({ [0] = 1, [90] = 2, [180] = 4, [270] = 8 })[t.rotate or 0]
		+ (t.reflect_x and 16 or 0)
		+ (t.reflect_y and 32 or 0)
	)
end

-- Copied from xrandr's mode_refresh().
local function get_mode_refresh(mode)
	local vtotal = mode.vtotal

	if band(mode.mode_flags, c.XCB_RANDR_MODE_FLAG_DOUBLE_SCAN) ~= 0 then
		vtotal = vtotal * 2
	end

	if band(mode.mode_flags, c.XCB_RANDR_MODE_FLAG_INTERLACE) ~= 0 then
		vtotal = vtotal / 2
	end

	local rate = mode.dot_clock / (vtotal * mode.htotal)

	if rate == math.huge then
		return 0
	end

	return rate
end

local function match_mode(mode, spec)
	if spec.id and mode.id ~= spec.id then
		return math.huge
	end

	if spec.name and mode.name ~= spec.name then
		return math.huge
	end

	if spec.width and mode.width ~= spec.width then
		return math.huge
	end

	if spec.height and mode.height ~= spec.height then
		return math.huge
	end

	if spec.refresh then
		return math.abs(mode.refresh - spec.refresh)
	else
		return -mode.refresh
	end
end

local function find_mode(spec, ...)
	local best, best_mode = math.huge
	for _, mode in ... do
		local score = match_mode(mode, spec)
		if score < best then
			best, best_mode = score, mode
		end
	end
	return best_mode
end

local function resolve_crtc_configs(crtc_configs, output_configs, index)
	local output_config = output_configs[index]
	if not output_config then
		return true
	end

	for _, crtc in ipairs(output_config.possible_crtcs) do
		local crtc_config = crtc_configs[crtc.id]

		if not crtc_config.randr_config then
			crtc_config.randr_config = output_config.randr_config
			if resolve_crtc_configs(crtc_configs, output_configs, index + 1) then
				table.insert(crtc_config.output_ids, output_config.output.id)
				return true
			end
			crtc_config.randr_config = nil
		elseif crtc_config.randr_config == output_config.randr_config then
			if resolve_crtc_configs(crtc_configs, output_configs, index + 1) then
				table.insert(crtc_config.output_ids, output_config.output.id)
				return true
			end
		end
	end
end

local function commit_randr_config_helper(self, root_id, config, user_config)
	local new_screen_width, new_screen_height = 0, 0

	local output_configs = {}
	local primary_output_id = c.XCB_NONE
	local randr_configs = {}

	for output_name, output_spec in pairs(user_config) do
		if not output_spec.virtual and output_spec.enabled ~= false then
			local output = assert(
				config.outputs_by_name[output_name],
				string.format("invalid output '%s'", output_name)
			)

			local mode
			if output_spec.mode then
				mode = assert(
					find_mode(output_spec.mode, ipairs(output.modes)),
					string.format("invalid mode for output '%s'", output_name)
				)
			else
				mode = output.preferred_mode
			end

			local possible_crtcs
			if output_spec.crtc then
				possible_crtcs = { output_spec.crtc }
			else
				possible_crtcs = {}

				if output.crtc then
					table.insert(possible_crtcs, output.crtc)
				end

				for _, crtc in ipairs(output.possible_crtcs) do
					table.insert(possible_crtcs, crtc)
				end
			end

			local randr_config = {
				x = output_spec.x or 0,
				y = output_spec.y or 0,
				mode_id = mode.id,
				rotation = serialize_randr_rotation(output_spec or {}),
			}
			local key = string.format(
				'%d,%d,%d,%d',
				randr_config.x,
				randr_config.y,
				randr_config.mode_id,
				randr_config.rotation
			)
			if randr_configs[key] then
				randr_config = randr_configs[key]
			else
				randr_configs[key] = randr_config
			end

			local output_config = {
				output = output,
				randr_config = randr_config,
				possible_crtcs = possible_crtcs,
			}

			table.insert(output_configs, output_config)

			if output_spec.primary then
				primary_output_id = output.id
			end

			new_screen_width = math.max(new_screen_width, randr_config.x + mode.width)
			new_screen_height =
				math.max(new_screen_height, randr_config.y + mode.height)
		end
	end

	local crtc_configs = {}

	for crtc_id in pairs(config.crtcs_by_id) do
		crtc_configs[crtc_id] = {
			config = nil,
			output_ids = {},
		}
	end

	if not resolve_crtc_configs(crtc_configs, output_configs, 1) then
		error('cannot allocate CRTCs')
	end

	local monitors_cookie = c.xcb_randr_get_monitors(self.conn, root_id, false)
	local screen_info_cookie = c.xcb_randr_get_screen_info(self.conn, root_id)

	local monitors_reply = monitors_cookie:assert_reply(self.conn)

	local monitors_by_name = {}

	for monitor in monitors_reply:monitors() do
		local name = self.atoms[monitor.name]
		monitors_by_name[name] = monitor

		if string.match(name, '^VIRTUAL_') then
			c.xcb_randr_delete_monitor(self.conn, root_id, monitor.name)
				:discard_check(self.conn)
		end
	end

	local monitor_infos = {}

	for output_name, output_spec in pairs(user_config) do
		if output_spec.virtual then
			local default = monitors_by_name[output_spec.inherit] or {}
			local x = output_spec.x or default.x
			local y = output_spec.y or default.y
			local width = output_spec.width or default.width
			local height = output_spec.height or default.height

			new_screen_width = math.max(new_screen_width, x + width)
			new_screen_height = math.max(new_screen_height, y + height)

			local monitor_info = ffi_new('xcb_randr_monitor_info_t', {
				name = self.atoms['VIRTUAL_' .. output_name],
				primary = false,
				automatic = true,
				nOutput = 0,
				x = x,
				y = y,
				width = width,
				height = height,
				width_in_millimeters = output_spec.width_mm
					or default.width_in_millimeters,
				height_in_millimeters = output_spec.height_mm
					or default.height_in_millimeters,
			})

			table.insert(monitor_infos, monitor_info)
		end
	end

	local screen_info_reply = screen_info_cookie:assert_reply(self.conn)
	local screen_size = screen_info_reply:sizes()[screen_info_reply.sizeID]

	if
		screen_size.width < new_screen_width
		or screen_size.height < new_screen_height
	then
		c.xcb_randr_set_screen_size(
			self.conn,
			root_id,
			math.max(screen_size.width, new_screen_width),
			math.max(screen_size.height, new_screen_height),
			screen_size.mwidth,
			screen_size.mheight
		):discard_check(self.conn)
	end

	for _, monitor_info in ipairs(monitor_infos) do
		c.xcb_randr_set_monitor_checked(self.conn, root_id, monitor_info)
			:assert_check(self.conn)
	end

	for crtc_id in pairs(config.crtcs_by_id) do
		local crtc_config = crtc_configs[crtc_id]
		if #crtc_config.output_ids > 0 then
			c.xcb_randr_set_crtc_config(
				self.conn,
				crtc_id,
				c.XCB_CURRENT_TIME,
				config.timestamp,
				crtc_config.randr_config.x,
				crtc_config.randr_config.y,
				crtc_config.randr_config.mode_id,
				crtc_config.randr_config.rotation,
				#crtc_config.output_ids,
				ffi_new(
					'xcb_randr_output_t[?]',
					#crtc_config.output_ids,
					crtc_config.output_ids
				)
			):assert_reply(self.conn)
		else
			c.xcb_randr_set_crtc_config_unchecked(
				self.conn,
				crtc_id,
				c.XCB_CURRENT_TIME,
				config.timestamp,
				0,
				0,
				c.XCB_NONE,
				serialize_randr_rotation({}),
				0,
				nil
			):discard_reply(self.conn)
		end
	end

	c.xcb_randr_set_screen_size_checked(
		self.conn,
		root_id,
		new_screen_width,
		new_screen_height,
		100, -- FIXME: But apparently all programs give a fuck about it.
		100
	):assert_check(self.conn)

	c.xcb_randr_set_output_primary_checked(self.conn, root_id, primary_output_id)
		:assert_check(self.conn)
end

function M:set_randr_config(root_id, user_config)
	local config = self:get_randr_config(root_id)

	if type(user_config) == 'function' then
		user_config = user_config(config)
	end
	if not user_config then
		return
	end

	c.xcb_grab_server(self.conn):discard_check(self.conn)

	local ok, err =
		pcall(commit_randr_config_helper, self, root_id, config, user_config)

	c.xcb_ungrab_server(self.conn):discard_check(self.conn)

	if not ok then
		error(tostring(err))
	end
end

function M:get_randr_config(root_id)
	local reply = c.xcb_randr_get_screen_resources(self.conn, root_id)
		:assert_reply(self.conn)
	local config_timestamp = reply.config_timestamp

	local output_primary_cookie =
		c.xcb_randr_get_output_primary(self.conn, root_id)

	local output_cookie_queue = {}

	for _, output_id in reply:ioutputs() do
		local cookie =
			c.xcb_randr_get_output_info(self.conn, output_id, config_timestamp)
		local properties_cookie =
			c.xcb_randr_list_output_properties(self.conn, output_id)
		table.insert(output_cookie_queue, {
			output_id,
			cookie,
			properties_cookie,
		})
	end

	local modes_by_id = {}
	do
		local names = reply:names()
		local names_offset = 0

		for _, mode in reply:imodes() do
			modes_by_id[mode.id] = {
				id = mode.id,
				name = ffi_string(names + names_offset, mode.name_len),
				width = mode.width,
				height = mode.height,
				refresh = get_mode_refresh(mode),
			}
			names_offset = names_offset + mode.name_len
		end
	end

	local output_primary_reply = output_primary_cookie:assert_reply(self.conn)

	local outputs_by_id = {}
	local output_property_cookie_queue = {}

	for _, x in ipairs(output_cookie_queue) do
		local output_id, cookie, properties_cookie = unpack(x)
		local reply = cookie:assert_reply(self.conn)

		local output = {
			id = output_id,
			connected = ({
				[c.XCB_RANDR_CONNECTION_CONNECTED] = true,
				[c.XCB_RANDR_CONNECTION_DISCONNECTED] = false,
				[c.XCB_RANDR_CONNECTION_UNKNOWN] = nil,
			})[reply.connection],
			physical_size = {
				width = reply.mm_width,
				height = reply.mm_height,
			},
			name = reply:name(),
			modes = {},
			properties = {},
			possible_crtcs = {},
			primary = output_primary_reply.output == output_id,
			crtc = nil,
		}

		for i, mode_id in reply:imodes() do
			output.modes[i] = modes_by_id[mode_id]
		end

		output.preferred_mode = output.modes[reply.num_preferred]

		outputs_by_id[output.id] = output

		local properties_reply = properties_cookie:assert_reply(self.conn)

		for i, atom in properties_reply:iatoms() do
			local cookie = c.xcb_randr_get_output_property(
				self.conn,
				output.id,
				atom,
				c.XCB_ATOM_ANY,
				0,
				-1,
				false,
				false
			)
			local query_cookie =
				c.xcb_randr_query_output_property(self.conn, output.id, atom)
			table.insert(
				output_property_cookie_queue,
				{ output, atom, cookie, query_cookie }
			)
		end
	end

	local outputs_by_name = {}

	for _, output in pairs(outputs_by_id) do
		assert(not outputs_by_name[output.name], 'duplicated output name')
		outputs_by_name[output.name] = output
	end

	local crtc_cookie_queue = {}

	for _, crtc_id in reply:icrtcs() do
		local cookie =
			c.xcb_randr_get_crtc_info(self.conn, crtc_id, config_timestamp)
		table.insert(crtc_cookie_queue, { crtc_id, cookie })
	end

	local crtcs_by_id = {}

	for _, x in ipairs(crtc_cookie_queue) do
		local crtc_id, cookie = unpack(x)
		local reply = cookie:assert_reply(self.conn)

		local crtc = {
			id = crtc_id,
			x = reply.x,
			y = reply.y,
			mode = modes_by_id[reply.mode],
			rotation = parse_randr_rotation(reply.rotation),
			outputs = {},
			possible_outputs = {},
			width = reply.width,
			height = reply.height,
		}

		for i, output_id in reply:ioutputs() do
			local output = outputs_by_id[output_id]
			crtc.outputs[i] = output
			output.crtc = crtc
		end

		for i, output_id in reply:ipossible() do
			local output = outputs_by_id[output_id]
			crtc.possible_outputs[i] = output
			table.insert(output.possible_crtcs, crtc)
		end

		crtcs_by_id[crtc.id] = crtc
	end

	for _, x in ipairs(output_property_cookie_queue) do
		local output, atom, cookie, query_cookie = unpack(x)
		local reply = cookie:assert_reply(self.conn)

		local property = {
			name = self.atoms[atom],
			type = self.atoms[reply.format],
			data = reply:data(),
		}

		local reply = query_cookie:assert_reply(self.conn)

		property.immutable = reply.immutable ~= 0

		if reply.range ~= 0 then
			property.valid_values = {}
			for i, value in reply:ivalid_values() do
				property.valid_values[i] = value
			end
		else
			property.valid_range = {}
			for i, value in reply:ivalid_values() do
				property.valid_range[i == 1 and 'min' or 'max'] = value
			end
		end

		output.properties[property.name] = property
	end

	local config = {
		timestamp = config_timestamp,
		crtcs_by_id = crtcs_by_id,
		modes_by_id = modes_by_id,
		outputs_by_id = outputs_by_id,
		outputs_by_name = outputs_by_name,
	}

	return config
end

return M
