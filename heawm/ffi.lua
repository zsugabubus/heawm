local _ = require('ffi')
local ffi_cast, ffi_C, ffi_cdef, ffi_sizeof, ffi_string, ffi_gc, ffi_metatype, ffi_new, ffi_load =
	_.cast, _.C, _.cdef, _.sizeof, _.string, _.gc, _.metatype, _.new, _.load

local M = {}

ffi_cdef([[
void free(void *);

struct iovec {
	void *iov_base;
	size_t iov_len;
};
]])

local function ffi_string_nil(data)
	if data ~= nil then
		return ffi_string(data)
	end
end

local cairo = ffi_load('cairo.so', true)
local xcb = ffi_load('xcb.so', true)
local xcb_cursor = ffi_load('xcb-cursor.so', true)
local xcb_input = ffi_load('xcb-xinput.so', true)
local xcb_randr = ffi_load('xcb-randr.so', true)
local xcb_shape = ffi_load('xcb-shape.so', true)
local xcb_xtest = ffi_load('xcb-xtest.so', true)
local xcb_util = ffi_load('xcb-util.so', true)
local xcb_xfixes = ffi_load('xcb-xfixes.so', true)
local xcb_xkb = ffi_load('xcb-xkb.so', true)
local xkb = ffi_load('xkbcommon.so', true)
local xkb_x11 = ffi_load('xkbcommon-x11.so', true)

local function include(name)
	local path = string.format('/usr/include/%s', name)
	local f = assert(io.open(path))
	local source = f:read('*a')
	f:close()

	source = source
		:gsub('/%*.-%*/', ' ')
		:gsub('L << ', '<< ')
		:gsub('\\\n *', '')
		-- "#define A 20" -> "enum { A = 20 }"
		:gsub(
			'#define ([A-Z_]+) [^\n]-([0-9a-fA-Fx-]+)L?[)]*\n',
			'enum { %1 = %2 };'
		)
		-- "#if ..."
		:gsub('#[^\n]*', '')
		-- "extern C {"
		:gsub('extern[^\n]*{', '')
		-- "}"
		:gsub('\n} *\n', '')

	ffi_cdef(source)

	return source
end

local out_err

local Cookie = {
	__index = {
		-- Both *(un)checked(). When *checked(), error must be free()d.
		unsafe_reply = function(self, conn)
			return ffi_gc(
				ffi_C[self.xcb_get_reply_name](conn, self, out_err),
				ffi_C.free
			),
				ffi_gc(out_err[0], ffi_C.free)
		end,
		-- SHOULD NOT be *unchecked() so error is more discoverable.
		-- All other cookies must be cleaned up.
		assert_reply = function(self, conn)
			local reply, err = self:unsafe_reply(conn)
			if reply == nil then
				error(tostring(err))
			end
			return reply
		end,
		-- With *unchecked().
		discard_reply = function(self, conn)
			return xcb.xcb_discard_reply(conn, self.sequence)
		end,
	},
}

local function process_cookies(source, name)
	string.gsub(
		source,
		'struct ((xcb_[a-z_]*)_cookie_t)',
		function(cookie_name, name)
			-- Blacklist.
			if string.match(name, '^xcb_render_') then
				return
			end

			local t = {
				xcb_get_reply_name = name .. '_reply',
			}
			t.__index = t
			ffi_metatype(cookie_name, setmetatable(t, Cookie))
		end
	)
	return source
end

process_cookies(include('xcb/xcb.h'))
process_cookies(include('xcb/xproto.h'))
include('xcb/xcbext.h')
include('xcb/shape.h')
include('xcb/render.h')
process_cookies(include('xcb/xfixes.h'))
process_cookies(include('xcb/xinput.h'))
process_cookies(include('xcb/xkb.h'))
process_cookies(include('xcb/randr.h'))
include('xcb/xtest.h')
include('xcb/xcb_event.h')
include('xcb/xcb_cursor.h')
include('xcb/xcb_icccm.h')
include('xcb/xcb_aux.h')

local cairo_defs = [[
typedef struct cairo_t cairo_t;
typedef struct cairo_surface_t cairo_surface_t;

typedef struct {
	double x_bearing;
	double y_bearing;
	double width;
	double height;
	double x_advance;
	double y_advance;
} cairo_text_extents_t;

typedef enum {
	CAIRO_ANTIALIAS_DEFAULT,
	CAIRO_ANTIALIAS_NONE,
	CAIRO_ANTIALIAS_GRAY,
	CAIRO_ANTIALIAS_SUBPIXEL,
	CAIRO_ANTIALIAS_FAST,
	CAIRO_ANTIALIAS_GOOD,
	CAIRO_ANTIALIAS_BEST
} cairo_antialias_t;

typedef enum {
	CAIRO_FONT_SLANT_NORMAL,
	CAIRO_FONT_SLANT_ITALIC,
	CAIRO_FONT_SLANT_OBLIQUE
} cairo_font_slant_t;

typedef enum {
	CAIRO_FONT_WEIGHT_NORMAL,
	CAIRO_FONT_WEIGHT_BOLD
} cairo_font_weight_t;

typedef enum {
	CAIRO_OPERATOR_CLEAR,
	CAIRO_OPERATOR_SOURCE,
	CAIRO_OPERATOR_OVER,
} cairo_operator_t;

cairo_surface_t *cairo_get_target(cairo_t *);
cairo_surface_t *cairo_xcb_surface_create(xcb_connection_t *, xcb_drawable_t, xcb_visualtype_t *, int, int);
cairo_surface_t *cairo_xcb_surface_create_for_bitmap(xcb_connection_t *, xcb_screen_t *, xcb_pixmap_t, int, int);
cairo_t *cairo_create(cairo_surface_t *);
void cairo_arc(cairo_t *, double, double, double, double, double);
void cairo_destroy(cairo_t *);
void cairo_fill(cairo_t *);
void cairo_move_to(cairo_t *, double, double);
void cairo_new_path(cairo_t *);
void cairo_paint(cairo_t *);
void cairo_pop_group_to_source(cairo_t *);
void cairo_push_group(cairo_t *);
void cairo_rectangle(cairo_t *, double, double, double, double);
void cairo_restore(cairo_t *);
void cairo_save(cairo_t *);
void cairo_select_font_face(cairo_t *, const char *, cairo_font_slant_t, cairo_font_weight_t);
void cairo_set_antialias(cairo_t *, cairo_antialias_t);
void cairo_set_font_size(cairo_t *, double);
void cairo_set_line_width(cairo_t *, double);
void cairo_set_operator(cairo_t *, cairo_operator_t);
void cairo_set_source_rgb(cairo_t *, double, double, double);
void cairo_show_text(cairo_t *, const char *);
void cairo_stroke(cairo_t *);
void cairo_surface_destroy(cairo_surface_t *);
void cairo_surface_flush(cairo_surface_t *surface);
void cairo_text_extents(cairo_t *, const char *, cairo_text_extents_t *);
void cairo_text_path(cairo_t *, const char *);
void cairo_translate(cairo_t *, double, double);
]]

ffi_cdef(cairo_defs)

ffi_cdef([[
typedef struct FILE FILE;
]])

include('xkbcommon/xkbcommon.h')
include('xkbcommon/xkbcommon-x11.h')

out_err = ffi_new('xcb_generic_error_t *[1]')

local function xcb_iterator_factory(xcb_get_iterator, xcb_next)
	local function next(state)
		if state.rem > 0 then
			local data = state.data
			xcb_next(state)
			return data
		end
	end

	return function(self)
		return next, xcb_get_iterator(self)
	end
end

local function xcb_indexed_iterator_factory(xcb_get_data, xcb_get_length)
	local function next(self, i)
		local n = xcb_get_length(self)
		if i < n then
			local data = xcb_get_data(self)
			return i + 1, data[i]
		end
	end

	return function(self)
		return next, self, 0
	end
end

local function xcb_string_factory(xcb_get_data, xcb_get_length)
	return function(self)
		return ffi_string(xcb_get_data(self), xcb_get_length(self))
	end
end

ffi_metatype('xcb_generic_event_t', {
	__tostring = function(self)
		assert(self ~= nil)
		return string.format(
			'Generic event (type: %s (%d), sequence: %d)',
			ffi_string(xcb_util.xcb_event_get_label(self.response_type)),
			self.response_type,
			self.sequence
		)
	end,
})

ffi_metatype('xcb_generic_error_t', {
	__tostring = function(self)
		assert(self ~= nil)
		return string.format(
			'Generic error (error code: %s (%d), resource id: %d=0x%08x, major code: %s (%d), minor code: %d)',
			ffi_string_nil(xcb_util.xcb_event_get_error_label(self.error_code))
				or 'unknown',
			self.error_code,
			self.resource_id,
			self.resource_id,
			ffi_string_nil(xcb_util.xcb_event_get_request_label(self.major_code))
				or 'unknown',
			self.major_code,
			self.minor_code
		)
	end,
})

local function generate_cairo_index()
	local index = {}
	for cairo_name, name in
		string.gmatch(cairo_defs, '(cairo_([a-z_]*))%(cairo_t')
	do
		index[name] = cairo[cairo_name]
	end
	return index
end

ffi_metatype('cairo_t', {
	__index = generate_cairo_index(),
})

ffi_metatype('xcb_query_tree_reply_t', {
	__index = {
		ichildren = xcb_indexed_iterator_factory(
			xcb.xcb_query_tree_children,
			xcb.xcb_query_tree_children_length
		),
	},
})

ffi_metatype('xcb_input_button_press_event_t', {
	__index = {
		ibutton_mask = xcb_indexed_iterator_factory(
			xcb_input.xcb_input_button_press_button_mask,
			xcb_input.xcb_input_button_press_button_mask_length
		),
		ivaluator_mask = xcb_indexed_iterator_factory(
			xcb_input.xcb_input_button_press_valuator_mask,
			xcb_input.xcb_input_button_press_valuator_mask_length
		),
		iaxisvalues = xcb_indexed_iterator_factory(
			xcb_input.xcb_input_button_press_axisvalues,
			xcb_input.xcb_input_button_press_axisvalues_length
		),
	},
})

ffi_metatype('xcb_input_xi_device_info_t', {
	__index = {
		name = xcb_string_factory(
			xcb_input.xcb_input_xi_device_info_name,
			xcb_input.xcb_input_xi_device_info_name_length
		),
	},
})

ffi_metatype('xcb_input_xi_query_device_reply_t', {
	__index = {
		infos = xcb_iterator_factory(
			xcb_input.xcb_input_xi_query_device_infos_iterator,
			xcb_input.xcb_input_xi_device_info_next
		),
	},
})

ffi_metatype('xcb_setup_t', {
	__index = {
		roots = xcb_iterator_factory(
			xcb.xcb_setup_roots_iterator,
			xcb.xcb_screen_next
		),
	},
})

ffi_metatype('xcb_screen_t', {
	__index = {
		allowed_depths = xcb_iterator_factory(
			xcb.xcb_screen_allowed_depths_iterator,
			xcb.xcb_depth_next
		),
	},
})

ffi_metatype('xcb_depth_t', {
	__index = {
		visuals = xcb_iterator_factory(
			xcb.xcb_depth_visuals_iterator,
			xcb.xcb_visualtype_next
		),
	},
})

ffi_metatype('xcb_randr_get_monitors_reply_t', {
	__index = {
		monitors = xcb_iterator_factory(
			xcb_randr.xcb_randr_get_monitors_monitors_iterator,
			xcb_randr.xcb_randr_monitor_info_next
		),
	},
})

ffi_metatype('xcb_randr_get_screen_resources_reply_t', {
	__index = {
		icrtcs = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_screen_resources_crtcs,
			xcb_randr.xcb_randr_get_screen_resources_crtcs_length
		),
		ioutputs = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_screen_resources_outputs,
			xcb_randr.xcb_randr_get_screen_resources_outputs_length
		),
		imodes = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_screen_resources_modes,
			xcb_randr.xcb_randr_get_screen_resources_modes_length
		),
		names = xcb_randr.xcb_randr_get_screen_resources_names,
	},
})

ffi_metatype('xcb_randr_get_output_info_reply_t', {
	__index = {
		icrtcs = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_output_info_crtcs,
			xcb_randr.xcb_randr_get_output_info_crtcs_length
		),
		imodes = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_output_info_modes,
			xcb_randr.xcb_randr_get_output_info_modes_length
		),
		iclones = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_output_info_clones,
			xcb_randr.xcb_randr_get_output_info_clones_length
		),
		name = xcb_string_factory(
			xcb_randr.xcb_randr_get_output_info_name,
			xcb_randr.xcb_randr_get_output_info_name_length
		),
	},
})

ffi_metatype('xcb_randr_get_crtc_info_reply_t', {
	__index = {
		ioutputs = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_crtc_info_outputs,
			xcb_randr.xcb_randr_get_crtc_info_outputs_length
		),
		ipossible = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_crtc_info_possible,
			xcb_randr.xcb_randr_get_crtc_info_possible_length
		),
	},
})

ffi_metatype('xcb_randr_get_screen_info_reply_t', {
	__index = {
		sizes = xcb_randr.xcb_randr_get_screen_info_sizes,
		isizes = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_get_screen_info_sizes,
			xcb_randr.xcb_randr_get_screen_info_sizes_length
		),
		rates = xcb_iterator_factory(
			xcb_randr.xcb_randr_get_screen_info_rates_iterator,
			xcb_randr.xcb_randr_refresh_rates_next
		),
	},
})

ffi_metatype('xcb_randr_list_output_properties_reply_t', {
	__index = {
		iatoms = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_list_output_properties_atoms,
			xcb_randr.xcb_randr_list_output_properties_atoms_length
		),
	},
})

ffi_metatype('xcb_randr_query_output_property_reply_t', {
	__index = {
		ivalid_values = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_query_output_property_valid_values,
			xcb_randr.xcb_randr_query_output_property_valid_values_length
		),
	},
})

ffi_metatype('xcb_randr_get_output_property_reply_t', {
	__index = {
		data = xcb_string_factory(
			xcb_randr.xcb_randr_get_output_property_data,
			xcb_randr.xcb_randr_get_output_property_data_length
		),
	},
})

ffi_metatype('xcb_randr_refresh_rates_t', {
	__index = {
		irates = xcb_indexed_iterator_factory(
			xcb_randr.xcb_randr_refresh_rates_rates,
			xcb_randr.xcb_randr_refresh_rates_rates_length
		),
	},
})

ffi_metatype('xcb_get_atom_name_reply_t', {
	__index = {
		name = xcb_string_factory(
			xcb.xcb_get_atom_name_name,
			xcb.xcb_get_atom_name_name_length
		),
	},
})

ffi_metatype('xcb_input_xi_list_properties_reply_t', {
	__index = {
		atoms = xcb_indexed_iterator_factory(
			xcb_input.xcb_input_xi_list_properties_properties,
			xcb_input.xcb_input_xi_list_properties_properties_length
		),
	},
})

ffi_metatype('xcb_input_xi_get_property_reply_t', {
	__index = {
		items = xcb_input.xcb_input_xi_get_property_items,
	},
})

ffi_metatype('xcb_input_get_device_modifier_mapping_reply_t', {
	__index = {
		keymaps = xcb_input.xcb_input_get_device_modifier_mapping_keymaps,
	},
})

ffi_metatype('xcb_void_cookie_t', {
	__index = {
		-- With *checked().
		unsafe_check = function(self, conn)
			return ffi_gc(xcb.xcb_request_check(conn, self), ffi_C.free)
		end,
		-- With *checked().
		assert_check = function(self, conn)
			local err = self:unsafe_check(conn)
			assert(err == nil, err)
		end,
		-- With *unchecked().
		discard_check = function(self, conn)
			xcb.xcb_discard_reply(conn, self.sequence)
		end,
	},
})

M.CursorContext = ffi_metatype('xcb_cursor_context_t', {
	__index = {
		new = function(conn, screen)
			local out = ffi_new('xcb_cursor_context_t *[1]')
			assert(xcb_cursor.xcb_cursor_context_new(conn, screen, out) >= 0)
			return ffi_gc(
				ffi_new('xcb_cursor_context_t *', out[0]),
				xcb_cursor.xcb_cursor_context_free
			)
		end,
		load_cursor = xcb_cursor.xcb_cursor_load_cursor,
	},
})

ffi_metatype('struct xkb_keymap', {
	__index = {
		num_mods = xkb.xkb_keymap_num_mods,
		min_keycode = xkb.xkb_keymap_min_keycode,
		max_keycode = xkb.xkb_keymap_max_keycode,
		num_layouts_for_key = xkb.xkb_keymap_num_layouts_for_key,
		num_levels_for_key = xkb.xkb_keymap_num_levels_for_key,
		key_get_syms_by_level = xkb.xkb_keymap_key_get_syms_by_level,
		key_get_mods_for_level = xkb.xkb_keymap_key_get_mods_for_level,
		mod_get_name = xkb.xkb_keymap_mod_get_name,
	},
})

M.Connection = ffi_metatype('xcb_connection_t', {
	__index = {
		new = function(displayname)
			return ffi_gc(xcb.xcb_connect(displayname, nil), xcb.xcb_disconnect)
		end,
		poll_for_event = function(self)
			local event = xcb.xcb_poll_for_event(self)
			if event ~= nil then
				return ffi_gc(event, ffi_C.free)
			end
		end,
		poll_for_queued_event = function(self)
			local event = xcb.xcb_poll_for_queued_event(self)
			if event ~= nil then
				return ffi_gc(event, ffi_C.free)
			end
		end,
		setup = xcb.xcb_get_setup,
		file_descriptor = xcb.xcb_get_file_descriptor,
		generate_id = xcb.xcb_generate_id,
		has_error = xcb.xcb_connection_has_error,
	},
})

local function serialize_component_names(x)
	assert(#x.keymap <= 255)
	assert(#x.keycodes <= 255)
	assert(#x.types <= 255)
	assert(#x.compat <= 255)
	assert(#x.symbols <= 255)
	assert(#x.geometry <= 255)
	local len = (
		6
		+ #x.keymap
		+ #x.keycodes
		+ #x.types
		+ #x.compat
		+ #x.symbols
		+ #x.geometry
	)
	local pad = string.rep('\0', (4 - len) % 4)
	return string.format(
		'%c%s%c%s%c%s%c%s%c%s%c%s%s',
		#x.keymap,
		x.keymap,
		#x.keycodes,
		x.keycodes,
		#x.types,
		x.types,
		#x.compat,
		x.compat,
		#x.symbols,
		x.symbols,
		#x.geometry,
		x.geometry,
		pad
	)
end

-- Based on XkbGetKeyboardByName.
function M.xcb_xkb_get_kbd_by_name_ext_unchecked(
	c,
	device_spec,
	component_names,
	need,
	want,
	load
)
	local xcb_req = ffi_new('xcb_protocol_request_t', {
		count = 2,
		ext = xcb_xkb.xcb_xkb_id,
		opcode = xcb_xkb.XCB_XKB_GET_KBD_BY_NAME,
		isvoid = 0,
	})

	local xcb_parts = ffi_new('struct iovec[4]')
	local xcb_ret = ffi_new('xcb_xkb_get_kbd_by_name_cookie_t')
	local xcb_out = ffi_new('xcb_xkb_get_kbd_by_name_request_t', {
		deviceSpec = device_spec,
		need = need,
		want = want,
		load = load,
	})

	xcb_parts[2].iov_base = xcb_out
	xcb_parts[2].iov_len = ffi_sizeof(xcb_out)
	assert(ffi_sizeof(xcb_out) % 4 == 0)
	local s = serialize_component_names(component_names)
	xcb_parts[3].iov_base = ffi_cast('void *', s)
	xcb_parts[3].iov_len = #s

	xcb_ret.sequence = xcb.xcb_send_request(c, 0, xcb_parts + 2, xcb_req)
	return xcb_ret
end

-- Needs some ref.
M._refs = { xkb_x11, xcb_shape, xcb_xtest, cairo }

-- local i = 0
setmetatable(M, {
	--[[
	__index = function(_, name)
		local f = ffi_C[name]
		if type(f) == 'cdata' and not string.match(name, '_id$') then
			return function(...)
				local start = os.clock()
				local r = f(...)
				i = i + 1
				print(
					string.format(
						'(%-6d) # TIME %.3fms %s(%s)',
						i,
						(os.clock() - start) * 1000,
						name,
						string.sub((require('inspect'))({ ... }), 3, -3)
					)
				)
				return r
			end
		end
		return f
	end,
	]]
	__index = ffi_C,
})

return M
