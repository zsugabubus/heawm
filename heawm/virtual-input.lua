local c = require('heawm.ffi')
local utf8 = require('heawm.utf8')

local M = {}
M.__index = M

function M.new(cls, conn, keyboard, pointer)
	return setmetatable({
		conn = conn,
		keyboard = keyboard,
		pointer = pointer,
		delay = 0,
	}, cls)
end

local function emit_mod_mask(self, mod_mask)
	if self.mod_mask == mod_mask then
		return
	end
	local changes = self.mod_mask and bxor(self.mod_mask, mod_mask) or 0xff
	self.mod_mask = mod_mask

	local keymap = self.keyboard.keymap

	for i = 0, 7 do
		local bit = lshift(1, i)
		if band(changes, bit) ~= 0 then
			local press = band(mod_mask, bit) ~= 0

			local name = keymap.mod_mask_to_name[bit]
			local desc = keymap.name_to_desc[name][1]

			c.xcb_test_fake_input(
				self.conn,
				press and c.XCB_KEY_PRESS or c.XCB_KEY_RELEASE,
				desc.code,
				c.XCB_CURRENT_TIME,
				c.XCB_WINDOW_NONE,
				0,
				0,
				self.keyboard.id
			):discard_check(self.conn)
		end
	end
end

local function emit_key(type, spec)
	local keymap = self.keyboard.keymap

	local mods_table, detail_name = split_modifiers(spec)

	local desc = keymap.name_to_desc[detail_name]
	if not desc then
		local keysym = c.xkb_utf32_to_keysym(utf8.utf8_to_utf32(detail_name))
		local name = keymap.keysym_to_name[keysym]
		assert(name)
		desc = keymap.name_to_desc[name]
		assert(desc)
	end
	desc = desc[1]

	local mod_mask = bor(
		to_xcb_mod_mask(mods_table, keymap.name_to_mod_mask),
		to_xcb_mod_mask(desc.mods_table, {})
	)

	emit_mod_mask(self, mod_mask)

	c.xcb_test_fake_input(
		self.conn,
		type,
		desc.code,
		c.XCB_CURRENT_TIME + self.delay,
		c.XCB_WINDOW_NONE,
		0,
		0,
		self.keyboard.id
	):discard_check(self.conn)

	self.delay = 0
end

function M:sleep(seconds)
	self.delay = self.delay + seconds * 1000
	return self
end

function M:text(text)
	for c in utf8.utf8_codes(text) do
		self:down(c):up(c)
	end
	emit_mod_mask(self, 0)
	return self
end

function M:press(...)
	for _, detail_name in ipairs({ ... }) do
		self:down(detail_name):up(detail_name)
	end
	emit_mod_mask(self, 0)
	return self
end

function M:down(detail_name)
	emit_key(c.XCB_KEY_PRESS, detail_name)
	return self
end

function M:up(detail_name)
	emit_key(c.XCB_KEY_RELEASE, detail_name)
	return self
end
