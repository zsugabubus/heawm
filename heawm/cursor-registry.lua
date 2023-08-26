local M = {}

function M.new(conn, screen)
	local ffi = require('ffi')
	local c = require('heawm.ffi')

	local cursor_ctx = c.CursorContext.new(conn, screen)

	-- Reference cycle has to be broken in order to allow __gc to run.
	local cache_ids = {}

	return setmetatable({}, {
		__index = function(cache, name)
			local cursor_id = cursor_ctx:load_cursor(name)
			if cursor_id == c.XCB_CURSOR_NONE then
				return
			else
				table.insert(cache_ids, cursor_id)
				cache[name] = cursor_id
				return cursor_id
			end
		end,
		__gc = ffi.gc(ffi.new('char[1]'), function()
			for _, cursor_id in ipairs(cache_ids) do
				c.xcb_free_cursor(conn, cursor_id):discard_check(conn)
			end
		end),
	})
end

return M
