local M = {}

function M.new(conn)
	local c = require('heawm.ffi')

	local function send_request(any)
		if type(any) == 'number' then
			local atom = any
			assert(atom > 0)
			return c.xcb_get_atom_name(conn, atom)
		else
			local name = any
			return c.xcb_intern_atom(conn, false, #name, name)
		end
	end

	local function receive_reply(cache, any, cookie)
		if type(any) == 'number' then
			local atom = any
			local reply = cookie:assert_reply(conn)

			local name = reply:name()

			cache[name] = atom
			cache[atom] = name

			return name
		else
			local name = any
			local reply = cookie:assert_reply(conn)

			local atom = reply.atom

			cache[name] = atom
			cache[atom] = name

			return atom
		end
	end

	return setmetatable({}, {
		__call = function(cache, multi)
			if #multi == 0 then
				local cookies = {}
				local result = {}

				for any in pairs(multi) do
					local resolved = rawget(cache, any)
					if not resolved then
						cookies[any] = send_request(any)
					end
				end

				for any, v in pairs(multi) do
					local cookie = cookies[any]
					if cookie then
						any = receive_reply(cache, any, cookie)
					end
					result[any] = v
				end

				return result
			else
				local cookies = {}

				for i, any in ipairs(multi) do
					local resolved = rawget(cache, any)
					local cookie = false

					if resolved then
						multi[i] = resolved
					else
						cookie = send_request(any)
					end

					cookies[i] = cookie
				end

				for i, cookie in ipairs(cookies) do
					if cookie then
						multi[i] = receive_reply(cache, multi[i], cookie)
					end
				end

				return multi
			end
		end,
		__index = function(cache, any)
			return receive_reply(cache, any, send_request(any))
		end,
	})
end

return M
