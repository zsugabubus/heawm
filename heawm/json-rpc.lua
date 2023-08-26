local _ = require('cjson')
local json_decode, json_encode = _.decode, _.encode

local uv = require('luv')

local M = {}
M.__index = M

function M.new(cls, pipe)
	return setmetatable({
		request_id = 0,
		pipe = pipe,
		response_handlers = {},
	}, cls)
end

local function read_start(pipe, callback)
	local buffer = ''

	pipe:read_start(function(_, data)
		if not data then
			callback()
			return
		end

		for line, eol in string.gmatch(buffer .. data, '([^\n]*)(\n?)') do
			if eol == '' then
				buffer = line
				break
			end

			local msg = json_decode(line)

			callback(msg)
		end
	end)
end

local function handle_message(self, methods, msg)
	if msg.method then
		local handler = methods[msg.method]
		if not handler then
			if msg.id then
				self:send_error(msg.id, -32601, 'Method not found')
			end
			return
		end

		local ok, result = pcall(handler, self, unpack(msg.params))
		if result then
			self:send_response(msg.id, result)
		end
		return
	end

	if msg.error then
		error(string.format('%d: %s', msg.error.code, msg.error.message))
	end
	self.response_handlers[msg.id](self, msg.result)
end

function M:start(methods, on_close)
	read_start(self.pipe, function(msg)
		if not msg then
			on_close(self)
			return
		end

		local ok, err = pcall(handle_message, self, methods, msg)
		if not ok then
			print(err)
		end
	end)
end

local function noop_callback() end

function M:close()
	uv.shutdown(self.pipe, function()
		uv.close(self.pipe, noop_callback)
	end)
end

local function write_json_line(pipe, msg)
	uv.write(pipe, { json_encode(msg), '\n' }, noop_callback)
end

function M:send_request(method, params, on_response)
	self.request_id = self.request_id + 1

	local msg = {
		jsonrpc = '2.0',
		id = self.request_id,
		method = method,
		params = params,
	}

	write_json_line(self.pipe, msg)

	self.response_handlers[self.request_id] = on_response
end

function M:send_response(request_id, result)
	local msg = {
		jsonrpc = '2.0',
		id = request_id,
		result = result,
	}

	write_json_line(self.pipe, msg)
end

function M:send_notifiction(request_id, method, params)
	local msg = {
		jsonrpc = '2.0',
		method = method,
		params = params,
	}

	write_json_line(self.pipe, msg)
end

function M:send_error(request_id, code, message)
	local msg = {
		jsonrpc = '2.0',
		id = request_id,
		error = {
			code = code,
			message = message,
		},
	}

	write_json_line(self.pipe, msg)
end

return M
