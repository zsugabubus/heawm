local M = {}

function M.press(cb)
	return function(self, event)
		if event.event == 'press' then
			cb(self, event)
		end
	end
end

function M.down(cb)
	return function(self, event)
		if event.event ~= 'release' then
			cb(self, event)
		end
	end
end

function M.window(cb)
	return function(self, event)
		local user = event.user
		local window = event.user.focused_window
		if window then
			cb(self, window, event)
		end
	end
end

function M.press_window(cb)
	return M.press(M.window(cb))
end

function M.down_window(cb)
	return M.down(M.window(cb))
end

return M
