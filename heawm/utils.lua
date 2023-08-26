local M = {}

function M.key_press(cb)
	return function(self, event)
		if event.event == 'press' then
			cb(self, event)
		end
	end
end

function M.key_down(cb)
	return function(self, event)
		if event.event ~= 'release' then
			cb(self, event)
		end
	end
end

function M.key_window(cb)
	return function(self, event)
		local user = event.user
		local window = event.user.focused_window
		if window then
			cb(self, window, event)
		end
	end
end

function M.key_press_window(cb)
	return M.key_press(M.key_window(cb))
end

function M.key_down_window(cb)
	return M.key_down(M.key_window(cb))
end

return M
