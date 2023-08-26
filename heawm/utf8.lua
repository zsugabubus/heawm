local M = {}

function M.utf8_codes(s)
	-- http://www.lua.org/manual/5.3/manual.html#6.5
	local charpattern = '[\x01-\x7F\xC2-\xF4][\x80-\xBF]*'
	return string.gmatch(s, charpattern)
end

-- TODO: Implement for non-ASCII.
function M.utf8_to_utf32(c)
	local h = string.match(c, '^([\x01-\x7F])$')
	if h then
		return string.byte(h, 1)
	end

	assert(false)
end

return M
