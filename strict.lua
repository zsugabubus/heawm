local function where()
	local info = debug.getinfo(3)
	return string.format('%s:%d', info.short_src, info.currentline)
end

setmetatable(_G, {
	__index = function(global, k)
		print(string.format("%s: attempt to index global '%s'", where(), k))
		return rawget(global, k)
	end,
	__newindex = function(_, k)
		print(string.format("%s: attempt to newindex global '%s'", where(), k))
	end,
})
