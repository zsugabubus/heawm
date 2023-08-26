local M = {}

function M:eq(other)
	return self.x == other.x
		and self.y == other.y
		and self.width == other.width
		and self.height == other.height
end

function M:assign(other)
	self.x = other.x
	self.y = other.y
	self.width = other.width
	self.height = other.height
end

function M:shrink(width)
	return {
		x = self.x + width,
		y = self.y + width,
		width = math.max(0, self.width - 2 * width),
		height = math.max(0, self.height - 2 * width),
	}
end

function M:xsplit(left_width)
	left_width = math.min(left_width, self.width)
	return {
		x = self.x,
		width = left_width,
		y = self.y,
		height = self.height,
	}, {
		x = self.x + left_width,
		width = self.width - left_width,
		y = self.y,
		height = self.height,
	}
end

function M:ysplit(top_height)
	top_height = math.min(top_height, self.height)
	return {
		x = self.x,
		y = self.y,
		width = self.width,
		height = top_height,
	}, {
		x = self.x,
		y = self.y + top_height,
		width = self.width,
		height = self.height - top_height,
	}
end

return M
