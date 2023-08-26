local M = require('heawm')
local Rect = require('heawm.rect')

local ASCII_LOWER = 'abcdefghijklmnopqrstuvwxyz'
M.config.label_chars = ASCII_LOWER
M.config.base_weight = 4

local function grid_next(self)
	local net_width = self.geom.width - (self.cols - 1) * self.gap
	local net_height = self.geom.height - (self.rows - 1) * self.gap

	local wx = math.floor(net_width / self.cols * self.x)
	local ww = math.floor(net_width / self.cols * (self.x + 1)) - wx
	local wy = math.floor(net_height / self.rows * self.y)
	local wh = math.floor(net_height / self.rows * (self.y + 1)) - wy

	local tile = {
		x = self.geom.x + wx + self.x * self.gap,
		y = self.geom.y + wy + self.y * self.gap,
		width = ww,
		height = wh,
	}

	self.x = self.x + 1
	if self.x >= self.cols then
		self.y, self.x = self.y + 1, 0
		if self.y == self.rows - 1 then
			self.cols = self.total - self.cols * self.y
		end
	end

	return tile
end

local function grid(geom, total, opts)
	-- (1) r * c = total
	-- (2) r / c = h / w
	--
	-- (h / w * c) * c = total
	-- c = sqrt(total / h * w)
	local cols = opts.cols
		or math.max(
			1,
			math.min(
				math.floor(math.sqrt(total / geom.height * geom.width) + 0.5),
				total
			)
		)

	return grid_next,
		{
			geom = geom,
			cols = cols,
			rows = math.ceil(total / cols),
			total = total,
			gap = opts.gap,
			x = 0,
			y = 0,
		}
end

local function cascade_next(self)
	local tile = {
		x = self.geom.x + self.step * self.index,
		y = self.geom.y + self.step * self.index,
		width = self.geom.width - self.step * (self.total - 1),
		height = self.geom.height - self.step * (self.total - 1),
	}
	self.index = self.index + 1
	return tile
end

local function cascade(geom, total)
	return cascade_next,
		{
			geom = geom,
			step = math.min(math.min(geom.width, geom.height) / total, 75),
			index = 0,
			total = total,
		}
end

function M:setup_window_layout(window)
	window.layout = {
		gravity = 0,
		weight = self.config.base_weight,
	}
end

local function by_gravity(a, b)
	if a.layout.gravity ~= b.layout.gravity then
		return a.layout.gravity > b.layout.gravity
	end
	return a.index < b.index
end

function M:update_tree_layout(tree)
	tree.should_update_layout = false

	local output = tree.outputs[1]
	if not output then
		return
	end
	local geom = output.geom

	local windows = {}
	local console

	for _, window in ipairs(tree.window_stack) do
		if not window.console then
			table.insert(windows, window)
		else
			console = window
		end
	end

	if console then
		console.callsign = nil
		local window = console
		if window.focused then
			self:apply_window_layout_console(window, geom)
		else
			self:set_window_mapped(window, false)
		end
	end

	for i, window in ipairs(windows) do
		window.index = i
		window.callsign = string.sub(self.config.label_chars, i, i)
	end
	for _, window in ipairs(windows) do
		self:set_label_text(window.label, window.callsign)
	end

	if tree.layout == 'zoom' then
		local house = {}

		for _, window in ipairs(windows) do
			if window.focused then
				table.insert(house, window)
			else
				self:set_window_mapped(window, false)
			end
		end

		local gap = self:window_gap(true)
		local next_tile, next_tile_state = grid(geom, #house, {
			gap = gap,
		})

		for _, window in ipairs(house) do
			self:apply_window_layout_fullscreen(window, next_tile(next_tile_state))
		end

		return
	end

	table.sort(windows, by_gravity)

	local houses = {}

	for _, window in ipairs(windows) do
		local last_house = houses[#houses]
		if last_house and last_house[1].layout.gravity == window.layout.gravity then
			table.insert(last_house, window)
		else
			local new_house = { window }
			table.insert(houses, new_house)
		end
	end

	local weight_x, weight_y = self.config.base_weight, self.config.base_weight
	local count_weight_x, count_weight_y = 1, 1
	local min_weight_x, min_weight_y = weight_x - 1, weight_y - 1

	for _, house in ipairs(houses) do
		local window = house[1]
		local layout = window.layout

		if layout.gravity % 2 == 1 then
			count_weight_x = count_weight_x + 1
			min_weight_x = math.min(min_weight_x, layout.weight - 1)
			weight_x = weight_x + layout.weight
		elseif layout.gravity ~= 0 then
			count_weight_y = count_weight_y + 1
			min_weight_y = math.min(min_weight_y, layout.weight - 1)
			weight_y = weight_y + layout.weight
		end
	end

	weight_x = weight_x - count_weight_x * min_weight_x
	weight_y = weight_y - count_weight_y * min_weight_y

	local unit_x = geom.width / weight_x
	local unit_y = geom.height / weight_y

	for i, house in ipairs(houses) do
		local window = house[1]
		local layout = window.layout

		local house_geom
		if layout.gravity == 0 then
			house_geom = geom
		elseif layout.gravity % 4 == 1 then
			geom, house_geom =
				Rect.xsplit(geom, geom.width - unit_x * (layout.weight - min_weight_x))
		elseif layout.gravity % 4 == 3 then
			house_geom, geom =
				Rect.xsplit(geom, unit_x * (layout.weight - min_weight_x))
		elseif layout.gravity % 4 == 0 then
			house_geom, geom =
				Rect.ysplit(geom, unit_y * (layout.weight - min_weight_y))
		elseif layout.gravity % 4 == 2 then
			geom, house_geom =
				Rect.ysplit(geom, geom.height - unit_y * (layout.weight - min_weight_y))
		end

		local compact = #windows <= 1 or (layout.gravity ~= 0 and #house <= 1)
		local gap, outer_gap = self:window_gap(compact)
		house_geom = Rect.shrink(house_geom, outer_gap)

		local next_tile, next_tile_state
		if tree.layout == 'grid' then
			next_tile, next_tile_state = grid(house_geom, #house, {
				gap = gap,
			})
		elseif tree.layout == 'cascade' then
			next_tile, next_tile_state = cascade(house_geom, #house)
		else
			assert(false)
		end

		for _, window in ipairs(house) do
			self:apply_window_layout_tiled(window, next_tile(next_tile_state))
		end
	end
end

function M:reset_window_weight(window)
	self:set_window_weight(window, self.config.base_weight)
end

function M:change_window_weight(window, weight)
	self:set_window_weight(window, window.layout.weight + weight)
end

function M:set_window_weight(window, weight)
	local weight = math.min(math.max(0, weight), self.config.base_weight * 2)

	if window.layout.weight == weight then
		return
	end

	local gravity = window.layout.gravity

	for _, window in ipairs(window.tree.window_stack) do
		if window.layout.gravity == gravity then
			window.layout.weight = weight
		end
	end

	self:dirty_tree_layout(window.tree)
end

function M:reset_window_gravity(window)
	if window.layout.gravity == 0 then
		return
	end

	window.layout.gravity = 0

	self:dirty_tree_layout(window.tree)
end

local function is_only_window_in_house(window)
	local tree = window.tree
	for _, w in ipairs(tree.window_stack) do
		if
			w ~= window
			and math.floor((w.layout.gravity + 3) / 4)
				== math.floor((window.layout.gravity + 3) / 4)
		then
			return false
		end
	end
	return true
end

local function normalize_gravity(tree)
	local map = { [0] = true }
	for _, window in ipairs(tree.window_stack) do
		map[math.floor(window.layout.gravity / 4)] = true
	end

	local indices = {}
	for index in pairs(map) do
		table.insert(indices, index)
	end
	table.sort(indices)

	for new_index, index in ipairs(indices) do
		map[index] = (new_index - 1) * 4
	end

	for _, window in ipairs(tree.window_stack) do
		local layout = window.layout
		layout.gravity = map[math.floor(layout.gravity / 4)] + layout.gravity % 4
	end
end

function M:change_window_gravity(window, into_direction)
	local gravity = ({
		right = 1,
		down = 2,
		left = 3,
		up = 4,
	})[into_direction]

	local layout = window.layout

	if layout.gravity == 0 or not is_only_window_in_house(window) then
		for _, window in ipairs(window.tree.window_stack) do
			local layout = window.layout
			layout.gravity = 2 * math.floor((layout.gravity + 3) / 4) * 4
				+ layout.gravity % 4
		end
	end

	local max_gravity = 0
	for _, window in ipairs(window.tree.window_stack) do
		max_gravity = math.max(max_gravity, window.layout.gravity)
	end

	local original_geom = {}
	Rect.assign(original_geom, window.geom)

	repeat
		if layout.gravity == 0 or layout.gravity % 2 ~= gravity % 2 then
			layout.gravity = gravity
		elseif layout.gravity % 4 == gravity % 4 then
			layout.gravity = layout.gravity + 4
		elseif layout.gravity >= 4 then
			layout.gravity = layout.gravity - 4
		else
			layout.gravity = 0
		end

		self:update_tree_layout(window.tree)
	until layout.gravity >= max_gravity
		or not Rect.eq(original_geom, window.geom)

	normalize_gravity(window.tree)
end

function M:toggle_tree_layout(tree, layout)
	self:set_tree_layout(
		tree,
		tree.layout == layout and tree.prev_layout or layout
	)
end

function M:set_tree_layout(tree, layout)
	tree.prev_layout, tree.layout = tree.layout, layout
	self:dirty_tree_layout(tree)
end

function M:reset_tree_windows_layout(tree, opts)
	for _, window in ipairs(tree.window_stack) do
		if opts.gravity then
			self:reset_window_gravity(window)
		end

		if opts.weight then
			self:reset_window_weight(window)
		end
	end
end
