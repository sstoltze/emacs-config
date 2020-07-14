local wibox = require("wibox")
local vicious = require("vicious")

local memory = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memory, vicious.widgets.mem, "RAM: $2/$3", 71)

return memory
