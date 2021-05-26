local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")

local cpu = wibox.widget.textbox()

vicious.register(cpu, vicious.widgets.cpu, "CPU: $1%")

return cpu
