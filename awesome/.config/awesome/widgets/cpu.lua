local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")

local cpu = wibox.widget.textbox()

vicious.register(cpu, vicious.widgets.cpu, "CPU: $1%")

-- cpu:buttons(awful.util.table.join(
--                awful.button({ }, 1,
--                   function ()
--                      awful.util.spawn(terminal .. " -e top")
-- end)))

return cpu
