local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")
local vicious = require("vicious")

function make_battery_widget(theme)
   local battery = {}
   battery.widget = wibox.widget.textbox()
   battery.low_level = false
   battery.notify = function (level)
      if not battery.low_level and level < 20 then
         battery.low_level = true
         naughty.notify({preset = naughty.config.presets.critical,
                         text = "Low battery"})
      end
      if level > 20 then
         battery.low_level = false
      end
   end
   battery.tooltip = awful.tooltip({ objects = { battery.widget}, })
   vicious.register(battery.widget, vicious.widgets.bat,
                    function (widgets, args)
                       local urgent = false
                       -- If discharging battery and time is less than 30 minutes or 20% battery remaining, text is red
                       if args[1] == "-" then
                          if args[2] < 20 then
                             urgent = true
                          else
                             for h,m in string.gmatch(args[3],"(%d+):(%d+)") do
                                if tonumber(h) == 0 and tonumber(m) < 30 then
                                   urgent = true
                                end
                             end
                          end
                       end
                       battery.tooltip:set_text( (args[1] == "-" and "Time left: " or ("Charging done in: ")) .. args[3])
                       battery.notify(args[2])
                       return string.format("Bat: <span %s>%2d%s</span>",
                                            -- If urgent, set urgent colours
                                            urgent and "fgcolor='" .. theme.fg_urgent .. "' bgcolor='" .. theme.bg_urgent .."'" or "",
                                            args[2],
                                            args[1] == "-" and "%" or "+")
                    end, 61, "BAT0")
   return battery
end

return make_battery_widget
