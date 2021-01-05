local awful = require("awful")
local wibox = require("wibox")

local function themed_clock(theme)

   local clock = wibox.widget.textclock()
   local calendar = awful.tooltip({ objects = { clock }, })

   awful.spawn.easy_async("ncal -bM", function(stdout, stderr, reason, exit_code)
                             local cal_text = stdout:gsub("%p%c(%d)",
                                                          '<span underline="single" background="' .. theme.bg_widget
                                                             .. '" foreground="' .. theme.fg_widget
                                                             .. '">%1</span>') -- Erstat bold i terminalen med underline og baggrundsfarve
                                :gsub("%p%c%s", " ") -- Fjern bold i terminal fra whitespace
                                :gsub("[%c%s]+$", " ") -- Fjern alt overskydende whitespace og ekstra linier
                                :gsub("%s%s%c", " \n ") -- Lidt dumt, men outputtet er for langt på nogle linier og tomme strenge har en grim baggrundsfarve
                             calendar:set_markup('<tt><span background="' .. theme.bg_normal .. '"> ' -- Monospace og rigtig baggrundsfarve
                                                    .. cal_text
                                                    .. string.rep(" ", 58 + (select(2, cal_text:gsub('\n', '\n'))+1)*22 - cal_text:len()) -- Længde (7*22) + <spans> og lign. (58, åbenbart). Dette går nok hurtigt i stykker igen
                                                    .. "</span></tt>")
   end)
   return clock
end

return themed_clock
