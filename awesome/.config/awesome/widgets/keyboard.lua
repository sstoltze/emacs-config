-- Keyboard map indicator and switcher
local awful = require("awful")

local keyboard = {}

keyboard.layouts = { { "dk", "" }, { "us", "" } }

keyboard.widget = awful.widget.keyboardlayout()

keyboard.current_kb_layout = 0 -- Use first entry as default

keyboard.change_layout = function ()
   keyboard.current_kb_layout = keyboard.current_kb_layout % #(keyboard.layouts) + 1
   local layout = keyboard.layouts[keyboard.current_kb_layout]
   awful.spawn("setxkbmap " .. layout[1] .. " " .. layout[2])
end
keyboard.change_layout()

return keyboard
