local naughty = require("naughty")
local xrandr = require("xrandr")

local brightness = {}
brightness.brightness = 0.6
brightness.set = function (b)
   brightness.brightness = b
   local out = xrandr.outputs()
   for _, o in pairs(out) do
      os.execute("xrandr --output " .. o .. " --brightness " .. b)
   end
end

brightness.increase = function ()
   local b = brightness.brightness
   if b <= 0.9 then
      b = b + 0.1
   end
   naughty.notify({text = "Brightness " .. b})
   brightness.set(b)
end

brightness.decrease = function ()
   local b = brightness.brightness
   if b >= 0.1 then
      b = b - 0.1
   end
   naughty.notify({text = "Brightness " .. b})
   brightness.set(b)
end

brightness.set(brightness.brightness)

return brightness
