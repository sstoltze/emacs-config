-- Lock screen handler
local awful = require("awful")
local wibox = require("wibox")

local lock = {}

lock.automatic = false

lock.widget = wibox.widget.textbox()
lock.enabled_icon = "🔐"
lock.disabled_icon = "🔓"

lock.update_widget = function ()
   if lock.automatic then
      lock.widget.text = lock.enabled_icon
   else
      lock.widget.text = lock.disabled_icon
   end
end

awful.spawn.easy_async_with_shell("pgrep xautolock", function(stdout, stderr, reason, exit_code)
                                     if exit_code == 0 then
                                        lock.automatic = true
                                     end
                                     lock.update_widget()
end)

lock.start_autolock = function ()
   -- 'screenlock' is a fish function, so we need to get fish to evaluate it instead of the /bin/sh run by awful
   awful.spawn.with_shell("pgrep xautolock; or xautolock -time 10 -locker \"fish -c screenlock\" -nowlocker \"fish -c screenlock\"")
end

lock.enable_automatic_lock = function ()
   lock.start_autolock()
   awful.spawn.with_shell("xautolock -enable")
   lock.automatic = true
   lock.update_widget()
end

lock.disable_automatic_lock = function ()
   lock.start_autolock()
   awful.spawn.with_shell("xautolock -disable")
   lock.automatic = false
   lock.update_widget()
end

lock.toggle_automatic_lock = function ()
   if lock.automatic then
      lock.disable_automatic_lock()
   else
      lock.enable_automatic_lock()
   end
end

lock.lock_screen = function ()
   awful.spawn.with_shell("pgrep xautolock; and xautolock -locknow; or screenlock")
end

return lock
