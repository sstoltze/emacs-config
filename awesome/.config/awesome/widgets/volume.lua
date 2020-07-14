local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")

local volume = {}
volume.textbox = wibox.widget.textbox()
volume.command = [[pactl list sinks | awk '
BEGIN               { mute = "no"; running = 0; }
/RUNNING/           { running = 1; }
/^[^a-zA-Z]Mute/    { if ( running ) { mute = $2 } }
/^[^a-zA-Z]*Volume/ { if ( running ) { if ( mute ~ /yes/ ) { gsub(/%/, "M", $5); }; print $5; } }
/SUSPENDED/         { running = 0; }']]

volume.update = function ()
      awful.spawn.easy_async_with_shell(volume.command, function(vol, stderr, reason, exit_code)
                                        if vol == "" then
                                           vol = "off"
                                        end
                                        volume.textbox:set_markup("Vol: " .. vol)
   end)
end

volume.update()
volume.timer = timer({timeout = 13})
volume.timer:connect_signal("timeout", volume.update)
volume.timer:start()

volume.get_sink = "pactl list short sinks | grep -i running | cut -f 1"

volume.async = function (stdout, stderr, reason, exit_code)
   volume.update()
end

volume.lower = function ()
   awful.spawn.easy_async_with_shell(volume.get_sink,
                                     function(sink, stderr, reason, exit_code)
                                        awful.spawn.easy_async("pactl set-sink-volume " .. sink .. " -5%", volume.async)
   end)
end

volume.raise = function ()
   awful.spawn.easy_async_with_shell(volume.get_sink,
                                     function(sink, stderr, reason, exit_code)
                                        awful.spawn.easy_async("pactl set-sink-volume " .. sink .. " +5%", volume.async)
   end)
end

volume.mute = function ()
   awful.spawn.easy_async_with_shell(volume.get_sink,
                                     function(sink, stderr, reason, exit_code)
                                        awful.spawn.easy_async("pactl set-sink-mute " .. sink .. " toggle", volume.async)
   end)
end

-- Media controls
volume.media = {}
volume.media.playpause = function ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
end

volume.media.play = function ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Play")
end

volume.media.pause = function ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause")
end

volume.media.prev = function ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
end

volume.media.next = function ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
end

-- Bluetooth
volume.bluetooth = {}
volume.bluetooth.command = [[pactl list cards | awk '
BEGIN               { card = "no"; profile = "off"; }
/^Card/             { gsub(/[^0-9]/, "", $2); card = $2; }
/^[^a-zA-Z]Active/  { profile = $3; }
END                 { print card " " profile }']]
volume.bluetooth.change_profile = function ()
   awful.spawn.easy_async_with_shell(volume.bluetooth.command,
                                     function (output, stderr, reason, exit_code)
                                        local profile, card, new_profile
                                        -- Default to a2dp_sink
                                        new_profile = "a2dp_sink"
                                        card, profile = string.match(output, "(%d+) ([^%s]+)")
                                        -- If already a2dp_sink, switch to headset
                                        if profile:gsub("%s+", "") == "a2dp_sink" then
                                           new_profile = "headset_head_unit"
                                        end
                                        naughty.notify({text = "Bluetooth: " .. new_profile})
                                        -- Switch to correct profile
                                        awful.spawn.with_shell("pactl set-card-profile " .. card .. " " .. new_profile)
   end)
end

return volume
