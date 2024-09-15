local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")

local volume = {}

volume.textbox = wibox.widget.textbox()

volume.command = [[wpctl get-volume @DEFAULT_SINK@ | awk '
              BEGIN     { volume = 0; muted = ""; }
              /^Volume/ { volume = ($2 * 100);
                          if ($3 == "[MUTED]")
                            muted = "M";
                          else
                            muted = "%"; }
              END       { print volume muted; }
']]

volume.update = function ()
   awful.spawn.easy_async_with_shell(volume.command, function(vol, stderr, reason, exit_code)
                                        if vol == "0" then
                                           vol = "off"
                                        end
                                        volume.textbox:set_markup("Vol: " .. vol)
   end)
end

volume.update()

volume.async = function (stdout, stderr, reason, exit_code)
   volume.update()
end

volume.lower = function ()
   awful.spawn.easy_async_with_shell("wpctl set-volume @DEFAULT_SINK@ 5%-")
end

volume.raise = function ()
   awful.spawn.easy_async_with_shell("wpctl set-volume @DEFAULT_SINK@ 5%+")
end

volume.mute = function ()
   awful.spawn.easy_async_with_shell("wpctl set-mute @DEFAULT_SINK@ toggle")
end

-- Media controls
volume.media = {}

volume.media.track_tooltip = awful.tooltip({ objects = { volume.textbox }, })

volume.media.get_track_command = [[dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player string:Metadata | awk '
BEGIN                     { album = ""; artist = ""; title = ""; }
titleSeen && !--titleSeen { split($0, words, "\""); title = words[2]; }
albumSeen && !--albumSeen { split($0, words, "\""); album = words[2]; }
artistSeen && /string/    { split($0, words, "\""); artist = artist ", " words[2]; }
/"xesam:title"/           { titleSeen  = 1; }
/"xesam:album"/           { albumSeen  = 1; }
/"xesam:artist"/          { artistSeen = 1; }
/]/                       { artistSeen = 0; }
END                       { print title " - " substr(artist, 3) " - " album; }']]

volume.media.update_track = function ()
   awful.spawn.easy_async_with_shell(volume.media.get_track_command,
                                     function (track, stderr, reason, exit_code)
                                        volume.media.track_tooltip:set_text(track)
   end)
end

volume.media.update_track()

volume.media.update_track_async = function (stdout, stderr, reason, exit_code)
   volume.media.update_track()
end

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
   awful.spawn.easy_async_with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous", volume.media.update_track_async)
end

volume.media.next = function ()
   awful.spawn.easy_async_with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next", volume.media.update_track_async)
end

-- Bluetooth
volume.bluetooth = {}
volume.bluetooth.command = [[pactl list cards | awk '
BEGIN              { card = "no"; profile = "off"; }
/^Card/            { gsub(/[^0-9]/, "", $2); card = $2; }
/^[^a-zA-Z]Active/ { profile = $3; }
END                { print card " " profile }']]
volume.bluetooth.change_profile = function ()
   awful.spawn.with_shell(volume.bluetooth.command,
                          function (output, stderr, reason, exit_code)
                             local profile, card, new_profile
                             -- Default to a2dp_sink
                             new_profile = "a2dp_sink"
                             card, profile = string.match(output, "(%d+) ([^%s]+)")
                             -- If already a2dp_sink, switch to headset
                             if profile:gsub("%s+", "") == "a2dp_sink" then
                                new_profile = "handsfree_head_unit"
                             end
                             naughty.notify({text = "Bluetooth: " .. new_profile})
                             -- Switch to correct profile
                             awful.spawn.with_shell("pactl set-card-profile " .. card .. " " .. new_profile)
   end)
end

volume.timer = timer({timeout = 13})
volume.timer:connect_signal("timeout", function ()
                               volume.update()
                               volume.media.update_track()
end)
volume.timer:start()

return volume
