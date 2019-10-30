-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local vicious = require("vicious") -- Install awesome-extra
local hotkeys_popup = require("awful.hotkeys_popup").widget
local xrandr = require("xrandr")

-- Load Debian menu entries
require("debian.menu")

-- Theme
local theme_name = "orange" -- "orange" "ww" "xresources"
local theme_dir = awful.util.getdir("config") .. "/themes/" .. theme_name .. "/theme.lua"

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- Make sure we don't go into an endless error loop
                             if in_error then return end
                             in_error = true

                             naughty.notify({ preset = naughty.config.presets.critical,
                                              title = "Oops, an error happened!",
                                              text = tostring(err) })
                             in_error = false
   end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(theme_dir)

-- For use in widgets
local theme = beautiful.get()

-- This is used later as the default terminal and editor to run.
--terminal = "x-terminal-emulator"
local terminal = "kitty"
local editor = "emacs -nw" or os.getenv("EDITOR") or "editor"
local editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
local modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier,
   awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
   local instance = nil

   return function ()
      if instance and instance.wibox.visible then
         instance:hide()
         instance = nil
      else
         instance = awful.menu.clients({ theme = { width = 250 } })
      end
   end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
local myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

local function shutdown_fn()
   awful.spawn.with_shell("shutdown now")
end

local mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                   { "Debian", debian.menu.Debian_menu.Debian },
                                   { "open terminal", terminal },
                                   { "shutdown", shutdown_fn }
}
                             })

-- Not used at the moment
-- mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
--                                      menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
local mytextclock = wibox.widget.textclock()
local calendar = awful.tooltip({ objects = { mytextclock }, })

-- See https://awesomewm.org/doc/api/libraries/awful.spawn.html
awful.spawn.easy_async("ncal -bM", function(stdout, stderr, reason, exit_code)
                          cal_text = stdout:gsub("%p%c([%d%s])%p%c(%d)",
                                                 '<span underline="single" background="' .. theme.bg_widget .. '" foreground="' .. theme.fg_widget .. '">%1%2</span>') -- Erstat bold i terminalen med underline og baggrundsfarve
                             :gsub("[%c%s]+$", " ") -- Fjern alt overskydende whitespace og ekstra linier
                          :gsub("%s%s%c", " \n ") -- Lidt dumt, men outputtet er for langt på nogle linier og tomme strenge har en grim baggrundsfarve
                          calendar:set_markup('<tt><span background="' .. theme.bg_normal .. '"> ' -- Monospace og rigtig baggrundsfarve
                                                 .. cal_text
                                                 .. string.rep(" ", 58 + (select(2, cal_text:gsub('\n', '\n'))+1)*22 - cal_text:len()) -- Længde (7*22) + <spans> og lign. (58, åbenbart). Dette går nok hurtigt i stykker igen
                                                 .. "</span></tt>")
end)

-- Volume
local tbvolume = wibox.widget.textbox()
local vol_command = [[pactl list sinks | awk '
BEGIN               { mute = "no"; running = 0; }
/RUNNING/           { running = 1; }
/^[^a-zA-Z]Mute/    { if ( running ) { mute = $2 } }
/^[^a-zA-Z]*Volume/ { if ( running ) { if ( mute ~ /yes/ ) { gsub(/%/, "M", $5); }; print $5; } }
/SUSPENDED/         { running = 0; }']]

function updatevolume()
   awful.spawn.easy_async_with_shell(vol_command, function(vol, stderr, reason, exit_code)
                                        tbvolume:set_markup("Vol: " .. vol)
                                                  end
   )
end

updatevolume()
local volumetimer = timer({timeout = 13})
volumetimer:connect_signal("timeout", updatevolume)
volumetimer:start()

-- Spotify notifications
naughty.config.presets.spotify = {
   -- if you want to disable Spotify notifications completely, return false
   callback = function(args)
      return true
   end,

   -- Adjust the size of the notification
   height = 50,
   width  = 300,
   -- Guessing the value, find a way to fit it to the proper size later
   icon_size = 40
}
table.insert(naughty.dbus.config.mapping, {{appname = "Spotify"}, naughty.config.presets.spotify})

-- CPU widget
-- Initialize widget
local cpuwidget = wibox.widget.textbox()
-- Register widget
vicious.register(cpuwidget, vicious.widgets.cpu, "CPU: $1%")

cpuwidget:buttons(awful.util.table.join(
                     awful.button({ }, 1,
                        function ()
                           awful.util.spawn(terminal .. " -e top")
end)))

-- RAM usage widget
local memwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "RAM: $2/$3", 71)
--update every 71 seconds

local divider = wibox.widget.textbox() -- center
divider:set_text(" | ")

local bat = wibox.widget.textbox() -- center
local bat_t = awful.tooltip({ objects = { bat }, })
vicious.register(bat, vicious.widgets.bat,
                 function (widgets, args)
                    local f = io.popen("acpi -V | head -1 | cut -d ' ' -f 5")
                    local l = "Error"
                    local fg_colour = theme.fg_normal
                    local bg_colour = theme.bg_normal
                    if f ~= nil then
                       l = f:read()
                    end
                    f:close()
                    for h,m in string.gmatch(l,"(%d+):(%d+)") do
                       -- If discharging battery and time is less than 30 minutes or 20% battery remaining, text is red
                       if args[1] == "-" and (tonumber(h) == 0 and tonumber(m) < 30 or args[2] < 20) then
                          fg_colour = theme.fg_urgent
                          bg_colour = theme.bg_urgent
                       end
                    end
                    bat_t:set_text( (args[1] == "-" and "Time left: " or ("Charging done in: ")) .. l)
                    return string.format("Bat: <span fgcolor='%s' bgcolor='%s'>%2d%s</span>", fg_colour, bg_colour, args[2], args[1] == "-" and "%" or "+")
                 end, 61, "BAT0")

-- Media controls
local get_sink = "pactl list short sinks | grep -i running | cut -f 1"

local async_volume = function (stdout, stderr, reason, exit_code)
   updatevolume()
end
local function lowervolume()
   awful.spawn.easy_async_with_shell(get_sink,
                                     function(sink, stderr, reason, exit_code)
                                        awful.spawn.easy_async("pactl set-sink-volume " .. sink .. " -5%", async_volume)
   end)
end

local function raisevolume ()
   awful.spawn.easy_async_with_shell(get_sink,
                                     function(sink, stderr, reason, exit_code)
                                        awful.spawn.easy_async("pactl set-sink-volume " .. sink .. " +5%", async_volume)
   end)
end

local function togglemute ()
   awful.spawn.easy_async_with_shell(get_sink,
                                     function(sink, stderr, reason, exit_code)
                                        awful.spawn.easy_async("pactl set-sink-mute " .. sink .. " toggle", async_volume)
   end)
end

local function playpause ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
end

local function playmedia ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Play")
end

local function pausemedia ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause")
end

local function playprev ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
end

local function playnext ()
   awful.spawn.with_shell("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
end

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- Without this, the following
            -- :isvisible() makes no sense
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- This will also un-minimize
            -- the client, if needed
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, client_menu_toggle_fn()),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

-- Set all wallpapers at once
local function set_wallpaper(s)
   awful.spawn.with_shell("feh --bg-fill --randomize ~/.local/wallpapers/*")
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- Set wallpaper
set_wallpaper(true)

awful.screen.connect_for_each_screen(function(s)
      -- Each screen has its own tag table.
      awful.tag({ "Main", "Net", "Music", "Social", "Math", "Video", "Steam", 8, "VPN"  }, s, awful.layout.layouts[3])

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contains an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(awful.util.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

      -- Create a tasklist widget
      s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            --mylauncher, -- not needed IMO, kept here if I want it back
            s.mytaglist,
            s.mypromptbox,
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            divider,
            wibox.widget.systray(),
            divider,
            tbvolume,
            divider,
            bat,
            divider,
            cpuwidget,
            divider,
            memwidget,
            divider,
            mytextclock,
            s.mylayoutbox,
         },
      }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
local globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
      {description = "view next", group = "tag"}),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

   awful.key({ modkey,           }, "j",
      function ()
         awful.client.focus.byidx( 1)
      end,
      {description = "focus next by index", group = "client"}
   ),
   awful.key({ modkey,           }, "k",
      function ()
         awful.client.focus.byidx(-1)
      end,
      {description = "focus previous by index", group = "client"}
   ),
   awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
      {description = "swap with next client by index", group = "client"}),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
      {description = "swap with previous client by index", group = "client"}),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),
   awful.key({ modkey,           }, "Tab",
      function ()
         awful.client.focus.history.previous()
         if client.focus then
            client.focus:raise()
         end
      end,
      {description = "go back", group = "client"}),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
      {description = "increase master width factor", group = "layout"}),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
      {description = "decrease master width factor", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
      {description = "increase the number of master clients", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
      {description = "decrease the number of master clients", group = "layout"}),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
      {description = "increase the number of columns", group = "layout"}),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
      {description = "decrease the number of columns", group = "layout"}),
   awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
      {description = "select next", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            client.focus = c
            c:raise()
         end
      end,
      {description = "restore minimized", group = "client"}),

   -- Prompt
   awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
      {description = "run prompt", group = "launcher"}),

   awful.key({ modkey }, "x",
      function ()
         awful.prompt.run {
            prompt       = "Run Lua code: ",
            textbox      = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval"
         }
      end,
      {description = "lua execute prompt", group = "awesome"}),
   -- Menubar
   awful.key({ modkey }, "p", function() menubar.show() end,
      {description = "show the menubar", group = "launcher"}),

   -- Media controls
   awful.key({ }, "XF86AudioLowerVolume", lowervolume, {description = "volume down",   group = "audio"}),
   awful.key({ modkey }, "Down",          lowervolume, {description = "volume down",   group = "audio"}),
   awful.key({ modkey }, "æ",             lowervolume, {description = "volume down",   group = "audio"}),
   awful.key({ }, "XF86AudioRaiseVolume", raisevolume, {description = "volume up",     group = "audio"}),
   awful.key({ modkey }, "Up",            raisevolume, {description = "volume up",     group = "audio"}),
   awful.key({ modkey }, "ø",             raisevolume, {description = "volume up",     group = "audio"}),
   awful.key({ }, "XF86AudioMute",        togglemute,  {description = "volume mute",   group = "audio"}),
   awful.key({ modkey }, "'",             togglemute,  {description = "volume mute",   group = "audio"}),
   awful.key({ modkey }, ".",             playpause,   {description = "play/pause",    group = "audio"}),
   awful.key({ }, "XF86AudioPause",       pausemedia,  {description = "pause media",   group = "audio"}),
   awful.key({ }, "XF86AudioPlay",        playmedia,   {description = "play media",    group = "audio"}),
   awful.key({ modkey }, ",",             playprev,    {description = "play previous", group = "audio"}),
   awful.key({ }, "XF86AudioPrev",        playprev,    {description = "play previous", group = "audio"}),
   awful.key({ modkey }, "-",             playnext,    {description = "play next",     group = "audio"}),
   awful.key({ }, "XF86AudioNext",        playnext,    {description = "play next",     group = "audio"})
)

clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey,           }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {description = "maximize", group = "client"})
   ,
   awful.key({ modkey, "Control" }, "o", function () xrandr.xrandr() end, { description = "toggle xrandr options", group = "screen"}),
   awful.key({ modkey,                    }, "q", function () awful.spawn.with_shell("slock")  end, { description = "slock", group = "awesome" })
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = awful.util.table.join(globalkeys,
                                      -- View tag only.
                                      awful.key({ modkey }, "#" .. i + 9,
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               tag:view_only()
                                            end
                                         end,
                                         {description = "view tag #"..i, group = "tag"}),
                                      -- Toggle tag display.
                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               awful.tag.viewtoggle(tag)
                                            end
                                         end,
                                         {description = "toggle tag #" .. i, group = "tag"}),
                                      -- Move client to tag.
                                      awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:move_to_tag(tag)
                                               end
                                            end
                                         end,
                                         {description = "move focused client to tag #"..i, group = "tag"}),
                                      -- Toggle tag on focused client.
                                      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:toggle_tag(tag)
                                               end
                                            end
                                         end,
                                         {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    raise = true,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    screen = awful.screen.preferred,
                    placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
   },

   -- Floating clients.
   { rule_any = {
        instance = {
           "DTA",  -- Firefox addon DownThemAll.
           "copyq",  -- Includes session name in class.
        },
        class = {
           "Arandr",
           "Gpick",
           "Kruler",
           "MessageWin",  -- kalarm.
           "Sxiv",
           "Wpa_gui",
           "pinentry",
           "veromix",
           "xtightvncviewer"},

        name = {
           "Event Tester",  -- xev.
        },
        role = {
           "AlarmWindow",  -- Thunderbird's calendar.
           "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
   }, properties = { floating = true }},

   -- Add titlebars to normal clients and dialogs
   { rule_any = {type = { "normal", "dialog" }
                }, properties = { titlebars_enabled = false }
   },

   { rule = { class = "MPlayer" },
     properties = { floating = true } },
   { rule = { class = "mpv" },
     properties = { floating = true,
                    fullscreen = true,
                    screen = 1,
                    tag = "Video" } },
   { rule = { class = "pinentry" },
     properties = { floating = true } },
   { rule = { class = "Gimp" },
     properties = { floating = true } },
   { rule = { class = "Firefox" },
     properties = { maximized = true,
                    tag = "Net" } },
   { rule = { class = "Slack" },
     properties = { maximized = true,
                    tag = "Social" } },
   { rule = { class = "Emacs" },
     properties = { maximized = true } },
   { rule = { class = "Steam" },
     properties = { maximized = true,
                    tag = "Steam" } },
   -- { rule = { class = "Evince" },
   --   properties = { floating = true } },
   { rule = { class = "Gnome-calculator" },
     properties = { floating = true } },
   { rule = { class = "Code" },
     properties = { floating = true } },
   { rule = { class = "Skype" },
     properties = { maximized = 1,
                    tag = "Social" } },
   { rule = { class = "Totem" },
     properties = { maximized = true,
                    tag = "Video" } },
   { rule = { class = "Conkeror" },
     properties = { maximized = true,
                    tag = "Net" } },
   { rule = { class = "Dwarf_Fortress" },
     properties = { maximized = true } },
   { rule = { class = "[Ss]potify" },
     properties = { maximized = true,
                    tag = "Music" } },
   { rule = { instance = "xfi" }, --- xfimage
     --- class = "Xfe" also matches xfe
     properties = { maximized= true,
                    fullscreen = true } },
   { rule = { class = "Mathematica" },
     properties = { floating = true,
                    tag = "Math",
                    fullscreen = true } }

   -- Set Firefox to always map on the tag named "2" on screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { screen = 1, tag = "2" } },
}
client.connect_signal("manage",
                      function (c)

end)
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
                         -- Set the windows at the slave,
                         -- i.e. put it at the end of others instead of setting it master.
                         -- if not awesome.startup then awful.client.setslave(c) end

                         if awesome.startup and
                            not c.size_hints.user_position
                         and not c.size_hints.program_position then
                            -- Prevent clients from being unreachable after screen count changes.
                            awful.placement.no_offscreen(c)
                         end

                         -- Fix spotify spawning without a class by applying rules again after class is set
                         if c.class == nil then
                            c.minimized = true
                            c:connect_signal("property::class",
                                             function ()
                                                c.minimized = false
                                                awful.rules.apply(c)
                            end)
                         end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
                         -- buttons for the titlebar
                         local buttons = awful.util.table.join(
                            awful.button({ }, 1, function()
                                  client.focus = c
                                  c:raise()
                                  awful.mouse.client.move(c)
                            end),
                            awful.button({ }, 3, function()
                                  client.focus = c
                                  c:raise()
                                  awful.mouse.client.resize(c)
                            end)
                         )

                         awful.titlebar(c) : setup {
                            { -- Left
                               awful.titlebar.widget.iconwidget(c),
                               buttons = buttons,
                               layout  = wibox.layout.fixed.horizontal
                            },
                            { -- Middle
                               { -- Title
                                  align  = "center",
                                  widget = awful.titlebar.widget.titlewidget(c)
                               },
                               buttons = buttons,
                               layout  = wibox.layout.flex.horizontal
                            },
                            { -- Right
                               awful.titlebar.widget.floatingbutton (c),
                               awful.titlebar.widget.maximizedbutton(c),
                               awful.titlebar.widget.stickybutton   (c),
                               awful.titlebar.widget.ontopbutton    (c),
                               awful.titlebar.widget.closebutton    (c),
                               layout = wibox.layout.fixed.horizontal()
                            },
                            layout = wibox.layout.align.horizontal
                                                   }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
                         if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                         and awful.client.focus.filter(c) then
                            client.focus = c
                         end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

local function spawn_once_with_shell(prg, prg_opts)
   prg_opts = prg_opts or ""
   spawn_opts = spawn_opts or {}
   awful.spawn.with_shell("pgrep " .. prg .. " >/dev/null; or " .. prg .. " " .. prg_opts)
end

spawn_once_with_shell("nm-applet", "&")

-- Computer specific setup
awful.spawn.easy_async_with_shell('echo -n "$USER"', function(user, stderr, reason, exit_code)
                                     if user == "simon\n" then -- Laptop
                                        spawn_once_with_shell("xfce4-power-manager", "--no-daemon")
                                        spawn_once_with_shell("dropbox", "start")
                                     elseif user == "w26164\n" then -- Work setup
                                        spawn_once_with_shell("solaar")
                                        spawn_once_with_shell("blueman-applet", "&")
                                     end
end)
