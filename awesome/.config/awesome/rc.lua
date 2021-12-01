-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")

-- Theme
local theme_name = "orange" -- "orange" "ww" "xresources"
local theme_dir = gears.filesystem.get_configuration_dir() .. "/themes/" .. theme_name .. "/theme.lua"

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
local terminal = "kitty --single-instance"
local editor = os.getenv("EDITOR") or "nano" or "emacs -nw"
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
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

local function shutdown_fn()
   awful.spawn.with_shell("shutdown now")
end

local mymainmenu = awful.menu({ items = {
                                   { "awesome", myawesomemenu, beautiful.awesome_icon },
                                   { "Debian", debian.menu.Debian_menu.Debian },
                                   { "open terminal", terminal },
                                   { "shutdown", shutdown_fn }
                             } })

-- Not used at the moment
-- mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
--                                      menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- This should make menubar show up quicker the first time it is run
menubar.menu_gen.lookup_category_icons = function() end
-- }}}


-- {{{ Wibar

-- Widgets
local xrandr = require("widgets.xrandr")

local kbdcfg = require("widgets.keyboard")

local make_clock = require("widgets.clock")
local clock = make_clock(theme)

local volume = require("widgets.volume")

local cpuwidget = require("widgets.cpu")

local memwidget = require("widgets.memory")

local divider = wibox.widget.textbox() -- center
divider:set_text(" | ")

local make_battery = require("widgets.battery")
local battery = make_battery(theme)

local brightness = require("widgets.brightness")

local lock = require("widgets.lock")

-- Notifications
-- Discord and Spotify
naughty.config.defaults.position = "top_middle"

naughty.config.presets.notifications = {
   -- if you want to disable Spotify notifications completely, return false
   callback = function(args)
      return true
   end,

   -- Adjust the size of the notification
   -- height = 100,
   -- width  = 300,
   icon_size = 100,
}

table.insert(naughty.dbus.config.mapping, {{appname = "Spotify"}, naughty.config.presets.notifications})
table.insert(naughty.dbus.config.mapping, {{appname = "discord"}, naughty.config.presets.notifications})


-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
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

local tasklist_buttons = gears.table.join(
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


local function set_wallpaper(s)
   awful.spawn.with_shell("feh --bg-fill --randomize ~/.local/wallpapers/*")
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- Set wallpaper for all screens at once
set_wallpaper(true)

awful.screen.connect_for_each_screen(function(s)
      -- Each screen has its own tag table.
      awful.tag({ "Main", "Net", "Social", "Music", "Math", "Video", "Steam", 8, "VPN"  }, s, awful.layout.layouts[6])

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contains an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist {
         screen = s,
         filter = awful.widget.taglist.filter.all,
         buttons = taglist_buttons
      }

      -- Create a tasklist widget
      s.mytasklist = awful.widget.tasklist {
         screen = s,
         filter = awful.widget.tasklist.filter.currenttags,
         buttons = tasklist_buttons
      }

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            --mylauncher, -- not needed IMO, kept here if I want it back
            s.mytaglist,
            divider,
            s.mypromptbox,
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            divider,
            kbdcfg.widget,
            divider,
            wibox.widget.systray(),
            lock.widget,
            divider,
            volume.textbox,
            divider,
            battery.widget,
            divider,
            cpuwidget,
            divider,
            memwidget,
            divider,
            clock,
            s.mylayoutbox,
         },
      }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
local globalkeys = gears.table.join(
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
   -- Never used, only leads to trouble
   -- awful.key({ modkey, "Shift"   }, "q", awesome.quit,
   --    {description = "quit awesome", group = "awesome"}),

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
   awful.key({ modkey, "Control" }, "space", function () awful.layout.inc( 1)                end,
      {description = "select next", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            c:emit_signal(
               "request::activate", "key.unminimize", {raise = true}
            )
         end
      end,
      {description = "restore minimized", group = "client"}),

   -- Prompt
   awful.key({ modkey            }, "r", function () awful.screen.focused().mypromptbox:run() end,
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

   -- Keyboard
   awful.key({ modkey }, "space", kbdcfg.change_layout,
      {description = "change keyboard layout", group = "keyboard"}),

   -- Volume/media controls
   awful.key({ }, "XF86AudioLowerVolume", volume.lower,                    {description = "volume down",              group = "audio"}),
   awful.key({ modkey }, "æ",             volume.lower,                    {description = "volume down",              group = "audio"}),
   awful.key({ modkey }, ";",             volume.lower,                    {description = "volume down",              group = "audio"}),
   awful.key({ }, "XF86AudioRaiseVolume", volume.raise,                    {description = "volume up",                group = "audio"}),
   awful.key({ modkey }, "ø",             volume.raise,                    {description = "volume up",                group = "audio"}),
   awful.key({ modkey }, "'",             volume.raise,                    {description = "volume up",                group = "audio"}),
   awful.key({ }, "XF86AudioMute",        volume.mute,                     {description = "volume mute",              group = "audio"}),
   awful.key({ modkey }, "å",             volume.mute,                     {description = "volume mute",              group = "audio"}),
   awful.key({ modkey }, "[",             volume.mute,                     {description = "volume mute",              group = "audio"}),
   awful.key({ }, "XF86AudioPause",       volume.media.pause,              {description = "pause media",              group = "audio"}),
   awful.key({ }, "XF86AudioPlay",        volume.media.play,               {description = "play media",               group = "audio"}),
   awful.key({ modkey }, ".",             volume.media.playpause,          {description = "play/pause",               group = "audio"}),
   awful.key({ }, "XF86AudioPrev",        volume.media.prev,               {description = "play previous",            group = "audio"}),
   awful.key({ modkey }, ",",             volume.media.prev,               {description = "play previous",            group = "audio"}),
   awful.key({ }, "XF86AudioNext",        volume.media.next,               {description = "play next",                group = "audio"}),
   awful.key({ modkey }, "-",             volume.media.next,               {description = "play next",                group = "audio"}),
   awful.key({ modkey }, "/",             volume.media.next,               {description = "play next",                group = "audio"}),
   awful.key({ modkey }, "+",             volume.bluetooth.change_profile, {description = "change bluetooth profile", group = "audio"}),

   -- Brightness
   awful.key({ modkey }, "Down",          brightness.decrease,             {description = "brightness down",   group = "screen"}),
   awful.key({ modkey }, "Up",            brightness.increase,             {description = "brightness up",     group = "screen"})
)

local prev_gap = 10
if theme.useless_gap > 0 then
   prev_gap = theme.useless_gap
end

function toggle_gap ()
   if theme.useless_gap > 0 then
      prev_gap = theme.useless_gap
      theme.useless_gap = 0
   else
      theme.useless_gap = prev_gap
   end
   -- Used to get the screen to update. This should be easier...
   awful.tag.incmwfact( 0.05)
   awful.tag.incmwfact(-0.05)
end

function increase_gap()
   theme.useless_gap = theme.useless_gap + 5
   -- Used to get the screen to update. This should be easier...
   awful.tag.incmwfact( 0.05)
   awful.tag.incmwfact(-0.05)
end

function decrease_gap()
   theme.useless_gap = theme.useless_gap - 5
   -- Used to get the screen to update. This should be easier...
   awful.tag.incmwfact( 0.05)
   awful.tag.incmwfact(-0.05)
end

-- https://thibaultmarin.github.io/blog/posts/2016-10-05-Awesome-wm_configuration.html
-- {{{ Highlight current monitor
screen_highlight_timer = timer({timeout = 0.2})
screen_highlight_idx = 1
screen_highlight_timer:connect_signal(
   "timeout",
   function ()
      if awful.screen.focused() ~= screen_highlight_idx then
         screen_highlight_idx = awful.screen.focused()
         for s in screen do
            if s == awful.screen.focused() then
               col_bg = beautiful.bg_normal
               col_fg = beautiful.fg_normal
            else
               col_bg = beautiful.bg_inactive
               col_fg = beautiful.fg_inactive
            end
            s.mywibox.bg = col_bg
            s.mywibox.fg = col_fg
         end
      end
end)
screen_highlight_timer:start()
-- }}}

clientkeys = gears.table.join(
   awful.key({ modkey,           }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                             end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "f",  awful.client.floating.toggle,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster())     end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()                   end,
      {description = "move to next screen", group = "client"}),
   awful.key({ modkey, "Control" }, "o",      function (c) c:move_to_screen(c.screen.index - 1) end,
      {description = "move to previous screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop                end,
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
      {description = "(un)maximize", group = "client"}),
   awful.key({ modkey, "Control" }, "m",
      function (c)
         c.maximized_vertical = not c.maximized_vertical
         c:raise()
      end ,
      {description = "(un)maximize vertically", group = "client"}),
   awful.key({ modkey, "Shift"   }, "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c:raise()
      end ,
      {description = "(un)maximize horizontally", group = "client"}),
   awful.key({ modkey, "Control", "Shift" }, "o", xrandr.xrandr,
      { description = "toggle xrandr options", group = "screen"}),
   awful.key({ modkey, "Control", "Shift" }, "d", xrandr.set_dpi,
      { description = "set dpi", group = "screen"}),
   awful.key({ modkey, "Control", "Shift" }, "a", xrandr.autorandr,
      { description = "run autorandr", group = "screen"}),
   awful.key({ modkey, "Control" }, "a", xrandr.autorandr,
      { description = "run autorandr", group = "screen"}),
   awful.key({ modkey, "Shift"   }, "g", toggle_gap,
      { description = "toggle useless_gap", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "f", increase_gap,
      { description = "increase useless_gap", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "d", decrease_gap,
      { description = "decrease useless_gap", group = "layout"}),
   awful.key({ modkey,           }, "q", lock.lock_screen,
      { description = "lock screen", group = "awesome" }),
   awful.key({ modkey, "Control" }, "q", lock.toggle_automatic_lock,
      { description = "toggle automatic lock screen", group = "awesome" })
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(globalkeys,
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

clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 3, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.resize(c)
   end)
)

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
   -- Remove titlebars from normal clients and dialogs
   { rule_any = { type = { "normal",
                           "dialog", } },
     properties = { titlebars_enabled = false} },
   -- Floating clients
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
           "xtightvncviewer",
           "Gnome-calculator",
           "Code",
           "MPlayer",
           "pinentry",
           "Gimp",
           "mpv",
           "Mathematica",
           "zoom",
        },
        name = {
           "Event Tester",  -- xev.
           "Microsoft Teams Notification",
           "Slack Call Minipanel",
        },
        role = {
           "AlarmWindow",  -- Thunderbird's calendar.
           "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
           "Dialog",       -- Firefox download dialog
        },
        type = {
           "notification",
        },
   }, properties = { floating = true } },
   -- Maximized clients
   { rule_any = {
        instance = {
           "xfi"
        },
        class = {
           "Emacs",
           "Conkeror",
           "Slack",
           "discord",
           "Skype",
           "Steam",
           "Dwarf_Fortress",
           "[Ss]potify",
           "Totem",
           "DBeaver",
        },
   }, properties = { maximized = true } },
   -- Fullscreen clients
   { rule_any = {
        instance = {
           "xfi"
        },
        class = {
           "mpv",
           "Mathematica",
        },
   }, properties = { fullscreen = true } },
   -- Prevent notifications from stealing focus and place in top right
   { rule_any = {
        name = {
           "Microsoft Teams Notification",
           "Slack Call Minipanel",
        },
        type = {
           "notification",
        },
   }, properties = { focusable = false,
                     ontop = true,
                     placement = awful.placement.top_left, } },
   -- Net
   { rule_any = {
        role = {
           "browser",
        },
        class = {
           "Firefox",
        },
   }, properties = { maximized = true,
                     tag = "Net" } },
   -- This is stupid
   { rule = {
        role = "Dialog",
        class = "Firefox",
   }, properties = { maximized = false, } },
   -- Social
   { rule_any = {
        class = {
           "Slack",
           "discord",
           "Skype",
           "Microsoft Teams - Preview",
           "Keybase",
           "zoom",
        },
   }, properties = { tag = "Social" } },
   -- Steam
   { rule = { class = "Steam" },
     properties = { tag = "Steam" } },
   -- Music
   { rule = { class = "[Ss]potify" },
     properties = { tag = "Music" } },
   -- Video
   { rule_any = { class = {"mpv", "Totem"}},
     properties = { tag = "Video" } },
   -- Math
   { rule = { class = "Mathematica" },
     properties = { tag = "Math" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
                         -- Set the windows at the slave,
                         -- i.e. put it at the end of others instead of setting it master.
                         -- if not awesome.startup then awful.client.setslave(c) end

                         if awesome.startup
                            and not c.size_hints.user_position
                            and not c.size_hints.program_position then
                            -- Prevent clients from being unreachable after screen count changes.
                            awful.placement.no_offscreen(c)
                         end

                         -- Fix spotify spawning without a class by applying rules again after class is set
                         if c.class == nil and c.name ~= "Event Tester" then
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
                         local buttons = gears.table.join(
                            awful.button({ }, 1, function()
                                  c:emit_signal("request::activate", "titlebar", {raise = true})
                                  awful.mouse.client.move(c)
                            end),
                            awful.button({ }, 3, function()
                                  c:emit_signal("request::activate", "titlebar", {raise = true})
                                  awful.mouse.client.resize(c)
                            end)
                         )

                         awful.titlebar(c) : setup
                         {
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
                            layout = wibox.layout.align.horizontal,
                         }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
                         c:emit_signal("request::activate",
                                       "mouse_enter",
                                       {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

local function spawn_once_with_shell(prg, prg_opts, grep_opts)
   prg_opts = prg_opts or ""
   grep_opts = grep_opts or ""
   awful.spawn.with_shell("pgrep " .. grep_opts .. " " .. prg .. " >/dev/null; or " .. prg .. " " .. prg_opts)
end

awful.spawn.with_shell("source ~/.xprofile")
spawn_once_with_shell("nm-applet")

-- Computer specific setup
awful.spawn.easy_async_with_shell('echo -n "$USER"', function(user, stderr, reason, exit_code)
                                     if user == "sst\n" then -- Work setup
                                        spawn_once_with_shell("solaar")
                                        spawn_once_with_shell("blueman-applet")
                                        spawn_once_with_shell("emacs")
                                        spawn_once_with_shell("firefox", "", "-f")
                                        if #xrandr.outputs() > 1 then
                                           spawn_once_with_shell("slack", " --force-device-scale-factor=2")
                                        else
                                           spawn_once_with_shell("slack")
                                        end

                                        spawn_once_with_shell("zoom")
                                        lock.enable_automatic_lock()
                                     end
end)
