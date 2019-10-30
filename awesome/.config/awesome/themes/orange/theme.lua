local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
--local xrdb = xresources.get_current_theme()
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

-- inherit default theme
local theme = dofile(themes_path.."default/theme.lua")
-- load vector assets' generators for this theme

local function darker(color_value, darker_n)
   local result = "#"
   for s in color_value:gmatch("[a-fA-F0-9][a-fA-F0-9]") do
      local bg_numeric_value = tonumber("0x"..s) - darker_n
      if bg_numeric_value < 0 then bg_numeric_value = 0 end
      if bg_numeric_value > 255 then bg_numeric_value = 255 end
      result = result .. string.format("%2.2x", bg_numeric_value)
   end
   return result
end

-- sudo apt install xfonts-terminus
theme.font          = "terminus 8"

-- Normal
theme.bg_normal     = "#101820"
--theme.fg_normal     = xrdb.foreground

-- Focus
theme.bg_focus      = darker(theme.bg_normal, -15)
theme.fg_focus      = "#F2AA4C"

-- Urgent
theme.bg_urgent     = "#F21133" --"#900C3F"--  -- "#9E1030" --darker(theme.fg_focus, 60)
theme.fg_urgent     = theme.bg_normal

-- Systray
theme.bg_systray    = theme.bg_normal

-- Minimize
theme.fg_minimize   = darker(theme.fg_normal, 50)
theme.bg_minimize   = darker(theme.bg_normal, 5)

-- Border
theme.border_focus  = theme.fg_focus
--theme.border_normal = xrdb.color0
--theme.border_marked = xrdb.color10

-- Widgets
theme.bg_widget     = theme.bg_normal
theme.fg_widget     = theme.fg_focus

-- Tooltip
theme.tooltip_fg    = theme.fg_normal
theme.tooltip_bg    = theme.bg_normal

-- Hotkeys
theme.hotkeys_modifiers_fg = theme.fg_minimize

-- no extra borders
-- theme.useless_gap   = dpi(3)
-- theme.border_width  = dpi(2)

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"


-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(16)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua

-- Recolor Layout icons:
--theme = theme_assets.recolor_layout(theme, theme.fg_normal)

-- Recolor titlebar icons:
--
-- theme = theme_assets.recolor_titlebar(
--     theme, theme.fg_normal, "normal"
-- )
-- theme = theme_assets.recolor_titlebar(
--     theme, darker(theme.fg_normal, -60), "normal", "hover"
-- )
-- theme = theme_assets.recolor_titlebar(
--     theme, xrdb.color1, "normal", "press"
-- )
-- theme = theme_assets.recolor_titlebar(
--     theme, theme.fg_focus, "focus"
-- )
-- theme = theme_assets.recolor_titlebar(
--     theme, darker(theme.fg_focus, -60), "focus", "hover"
-- )
-- theme = theme_assets.recolor_titlebar(
--     theme, xrdb.color1, "focus", "press"
-- )

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
   theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
   taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
   taglist_square_size, theme.fg_normal
)

return theme
