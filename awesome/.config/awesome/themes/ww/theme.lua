--------------------------------
--    "WWII" awesome theme    --
--------------------------------
--  Author: Adrian C. (anrxc) --
--  * inspired by wmii colors --
--------------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local xrdb = xresources.get_current_theme()
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local theme = dofile(themes_path.."default/theme.lua")

-- {{{ Styles
theme.font          = "sans 8"

-- {{{ Colors
theme.fg_normal     = "#000000"
theme.fg_focus      = "#000000"
theme.fg_urgent     = "#CF6171"
theme.fg_minimize   = "#000000"

theme.bg_normal     = "#C1C48B"
theme.bg_focus      = "#81654F"
theme.bg_urgent     = "#C1C48B"
theme.bg_minimize   = "#81654F"

theme.taglist_sel   = "#ffffff"
theme.taglist_unsel = "#ffffff"
-- }}}

-- {{{ Borders
--theme.border_width  = "1"
theme.border_normal = theme.bg_focus
theme.border_focus  = theme.fg_focus
theme.border_marked = theme.fg_marked
-- }}}

theme.tooltip_fg = theme.fg_normal
theme.tooltip_bg = theme.bg_normal

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal
-- theme.titlebar_[normal|focus]
-- }}}

theme = theme_assets.recolor_layout(theme, theme.fg_normal)

-- {{{ Other
-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
--theme.taglist_bg_focus = "#CF6171"
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.fg_widget        = "#AECF96"
--theme.fg_center_widget = "#88A175"
--theme.fg_end_widget    = "#FF5656"
--theme.bg_widget        = "#494B4F"
--theme.border_widget    = "#3F3F3F"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = theme.fg_urgent
-- theme.mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(16)
theme.menu_width  = dpi(100)

-- }}}

-- {{{ Icons
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
    taglist_square_size, theme.taglist_sel --theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.taglist_unsel --theme.fg_normal
)
-- }}}

return theme
