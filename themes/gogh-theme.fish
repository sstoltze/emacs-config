#!/usr/bin/fish

set -l theme "adventuretime"
set -l url (string join "" "https://raw.githubusercontent.com/Mayccoll/Gogh/master/themes/" $theme ".sh")

if test ! -e "$theme.sh"
    # Download theme
    wget "$url"
else
    echo "Already downloaded $theme"
end

# Xresources
awk 'BEGIN {OFS=""}
/COLOR/ {
gsub(/"/,"",$2);
gsub(/=/,": ", $2);
gsub(/COLOR_0*/,"color",$2);
gsub(/_COLOR/,"",$2);
print "*.", tolower($2)
}' "$theme.sh" > "$theme.Xresources"

# Kitty
awk 'BEGIN {OFS=""}
/COLOR/ {
gsub(/"/,"",$2);
gsub(/=/," ", $2);
gsub(/COLOR_0*/,"color",$2);
gsub(/_COLOR/,"",$2);
print tolower($2)
}' "$theme.sh" > "$theme.conf"
