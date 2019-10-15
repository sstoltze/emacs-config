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
awk -v xtheme="$theme.Xresources" -v kittyconf="$theme.conf" 'BEGIN {OFS=""}
/COLOR/ {
gsub(/"/,"",$2);
gsub(/COLOR_0*/,"color",$2);
gsub(/_COLOR/,"",$2);
split($2,a,"=");
v=tolower(a[1])

if ( substr(a[2],1,1) == "$" ) {
val=var[tolower(substr(a[2],2))];
}
else {
    val=tolower(a[2]);
}
var[v]=val;

if ( v ~ /color[0-9]+/ ) {
number=substr(v,match(v,/[0-9]+/))-1
v="color" number
}

xv=v;
xval=val;
gsub(/cursor/,"cursorColor",xv);
print "*.", xv, ": ", xval > xtheme;

kittyv=v;
kittyval=val;
print kittyv, " ", kittyval > kittyconf;
}' "$theme.sh"
