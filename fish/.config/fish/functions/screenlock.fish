#!/usr/bin/fish
# Glichting screen lock script from https://github.com/kittenparry/dot-files/blob/master/.scripts/_others/scrlock.sh by xzvf
# Updated to fish and modified
function screenlock
    set -l pngfile "/tmp/screenlock.png"
    set -l bmpfile "/tmp/screenlock.bmp"
    set -l glitchedfile "/tmp/screenlock_g.bmp"

    scrot -z $pngfile

    # convert to bmp and pixelate
    convert $pngfile -scale 20% -scale 500% $bmpfile

    # Glitch it with sox: https://maryknize.com/blog/glitch_art_with_sox_imagemagick_and_vim/
    sox -t ul -c 1 -r 48k $bmpfile -t ul $glitchedfile trim 0 100s : echo 0.4 0.8 8 0.9
    convert $glitchedfile -rotate 90 $glitchedfile
    sox -t ul -c 1 -r 48k $glitchedfile -t ul $bmpfile trim 0 100s : echo 0.4 0.8 8 0.9
    # sox -t ul -c 1 -r 48k $glitchedfile -t ul $bmpfile trim 0 100s : phaser 0.3 0.9 1 0.7 0.5 -t #echo 0.9 0.9 4 1
    # sox -t ul -c 1 -r 48k $glitchedfile -t ul $bmpfile trim 0 100s : flanger
    convert $bmpfile -rotate -90 $bmpfile

    # Add lock icon, pixelate and convert back to png
    convert $bmpfile -gravity center -font "Iosevka" -pointsize 200 -fill '#bf616a' -draw "text 0,0 '?" -channel RGBA $pngfile

    i3lock -e -u -i $pngfile
    #feh $pngfile
    rm $pngfile $bmpfile $glitchedfile
end
