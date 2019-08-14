#/usr/fish
function start-bluetooth
    sudo modprobe btusb
    sudo service bluetooth restart
end
