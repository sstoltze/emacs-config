#/usr/fish
function start-bluetooth
    modprobe btusb
    service bluetooth restart
end
