# Defined in - @ line 0
function linux-update --description 'alias update sudo apt-get update; sudo apt-get -y dist-upgrade; sudo apt-get -y autoremove; sudo apt-get -y autoclean'
	sudo apt-get update; sudo apt-get -y dist-upgrade; sudo apt-get -y autoremove; sudo apt-get -y autoclean $argv;
end
