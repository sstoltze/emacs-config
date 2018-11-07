#!/usr/bin/fish

function handle_git
    set -l gitstatus (git status -s)
    if test -n "$gitstatus"
        echo $gitstatus
        echo ""
        echo "Adding and pushing to git."
        git add --all
        git commit -m (date +"%F %H:%M")
        git push
    else
        echo "No changes, nothing to commit."
    end
    echo ""
end

echo "Noter:"
cp -r /cygdrive/c/users/$USER/appdata/roaming/.emacs.d/org-files/ ~/noter/gtd/
cd ~/noter
handle_git
cd ~

echo "ML:"
cd ~/ml
handle_git
cd ~

echo "emacs-config:"
# Files handled by stow, in case I mess it up somehow
cp -H  -t ~/git/emacs-config/work/windows         /cygdrive/C/Users/$USER/AppData/Roaming/.emacs.d/init.el /cygdrive/C/Users/$USER/AppData/Roaming/.gitconfig
cp -H  -t ~/git/emacs-config/work/  ~/.gitconfig
cp -rH -t ~/git/emacs-config/work         ~/backup-work.fish ~/move-to-zip.fish ~/check-status.fish ~/update.fish ~/color-test.fish /cygdrive/c/Users/sisto/Pictures/background ~/prog
cp -rH -t ~/git/emacs-config/work/.config ~/.config/mintty
cd ~/git/emacs-config
handle_git
cd ~
