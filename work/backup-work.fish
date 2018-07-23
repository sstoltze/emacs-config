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
cd ~/noter
handle_git
cd ~

echo "emacs-config:"
cp -H -t ~/git/emacs-config ~/.emacs
cp -rH -t ~/git/emacs-config/work ~/.config ~/backup-work.fish ~/move-to-zip.fish ~/check-status.fish ~/update.fish ~/.gitconfig ~/.gitignore ~/color-test.fish /cygdrive/c/Users/sisto/Pictures/background ~/prog
rm -r ~/git/emacs-config/work/.config/fish/generated_completions
cd ~/git/emacs-config
handle_git
cd ~
