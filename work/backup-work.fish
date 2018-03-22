#!/usr/bin/fish

cp -rH -t ~/git/emacs-config/work ~/.config ~/backup-work.fish ~/move-to-zip.fish ~/check-status.fish ~/update.fish ~/.gitconfig ~/.gitignore ~/.emacs
rm -r ~/git/emacs-config/work/.config/fish/generated_completions
cd ~/git/emacs-config
if test -n (git status -s)
    echo "Pushing to git"
    git add --all
    git commit -m (date +"%F %H:%M")
    git push
end

