function git_save --description 'Write out the prompt'
	set -l git_dir (string split -m 1 -r / (pwd))[2]
        set -l branch (string split " " (git branch | grep "*")[1])[2]

        git archive $branch --format=zip --prefix="$git_dir/" >"$git_dir-"(date +%F)".zip"
end
