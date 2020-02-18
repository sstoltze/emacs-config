function git-kondo
    set -l start_dir (pwd)
    cd (git rev-parse --show-toplevel)
    set -l files (git diff (git merge-base origin/master HEAD) --name-only | grep -e '.clj[sc]*$')
    if test -n "$files"
        clj-kondo --lint $files
    end
    cd "$start_dir"
end
