#!/usr/bin/fish

set -l curr_date (date +%F)

if test -e "./.status-pattern"
    set pattern (cat ".status-pattern")
    echo "Grep pattern: '$pattern.*/'"
else
    echo "Please specify a grep pattern in the file .status-pattern"
    exit 0
end

if not test -d "./zip-versioner/$curr_date"
    echo "Making ./zip-versioner/$curr_date"
    mkdir "./zip-versioner/$curr_date"
end

for dir in (ls -F | grep "$pattern.*/")
    echo "Saving $dir"

    set -l git_dir (string split -m 1 -r / $dir)[1]
    set -l zip_name "$git_dir-"(date +%F)".zip"

    zip -q -r "$zip_name" -x "*.git*" @ "$git_dir"

    for z in (ls | grep "\.zip")
        echo "Moving $z"
        mv $z "./zip-versioner/$curr_date/."
    end
end
