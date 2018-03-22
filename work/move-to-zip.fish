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
    cd $dir
    echo "Saving $dir"
    git_save
    for z in (ls | grep ".zip")
        echo "Moving $z"
        mv $z "../zip-versioner/$curr_date/."
    end
    cd ..
end

