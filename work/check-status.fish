#!/usr/bin/fish

set args (getopt -s sh -q -l color,colour,no-color,no-colour,help,pattern:,ignore-case,branch,no-status -o c,n,h,p:,i,b -- $argv)
set args (string split " " $args)

if     contains -- "-h"     $args
    or contains -- "--help" $args
    echo "Runs trough folders with 'dagpenge' in the name and runs git log, status and branch for each folder."
    echo -e ""
    echo -e "Possible options:"
    echo -e "-h, --help                  - This text."
    echo -e "-c, --color,    --colour    - Colours all output."
    echo -e "                              Use with 'less -R'."
    echo -e "-n, --no-color, --no-colour - Removes all colour from output.\n" # The -n supresses newline...
    echo -e "                              Use with 'less'."
    echo -e "-p, --pattern (PATTERN)     - Specify a grep pattern for matching git folders in CWD."
    echo -e "                              Default is given by the file .status-pattern"
    echo -e "-i, --ignore-case           - Ignore case when pattern matching with grep."
    echo -e "-b, --branch                - Prints info about branches."
    echo -e "--no-status                 - Do not display status info."
    exit 0
end

set headline_color TRUE

if contains -- "-p" $args
    set index   (contains -i -- "-p" $args)
    set pattern (string trim -c "'" $args[(math "$index + 1")])
else if contains -- "--pattern" $args
    set index   (contains -i -- "--pattern" $args)
    set pattern (string trim -c "'" $args[(math "$index + 1")])
else
    if test -e "./.status-pattern"
        set pattern (cat ".status-pattern")
    else
        echo "Please specify a grep pattern in the file .status-pattern"
        exit 0
    end
end

if     contains -- "-i"            $args
    or contains -- "--ignore-case" $args
    set grep_options "-ie"
else
    set grep_options "-e"
end

if     contains -- "-c"       $args
    or contains -- "--color"  $args
    or contains -- "--colour" $args
    set color "--color=always"
else if contains -- "-n"          $args
    or  contains -- "--no-color"  $args
    or  contains -- "--no-colour" $args
    set headline_color FALSE
    set color "--color=never"
else
    set color "--color=auto"
end

for dir in (ls -F | grep $grep_options "$pattern.*/")
    cd $dir
    if not test $headline_color = "FALSE"
        set_color -ou cyan
    end
    echo "Git status: $dir"
    if not test $headline_color = "FALSE"
        set_color normal
    end
    git lg-sim -n 2 $color
    echo ""
    if not contains -- "--no-status" $args
        git status -s
        echo ""
    end
    if     contains -- "-b"       $args
        or contains -- "--branch" $args
        git branch $color
        echo ""
    end
    cd ..
end











