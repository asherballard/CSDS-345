#!/usr/bin/fish

for file in ./*.plc
    echo "Testing $file"
    racket -f ../interpreter.rkt -t ../interpreter.rkt -e "(interpret \"$file\")"
    echo
end
