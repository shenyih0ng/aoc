#!/bin/bash

echo "y: $1 d: $2"

d=$2;
while [ ${#d} -ne 2 ];
do
  d="0"$d
done

read -p "Are you sure? [Y/n] " -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]
then
	mkdir -p ./$1/Day-$d/
	cp template.cpp ./$1/Day-$d/day$d.cpp
	(curl -v --cookie "session=${AOC_TOKEN}" https://adventofcode.com/$1/day/$2/input -o ./$1/Day-$d/input.txt)
	echo "./$1/Day-$d/ ✅"
fi

