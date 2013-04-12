#! /bin/sh

for i in *.md
do
	j="${i%.*}"
	pandoc -s --highlight-style zenburn --template ./skeleton.html -o $j.html $i
done
