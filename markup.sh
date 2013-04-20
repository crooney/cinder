#! /bin/sh

for i in *.md
do
	j="${i%.*}"
	pandoc -t html5 --template ./skeleton.html -o $j.html $i
done
