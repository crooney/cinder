#!/bin/sh

if [ -z "${1}" ] ; then
	echo "no markup lang specified. Usage: ${0} [HTML|SVG|whatever]"
	exit 42
fi

for i in $@
do
DIR="../src/Cinder/${i}/"

mkdir -p $DIR

echo "module Cinder.${i}.Attributes where\nimport Cinder.DSL\n\n" \
	>$DIR/Attributes.hs

cat "${i}attributes.txt" | sort | uniq | sed 'h;s/[-:]\(.\)/\u\1/g;G' \
	| sed -n 'N;s/\(.*\)\n\(.*\)/\1 :: String -> Primitive\n\1 = Attribute "\2"\n/p;' \
	>>$DIR/Attributes.hs

echo "module Cinder.${i}.Elements where\nimport Cinder.DSL\n\n" \
	>$DIR/Elements.hs

cat "${i}elements.txt" | sort | uniq | sed 'h;s/[-:]\(.\)/\u\1/g;G' \
	| sed -n 'N;s/\(.*\)\n\(.*\)/\1 :: Primitive\n\1 = Element "\2"\n/p;' \
	>>$DIR/Elements.hs

done
