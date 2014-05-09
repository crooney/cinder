#!/bin/sh

if [ -z "${1}" ] ; then
	echo "no markup lang specified. Usage: ${0} [HTML|SVG|whatever]"
	exit 42
fi

for i in $@
do
DIR="../src/Cinder/${i}/"

mkdir -p $DIR

echo "{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Cinder.${i}.Attributes where
import Cinder.DSL
import Fay.Text
" \
	>$DIR/Attributes.hs

cat "${i}attributes.txt" | sort | uniq | sed 'h;s/[-:]\(.\)/\u\1/g;G' \
	| sed -n 'N;s/\(.*\)\n\(.*\)/\1 :: Text -> Primitive\
\1 = Attribute "\2"\
/p;' \
	>>$DIR/Attributes.hs

echo "{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Cinder.${i}.Elements where 
import Cinder.DSL
import Fay.Text
" \
	>$DIR/Elements.hs

cat "${i}elements.txt" | sort | uniq | sed 'h;s/[-:]\(.\)/\u\1/g;G' \
	| sed -n 'N;s/\(.*\)\n\(.*\)/\1 :: Primitive\
\1 = Element "\2"\
/p;' \
	>>$DIR/Elements.hs

done
