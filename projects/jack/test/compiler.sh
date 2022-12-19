#!/bin/sh

cd test_compiler

for dir in */
do
	cd $dir
	for file in *.jack
	do
		../../../jack $file
		diff --color vmout "${file%%jack}vm" || \
			(echo "FAILED $file" && cp vmout ../../failure && exit 1)
	done
	rm astout symout xmlout vmout
	cd ..
done

cd ..
