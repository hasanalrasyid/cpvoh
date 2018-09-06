shake: 
	./build.hs
#	stack exec ./build.shake.sh

clean:
	rm -rf _build

test:
	stack build cpvoh:exe:test
