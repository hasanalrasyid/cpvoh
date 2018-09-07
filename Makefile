shake: 
	./build.hs
#	stack exec ./build.shake.sh

cleanf:
	./build.hs cleanf

clean:
	rm -rf _build

test:
	stack build cpvoh:exe:test
