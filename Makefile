shake:
	./build.hs
#	stack exec ./build.shake.sh

test:
	stack build hascpvo:exe:test
