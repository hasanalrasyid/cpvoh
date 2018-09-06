if [ -z "$1" ]; then 
  tipe="exe:t3"
else
  tipe=$1
fi
#examples:
# exe:t2
# test:f1
#stack ghci --main-is cpvoh:${tipe} --only-main 
#stack ghci --main-is cpvoh:${tipe} --only-main --ghci-options "-fexternal-interpreter -prof"
#stack ghci --main-is cpvoh:${tipe} --only-main --ghci-options "-prof"
#stack ghci --main-is cpvoh:${tipe} --only-main --ghci-options "-L. -L./foreign/lib -L./foreign foreign/lib/test.o -v"
stack ghci --main-is cpvoh:${tipe} --only-main --ghci-options "-prof"
