if [ -z "$1" ]; then 
  tipe="exe:t3"
else
  tipe=$1
fi
#examples:
# exe:t2
# test:f1
#stack ghci --main-is hascpvo:${tipe} --only-main 
#stack ghci --main-is hascpvo:${tipe} --only-main --ghci-options "-fexternal-interpreter -prof"
stack ghci --main-is hascpvo:${tipe} --only-main --ghci-options "-prof"
