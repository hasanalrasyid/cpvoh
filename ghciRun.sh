if [ -z "$1" ]; then 
  tipe="t3"
else
  tipe=$1
fi
stack ghci --main-is hascpvo:exe:${tipe} --only-main
