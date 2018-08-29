if [ -z "$1" ]; then 
  tipe="exe:t3"
else
  tipe=$1
fi
LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH ghcid -c "stack ghci --main-is hascpvo:${tipe} --only-main --ghci-options -Wall"
