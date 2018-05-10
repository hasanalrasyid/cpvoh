if [ -z "$1" ]; then 
  tipe="exe:t3"
else
  tipe=$1
fi
ghcid -c "stack ghci --main-is hascpvo:${tipe} --only-main"
