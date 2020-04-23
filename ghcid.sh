if [ -z "$1" ]; then
  tipe="cpvoh:exe:t3"
else
  tipe=$1
fi
if [ -n "$2"]; then
  dev="-Wno-missing-signatures -Wno-orphans -Wno-unused-matches -Wno-unused-local-binds -Wno-unused-top-binds -Wno-unused-imports "
else
  dev= ""
fi

LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH ghcid -c "stack ghci --main-is ${tipe} --only-main --ghci-options '-Wall $dev'"
