#./_build/run
#LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH stack exec test
make;mpirun -n 3 -f hostfile ./_build/run
