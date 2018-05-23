SOURCES = $(wildcard *.F)
OBJECT = $(addprefix obj.gfortran/,$(SOURCES:%.F=%.o))
FC = gfortran
FFLAGS = -fPIC -O2  -fomit-frame-pointer -funroll-loops  -ffast-math  -ffixed-line-length-132 -DHASIARGC  -DHASGETARG  -DFDATE  -DHASCPUTIME -DHASGETENVIRONMENTVARIABLE  -DNOQUAD  -DNOT_OVERLAP_VAR  -J./MODDIR/obj.gfortran -I./MODDIR/obj.gfortran -c

all: $(OBJECT) 
	@echo "Compiling in src" 
	@echo "=========FIN src" 

%.o:
	@echo "compiling "$@
	$(FC) $(FFLAGS) $(patsubst obj.gfortran/%.o,%.F,$@) -o $@

clean:
	rm -f obj.gfortran/* 
