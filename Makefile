all: vm run solve trace

vm: vm.h vm.c
	gcc -shared -fPIC -Wall -O2 -lm -o libvm.so vm.c
	rm -f libvm.dylib
	ln -s libvm.so libvm.dylib

run: run.c
	gcc -Wall -O2 -L. -lvm -o run run.c

solve: *.hs
	/usr/bin/ghc --make -O2 -L. -lvm -o solve Main.hs

trace: trace.c
	gcc -Wall -O2 -o trace trace.c

solve_all: vm solve
	./solve 1001 > solved/1001.txt
	./solve 1002 > solved/1002.txt
	./solve 1003 > solved/1003.txt
	./solve 1004 > solved/1004.txt
	./solve 2001 > solved/2001.txt
	./solve 2003 > solved/2003.txt
	./solve 2004 > solved/2004.txt

trace_all: trace
	./trace < solved/1001.txt > traced/1001.osf
	./trace < solved/1002.txt > traced/1002.osf
	./trace < solved/1003.txt > traced/1003.osf
	./trace < solved/1004.txt > traced/1004.osf
	./trace < solved/2001.txt > traced/2001.osf
	./trace < solved/2003.txt > traced/2003.osf
	./trace < solved/2004.txt > traced/2004.osf

clean:
	rm -f libvm.so libvm.dylib run solve trace *\.hi *\.o