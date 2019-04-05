all:
	chicken-install -n

install:
	chicken-install -s

test:
	chicken-install -n -test

clean:
	rm -vf *.so *.import.scm *.link *.static.o *.build.sh *.install.sh
