all:
	(cd src; $(MAKE))

clean:
	rm -f erl_crash.dump
	(cd src; $(MAKE) clean)
