## -*- makefile -*-
SHELL := /bin/bash
include vsn.mk
.PHONY: all test edoc plt dialyzer tags clean dist install uninstall

PLT=".dialyzer_plt"

all:
	(mkdir -p ./ebin)
	(cd src;$(MAKE))

test: clean
	(mkdir -p ./ebin)
	(cd src;$(MAKE) DEBUG=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin\", [verbose]), init:stop()")

#comm_test:
#	(mkdir -p ./test/log)
#	(run_test -logdir ./test/log -dir ./test/)

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

plt : 
	(./scripts/gen_plt.sh -a sasl -a compiler)

dialyzer: clean
	(cd src;$(MAKE) DEBUG=true)
	(dialyzer --plt $(PLT) -Werror_handling -Wrace_conditions  -r .)

tags :
	(ctags -R .)

clean:
	(rm -rf ./ebin/*; rm -rf ./edoc/*)
	(cd src;$(MAKE) clean)

dist: clean
	(tar czf $(APP_NAME)-$(APP_VSN).tar.gz ./src ./include Makefile)

install:

uninstall:
