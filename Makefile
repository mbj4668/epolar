DESCRIPTION = Erlang library for sailing polars

DEPS = eclip mtab

dep_eclip = git https://github.com/mbj4668/eclip.git
dep_mtab = git https://github.com/mbj4668/mtab.git

ESCRIPT_FILE = bin/epolar
ESCRIPT_MODULE = epolar_script

#EXCLUDE_ERL_MODULES  = epolar_script

include erl.mk

all: bin/epolar

erl.mk:
	curl -s -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@

clean: bin-clean

bin-clean:
	rm -rf bin
