## -*- makefile -*-
## Erlang
#SHELL=/bin/bash
include ../vsn.mk
.PHONY: all test edoc dialyzer clean

DOC_OPTS={dir,\"../edoc\"}

ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../include 
EBIN_DIRS := $(wildcard ../libs/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifdef DEBUG
  ERLC_FLAGS += -DUNITTEST -DTEST
  ERLC_FLAGS += +debug_info
else
  ERLC_FLAGS += +inline
endif

ifdef HIPE
  ERLC_FLAGS += +native +"{hipe, [o3]}"
endif

BASE_DIR := ..
EBIN_DIR := ../ebin
DOC_DIR  = ../edoc
LIBS_DIR = ../libs
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_MODULES := $(ERL_SOURCES:%.erl=%)
ERL_MODULES_IN_APP := $(ERL_SOURCES:%.erl=`%`,)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_TPL_FILES = $(wildcard *.app.tpl)
APP_FILES = $(APP_TPL_FILES:%.tpl=%)

ALL_FILES = $(ERL_OBJECTS) $(addprefix $(EBIN_DIR)/, $(APP_FILES)) 

$(EBIN_DIR)/%.app: %.app.tpl
	@echo "creating .app file... "
	@sed -e 's:@MODULES@:$(ERL_MODULES_IN_APP):g' -e "s:\`:\':g" -e 's/,]/]/g' $< > $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

all: $(ALL_FILES)
	@echo "all files is " $(ALL_FILES)

edoc:
	$(ERL) -noshell -eval "edoc:application($(APP_NAME), \".\", [${DOC_OPTS}]),init:stop()"

test: all
	$(ERL) -noshell -pa ../ebin -s eunit test '.' -s init stop

clean:
	(rm -rf ./*.beam; rm -f *.app; rm -f *ctl)
