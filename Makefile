PROJECT = epolar
PROJECT_DESCRIPTION = Erlang library for sailing polars
PROJECT_VERSION = 0.1.0

DEPS =

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
