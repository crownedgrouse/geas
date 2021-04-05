PROJECT = $(notdir $(shell pwd))
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DIALYZER_OPTS = -Werror_handling -Wrace_conditions

CUR_DIR = $(shell pwd)

DEPS = samovar
dep_samovar = hex 1.0.0

-include local.mk
include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

