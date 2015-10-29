PROJECT = $(notdir $(shell pwd))
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

CUR_DIR = $(shell pwd)

GEAS_RELEASES = R15B03 R16B01
relinfo = mkdir -p .geas && cd .geas && kerl install $(1) && bash ./activate && erl -pa ../ebin -s geas relinfo $(1) $(2) -s init stop && cd $(CUR_DIR) 

include erlang.mk

clean:: 
	-@find . -type f -name \*~ -delete


relinfos:
	$(foreach rel, $(GEAS_RELEASES), $(call relinfo, $(rel), "$(CUR_DIR)/priv/relinfos" ) )
