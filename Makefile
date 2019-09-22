PROJECT = $(notdir $(shell pwd))
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
DIALYZER_OPTS = -Werror_handling -Wrace_conditions

CUR_DIR = $(shell pwd)

#GEAS_RELEASES = R15B R15B01 R15B02 R15B03 R15B03-1 R16B R16B01 R16B02 R16B03 R16B03-1 17.0 17.1 17.3 17.4 17.5 18.0 18.1 18.2

#relinfo = rm -rf .geas && mkdir -p .geas && cd .geas && kerl install $(1) && bin/erl -pa ../ebin -init_debug -s geas_doc relinfo $(1) $(2) -s init stop && cd $(CUR_DIR)

DEPS = samovar
dep_samovar = hex ~1

include erlang.mk
#-include plugins.mk

otp:
	cd priv && rm -f otp_versions.table && wget https://raw.githubusercontent.com/erlang/otp/master/otp_versions.table

clean:: 
	-@find . -type f -name \*~ -delete


#relinfos:

