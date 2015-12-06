# 
#  GEAS Erlang.mk plugins
#  Licence : https://github.com/crownedgrouse/geas/blob/master/LICENCE
#  Author : Eric Pailleau <geas@crownegrouse.com>
#

help::
	@printf "%s\n" "" \
                "Geas targets:"
	@printf "%s\n" "  geas           Apply all geas plugins below"


THIS := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
include $(THIS)/mk/geas_compat.mk
include $(THIS)/mk/geas_guilty.mk

geas: geas_compat geas_guilty

