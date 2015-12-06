#
#  Geas display Erlang.mk plugin
#  Licence : Licence : https://github.com/crownedgrouse/geas/blob/master/LICENCE
#  Author : Eric Pailleau <geas@crownegrouse.com> 


help::
	@printf "%s\n" "" \
                "  geas_compat    Display compatibility of whole project with official Erlang releases"

geas_compat:
	$(verbose) erl -noshell -pa deps/geas/ebin -pa ebin -s geas display $(shell pwd) -s init stop
