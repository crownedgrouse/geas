#
#  Geas compat Erlang.mk plugin
#  Licence : https://github.com/crownedgrouse/geas/blob/master/LICENCE
#  Author : Eric Pailleau <geas@crownegrouse.com> 


help::
	@printf "%s" "" \
                "  geas_compat    Display compatibility of whole project with official Erlang releases"

geas_compat: deps app
	$(verbose) erl -noshell -pa deps/geas/ebin -pa ebin -s geas compat $(shell pwd) -s init stop
