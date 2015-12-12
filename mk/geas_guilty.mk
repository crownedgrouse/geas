#
#  Geas guilty Erlang.mk plugin
#  Licence : https://github.com/crownedgrouse/geas/blob/master/LICENCE
#  Author : Eric Pailleau <geas@crownegrouse.com> 


help::
	@printf "%s\n" "" \
                "  geas_guilty    Display offending functions that reduce official Erlang release runnable window"

geas_guilty: deps app
	$(verbose) erl -noshell -pa deps/geas/ebin -pa ebin -s geas guilty $(shell pwd) -s init stop
