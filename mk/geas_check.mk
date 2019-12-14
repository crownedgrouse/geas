#
#  Geas Erlang.mk plugin
#  Licence : https://github.com/crownedgrouse/geas/blob/master/LICENCE
#  Author : Eric Pailleau <geas@crownegrouse.com> 

geas_check: deps app
	$(verbose) erl -noshell -pa deps/geas/ebin -pa ebin -s geas geas_check $(shell pwd)