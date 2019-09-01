#
#  Geas Erlang.mk plugin
#  Licence : https://github.com/crownedgrouse/geas/blob/master/LICENCE
#  Author : Eric Pailleau <geas@crownegrouse.com> 

geas_check: deps app
	$(verbose) erl -noshell -pa deps/geas/ebin -pa ebin -s geas geas_check $(shell pwd) 
	ifeq ($(.SHELLSTATUS),1)
    	$(error Current version is incompatible with release window)
	endif
	ifeq ($(.SHELLSTATUS),2)
    	$(error Release window do not match required semver version range)
	endif
	ifeq ($(.SHELLSTATUS),3)
    	$(error Incompatible BEAM file, may need recompilation)
	endif
	ifeq ($(.SHELLSTATUS),4)
    	$(error Incompatible BEAM maximum opcode, may need recompilation)
	endif