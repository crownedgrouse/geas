%%% File: geas_disc.hrl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright Eric Pailleau <geas@crownedgrouse.com>
%%% @licence   https://github.com/crownedgrouse/geas/blob/master/LICENCE
%%% @doc 
%%% Geas database to discard or discourage use of some releases when
%%% some modules or functions used. 
%%% @end 

%%-------------------------------------------------------------------------
%% @doc Return list of release to discard and OTP number explaining why.
%% @end
%%-------------------------------------------------------------------------
-spec rel_disc({atom(), atom(), integer()}) -> {list(), atom()} | ok.

rel_disc({syntax_tools, _, _}) -> {["R16B03"], 'OTP-11576'} ;
% Last rule : accept all
rel_disc({_, _, _}) -> ok.

