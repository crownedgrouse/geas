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

% syntax_tool-1.6.12 buggy
rel_disc({epp_dodger, _, _})     -> {["R16B03"], 'OTP-11576'} ;
rel_disc({erl_prettypr, _, _})   -> {["R16B03"], 'OTP-11576'} ;
rel_disc({erl_recomment, _, _})  -> {["R16B03"], 'OTP-11576'} ;
rel_disc({erl_syntax, _, _})     -> {["R16B03"], 'OTP-11576'} ;
rel_disc({erl_syntax_lib, _, _}) -> {["R16B03"], 'OTP-11576'} ;
rel_disc({erl_tidy, _, _})       -> {["R16B03"], 'OTP-11576'} ;
rel_disc({igor, _, _})           -> {["R16B03"], 'OTP-11576'} ;
% Last rule : accept all
rel_disc({_, _, _}) -> ok.

