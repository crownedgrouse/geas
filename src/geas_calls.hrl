%%-------------------------------------------------------------------------
%% @doc Extract remote call of external functions in abstract code
%% @end
%%-------------------------------------------------------------------------
-spec get_remote_call(tuple(), list()) -> list().

get_remote_call({call,_,{atom,_, F}, Args}, Exp)
   when is_list(Args)->
      Arity = length(Args),
      Z = lists:any(fun({_, A, B}) -> ((A == F) andalso (B == Arity)) end, Exp),
      X = case Z of
            false -> {erlang, F, Arity};
            true  -> []
            end,
      [X, lists:flatmap(fun(X_) -> get_remote_call(X_, Exp) end, Args)] ;
get_remote_call({call,_, {remote,_,{atom, _,M},{atom, _, F}}, Args}, Exp)
                when is_list(Args)-> [{M, F, length(Args)}, lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, Args)] ;
get_remote_call({call,_, {remote,_,{var,_,M},{atom,_,F}}, Args}, Exp)
                when is_list(Args)-> [{M, F, length(Args)}, lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, Args)] ;
get_remote_call({_, _, _, L1, L2, L3}, Exp)
      when  is_list(L1),
            is_list(L2),
            is_list(L3) -> [lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L1),
                            lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L2),
                            lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L3)];
get_remote_call({_, _, _, _, L}, Exp) when is_list(L) -> lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L) ;
get_remote_call({'case', _, {call, A, B, C}, L}, Exp) -> [get_remote_call({call, A, B, C}, Exp), lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L)] ;
get_remote_call({_, _, _, _, _}, _Exp) -> [] ;
get_remote_call({_, _, _, L}, Exp)   when is_list(L) -> lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L) ;
get_remote_call({_, _, _, T}, Exp)   when is_tuple(T) -> get_remote_call(T, Exp) ;
get_remote_call({_, _, _, _}, _Exp) -> [] ;
get_remote_call({_, _, L}, Exp) when is_list(L) -> lists:flatmap(fun(X) -> get_remote_call(X, Exp) end, L) ;
get_remote_call({_, _, _}, _Exp) -> [] ;
get_remote_call({_, _}, _Exp) -> [] ;
get_remote_call(_I, _Exp) when is_integer(_I) -> [];
get_remote_call(_A, _Exp) when is_atom(_A) -> [];
get_remote_call(_Z, _Exp) -> io:format("Missed : ~p~n", [_Z]),
				?LOG(geas_logs,{debug, unhandled, _Z}),
                       [].

