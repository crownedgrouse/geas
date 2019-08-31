%%-------------------------------------------------------------------------
%% @doc List currently installed patches
%% @end
%%-------------------------------------------------------------------------
list_installed_patches(undefined) -> [];
list_installed_patches(Current) ->
   P = list_potential_patches(Current),
   % For each potential patch, see if all updated application are found in lib_dir
   A =lists:foldl(fun({L, R}, Acc) ->
                   case lists:all(fun(X)-> [Nameb, _] = binary:split(list_to_binary(X), <<"-">>, ?SPLIT_OPTS),
                                            Name = list_to_atom(binary_to_list(Nameb)),
                                           code:lib_dir(Name) == filename:join(code:lib_dir(), X)
                                  end , R) of
                     false -> Acc ;
                     true  -> lists:flatten(Acc ++ [list_to_atom(L)])
                   end
               end, [], P),
   lists:flatmap(fun(X) -> [atom_to_list(X)] end, A).

%%-------------------------------------------------------------------------
%% @doc List possible patches for current release
%% @end
%%-------------------------------------------------------------------------
list_potential_patches(Current) ->
   O = parse_otp_table(),
   {Poss, _} = lists:partition(fun({L, _R}) ->
         % Keep L when not current release
         case L of
            Current -> false ;
            _ -> % Keep L when a patche of current release
                  lists:prefix(Current, L)
         end
      end, O),
   Poss.

%%-------------------------------------------------------------------------
%% @doc Parse otp_versions.table
%% @end
%%-------------------------------------------------------------------------
parse_otp_table() ->
   S = filename:join([get(geas_cwd),code:priv_dir(geas), "otp_versions.table"]),
   {ok, B} = file:read_file(S),
   Raw = binary:split(B, <<"\n">>, ?SPLIT_OPTS),
   Net = lists:foldl(fun(X, Acc) -> 
      case X of
         <<>> -> [];
         _  ->
            [N | _] = binary:split(X, <<"#">>,?SPLIT_OPTS),
            [L, R] = binary:split(N, <<":">>, ?SPLIT_OPTS),
            [_, V] = binary:split(L, <<"-">>, ?SPLIT_OPTS),
            P_ = binary:split(R, <<" ">>, ?SPLIT_OPTS),
            P  = lists:flatmap(fun(Z) -> [erlang:binary_to_list(Z)] end, P_),
            Acc ++ [{string:strip(erlang:binary_to_list(V)), P}]
            end
   end, [], Raw),
   Net.

%%-------------------------------------------------------------------------
%% @doc Get recommanded patches
%% @end
%%-------------------------------------------------------------------------
get_recommanded_patches(Current) ->
   % List all applications used in current global code
   C = get(geas_calls),
   T = filename:join([get(geas_cwd),code:priv_dir(geas),"mod2app.term"]),
   {ok, B} = file:read_file(T),
   Term = erlang:binary_to_term(B),
   Apps = lists:usort(lists:flatmap(fun({M, _, _}) -> 
      case lists:keyfind(M, 1, Term) of
         false  -> [];
         {M, A} -> [A];
         _      -> []
      end
   end, C)),
   erlang:put(geas_apps, Apps),
   log({notice, applications, Apps}),
   % List patches higher than current version
   Pot = list_potential_patches(Current),
   Ins = list_installed_patches(Current),
   Candidats = lists:usort(lists:flatten(lists:flatmap(fun({P, L}) ->
                                                         case lists:member(P, Ins) of
                                                               true -> [];
                                                               false -> [{P, L}]
                                                         end
                                         end , Pot))),
   % For each patche, list application(s) that should be upgraded
   R = lists:usort(lists:flatmap(fun({P, Al}) ->
                                    %erlang:display({P, Al}),
                                    [lists:usort(lists:flatmap(fun(A) ->
                                                      [A_ | _] = string:split(A, "-"),
                                                      case lists:member(erlang:list_to_atom(A_), Apps) of
                                                         false -> [];
                                                         true  -> log({tip, {recommend, P}, A}),
                                                                  [P]
                                                      end
                                                end, Al))]
                 end, lists:reverse(Candidats))),
   R.