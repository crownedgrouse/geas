-define(LOG(X, Y),
        case erlang:get(X) of
			 undefined when is_list(Y)  -> erlang:put(X, Y) ;
			 _         when is_list(Y)  -> erlang:put(X, erlang:get(X) ++ Y);
			 undefined when is_tuple(Y) -> erlang:put(X, [Y]) ;
			 _         when is_tuple(Y) -> erlang:put(X, erlang:get(X) ++ [Y])
		end
).

%%-------------------------------------------------------------------------
%% @doc Log levels set
%% @since 2.0.6
%% @end
%%-------------------------------------------------------------------------
loglevels() ->
   case os:getenv("GEAS_LOG") of
         false -> [] ;
         ""    -> ["debug", "notice", "warning", "error", "tip"];
         LL    -> string:tokens(LL, " ")
   end.

%%-------------------------------------------------------------------------
%% @doc Log infos on analyze, and reinit log buffer
%% @since 2.0.6
%% @end
%%-------------------------------------------------------------------------
log() ->
   L  = get(geas_logs),
   LL = loglevels(),
   case is_list(L) of
      true  -> log(L, LL);
      false -> ok
   end,
   put(geas_logs, []).

log(L) when is_list(L) -> log(L, loglevels());
log(L) -> log([L], loglevels()).

log(L, LogLevels) ->
   Fun =
      fun(X) -> {Level, Info, File} = X,
         case lists:member(atom_to_list(Level), LogLevels) of
            true  -> 
               Tag = case Level of
                        notice  -> "(i)";
                        warning -> "/!\\" ;
                        error   -> "[!]";
                        tip     -> "(?)";
                        _       -> " @ "
                     end,
               InfoS     = lists:flatten(io_lib:format("~p",[Info])) ,
               FileInfos = lists:flatten(io_lib:format("~p",[File])) ,
               io:format(standard_error, "~s ~s ~s~n",[Tag, string:left(InfoS,30), FileInfos]);
            false -> ok
         end
      end,
   lists:foreach(Fun, L).