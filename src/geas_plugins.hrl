%%-------------------------------------------------------------------------
%% @doc rebar plugin call function
%% @since 2.0.2
%% @end
%%-------------------------------------------------------------------------

geas(_, _ ) -> 
   {ok, Dir} = file:get_cwd(),
   compat(Dir),
   guilty(Dir),
   geas:log().


%%-------------------------------------------------------------------------
%% @doc erlang.mk plugin call function
%% @since 2.5
%% @end
%%-------------------------------------------------------------------------
geas_check(Dir) ->
	compat(Dir),
    guilty(Dir),
    geas:log(),
    halt(get(geas_exit_code)).







