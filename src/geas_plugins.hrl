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
	put(geas_exit_code, 0),
	try 
		compat(Dir),
	    guilty(Dir),
		geas:log()
    catch
    	_:Exit1 -> put(geas_exit_code, Exit1)
    after
        Exit = get(geas_exit_code),
	    Msg  = case Exit of
	        1 -> "Current version is incompatible with release window";
	        2 -> "Release window do not match required semver version range";
	        3 -> "Incompatible BEAM file, may need recompilation";
	        4 -> "Incompatible BEAM maximum opcode, may need recompilation";
	        _ -> "Unexpected geas exit code"
	    end,
	    case Exit of
	    	0 ->  do_log({notice, compat, "Success"}) ;
	    	_ ->  do_log({error, compat, Msg}) 
	    end,
		geas:log(),
		halt(Exit)
    end.

 do_log(M) -> ?LOG(geas_logs, M) .








