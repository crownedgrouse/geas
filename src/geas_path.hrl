%%-------------------------------------------------------------------------
%% @doc Get file extension to search depending option environment variable
%% @end
%%-------------------------------------------------------------------------

ext_to_search() -> 
   case os:getenv("GEAS_USE_SRC") of
      false -> ".beam$" ;
      "0"   -> ".beam$" ;
      "1"   -> ".erl$"
   end.

%%-------------------------------------------------------------------------
%% @doc Get directory to search depending mode
%% @end
%%-------------------------------------------------------------------------
dir_to_search() -> 
   case os:getenv("GEAS_USE_SRC") of
      false -> "ebin" ;
      "0"   -> "ebin" ;
      "1"   -> "src"
   end.

%%-------------------------------------------------------------------------
%% @doc Get upper/parallel directory matching a needle
%% @end
%%-------------------------------------------------------------------------
get_upper_dir("/", _) -> "" ;
get_upper_dir(".", _) -> "" ;
get_upper_dir(Dir, Needle) -> 
   case filename:basename(Dir) of
      Needle -> Dir ;
      _      -> case filelib:is_dir(filename:join(Dir, Needle)) of
               false -> get_upper_dir(filename:dirname(Dir), Needle) ;
               true  -> filename:join(Dir, Needle)
               end
   end.

%%-------------------------------------------------------------------------
%% @doc Is the directory existing and readable ?
%% @end
%%-------------------------------------------------------------------------
-spec is_valid_dir(list()) -> ok | error.

is_valid_dir(Dir) ->
    case filelib:is_dir(Dir) of
      true  -> case file:list_dir(Dir) of
                    {ok, []} ->  throw("Empty directory : "++Dir);
                    {ok, _}  ->  ok;
                    {error, eacces}  -> throw("Access denied : "++Dir), error;
                    {error, E}       -> throw(io_lib:format("Error : ~p",[E])),
                                        error
               end;
      false -> throw("Directory not found : "++Dir), error
    end.

%%-------------------------------------------------------------------------
%% @doc Does this directory contains a valid Erlang project ?
%% @end
%%-------------------------------------------------------------------------
-spec is_valid_erlang_project(list()) -> atom().

is_valid_erlang_project(Dir) ->
    % Workaround for GEAS_USE_SRC : some projects have subdirectories under src/.
    % example :  https://github.com/nitrogen/simple_bridge
    % Bring upper directory in such case
    Dir2 = case filename:basename(Dir) of
			 "src" -> filename:dirname(Dir) ;
			 _     -> Dir
		   end,
    {ok, L} = file:list_dir(Dir2),
	% When using beam files, do not require src/ to be there
	Src  = case os:getenv("GEAS_USE_SRC") of
		   		false -> true ;
		   		"0"   -> true ;
		   		"1"   -> lists:any(fun(X) -> (X =:= "src") end, L)
	       end,
    % When using source files, do not require ebin/ to be there
    Ebin = case os:getenv("GEAS_USE_SRC") of
		   		false -> lists:any(fun(X) -> (X =:= "ebin") end, L) ;
		   		"0"   -> lists:any(fun(X) -> (X =:= "ebin") end, L) ;
		   		"1"   -> true
	       end,
    case ((Src =:= true) or (Ebin =:= true)) of
        true  -> ok ;
        false -> throw("Invalid Erlang project : "++Dir2) , error
    end.

%%-------------------------------------------------------------------------
%% @doc Get changelog file if any
%% return the name of the file found, if exists, otherwise 'undefined'.
%% @end
%%-------------------------------------------------------------------------
-spec get_changelog() -> list() | undefined .

get_changelog() -> 
   case filelib:wildcard("*") of
      [] -> undefined ; % strange, but...
      L  -> case lists:filter(fun(X) -> (string:to_lower(X) =:= "changelog") end, L) of
               []   -> undefined ;
               [C]  -> C ;
               _    -> undefined % Found more than one
            end
   end.

%%-------------------------------------------------------------------------
%% @doc Get releasenotes file if any
%% return the name of the file found, if exists, otherwise 'undefined'.
%% @end
%%-------------------------------------------------------------------------
-spec get_releasenotes() -> list() | undefined .

get_releasenotes() ->
   case filelib:wildcard("*") of
      [] -> undefined ; % strange, but...
      L  -> case lists:filter(fun(X) -> ((string:to_lower(X) =:= "releasenote") or
                                          (string:to_lower(X) =:= "releasenotes")) end, L) of
                  []   -> undefined ;
                  [R]  -> R ;
                  _    -> undefined % Found more than one
            end
   end.

%%-------------------------------------------------------------------------
%% @doc Test if the project use a C driver
%% @end
%%-------------------------------------------------------------------------
-spec is_using_driver(list()) -> boolean().

is_using_driver(Dir) -> 
   case filelib:is_dir(Dir) of
         true  -> % Empty ?
                  case filelib:wildcard("*") of
                     [] -> false ;
                     _  -> true
                  end ;
         false -> false
   end.