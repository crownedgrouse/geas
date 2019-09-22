%%-------------------------------------------------------------------------
%% @doc Get application type (escript / otp / app / lib )
%% esc : (escript) Contain main/1 in exported functions.
%% otp : Contains 'mod' in .app file
%%       {application, ch_app,
%%       [{mod, {ch_app,[]}}]}.
%%       And start/2 and stop/1 in exported functions.
%% app : Contain a start/0 or start/1 and a stop/0 or stop/1 .
%% lib : library application (one or several modules).
%% @end
%%-------------------------------------------------------------------------
-spec get_app_type(list() | atom(), list()) -> otp | app | lib | esc .

get_app_type(not_regular_project, _) -> lib ;

get_app_type(AppFile, Ebindir) ->
   % 'otp' ?
   % Search 'mod' in .app file
   {ok,[{application, App, L}]} = file:consult(AppFile),
   case lists:keyfind(mod, 1, L) of
         {mod, {Callback, _}} ->
            Beam = filename:join(Ebindir, Callback),
            case filelib:is_regular(Beam ++ ".beam") of
                  true -> % start/2 and stop/1 ?
                           {ok,{_,[{exports, L2}]}} = beam_lib:chunks(Beam, [exports]),
                           case (lists:member({start, 2}, L2) and
                           lists:member( {stop, 1}, L2)) of
                                 true  -> otp ;
                                 false -> lib   % Strange but... anyway.
                           end;
                  false -> mis
            end;
         false -> % 'app' ?
            Beam = filename:join(Ebindir, atom_to_list(App)),
            case filelib:is_regular(Beam ++ ".beam") of
               true -> {ok,{_,[{exports, L3}]}} = beam_lib:chunks(Beam, [exports]),
                        case ((lists:member({start, 0}, L3) or lists:member({start, 1}, L3)) and
                           (lists:member( {stop, 0}, L3) or lists:member( {stop, 1}, L3))) of
                                 true  -> app ;
                                 false -> % 'escript' ?
                                          case lists:member({main, 1}, L3) of
                                                true  -> esc ;
                                                false -> lib
                                          end
                        end;
               false -> SrcDir = filename:join(filename:dirname(Ebindir), "src"),
               SrcFile = filename:join(SrcDir, atom_to_list(App) ++ ".erl"),
               Form = get_abstract(SrcFile, src),
               store_exported(Form),
               case lists:keyfind(App, 1, get(geas_exports)) of
                  false   -> mis ;
                  {_, main, 1} -> esc ;
                  _        -> lib
               end
         end
   end.



%%-------------------------------------------------------------------------
%% @doc Find .app filename in ebin
%% @end
%%-------------------------------------------------------------------------
-spec get_app_file(list()) -> list() | atom().

get_app_file(Dir) -> 
   Conf = get_config(),
   Check = Conf#config.use_src ,
   case ( Check
            and filelib:is_dir(filename:join(Dir, "ebin"))
            and (length(filelib:wildcard("*.beam", filename:join(Dir, "ebin"))) > 0 ) 
            %or  filelib:is_dir(filename:join(Dir, "_build")) ) 
      ) of
      true ->
            case filelib:wildcard("*.app", Dir) of
                     []    -> %throw("Application Resource File (.app) not found. Aborting."),
                              ?LOG(geas_logs, {error, no_app_file_found , Dir}),
                              "" ;
                     [App] -> filename:join(Dir, App) ;
                     _     -> %throw("More than one .app file found in : "++ Dir), 
                              ?LOG(geas_logs, {error, many_app_file_found , Dir}),
                              ""
            end;
      false ->  % Workaround for subdirectories in src/
            DirSrc = case filename:basename(Dir) of
                        "src" -> Dir ;
                        _     -> filename:join(filename:dirname(Dir),"src")
                     end,
            case filelib:wildcard("*.app.src", DirSrc) of
                     []    -> % Some Erlang repo does not come with .app.src
                        % Check there is at least some .erl files !
                        case filelib:wildcard("*.erl", DirSrc) of
                           [] -> throw("Application Resource File (.app.src) not found, neither .erl files. Aborting.") ;
                           _  -> % Try directly in ebin
                                 case filelib:wildcard("*.app", Dir) of
                                          []    -> throw("Not a regular project. Missing .app(.src) file"), not_regular_project ;
                                          [App] -> filename:join(Dir, App) ;
                                          _     -> throw("More than one .app file found in : "++ Dir), ""
                                 end
                        end ;
                     [App] -> filename:join(DirSrc, App) ;
                     _     -> throw("More than one .app.src file found in : "++ DirSrc), ""
            end
   end.
   
%%-------------------------------------------------------------------------
%% @doc Get application name, version, desc from .app file in ebin/ directory
%% @end
%%-------------------------------------------------------------------------
-spec get_app_infos(list()) -> tuple().

get_app_infos(not_regular_project) -> {undefined, undefined, undefined};

get_app_infos(File) ->  
   {ok,[{application, App, L}]} = file:consult(File),
   VSN = case lists:keyfind(vsn, 1, L) of
            {vsn, V} -> V ;
            _  -> 
               ?LOG(geas_logs, {warning, {undefined, vsn}, File}),
               undefined
         end,
   Desc = case lists:keyfind(description, 1, L) of
               {description, D} -> D ;
               _ ->  ?LOG(geas_logs, {warning, {undefined, description}, File}),
                     undefined
          end,
   {App, VSN, Desc}.