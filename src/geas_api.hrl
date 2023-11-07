

-define(COMMON_DIR(Dir),
   AppFile   = get_app_file(Dir),
   % Information inventory
   {AppName, Version , Desc}  = get_app_infos(AppFile),
   AppType   = get_app_type(AppFile, Dir),
   Native    = is_native(Dir),
   Arch      = get_arch(Native, Dir),
   Word      = get_word(),
   AppBeam   = filename:join(Dir, atom_to_list(AppName)++".beam"),
   Date      = get_date(AppBeam),
   Author    = get_author(AppBeam),
   Os        = get_os(),
   %Erts      = get_erts_version(AppBeam),
   Comp      = get_compile_version(AppBeam),
   Erlang    = get_erlang_version(Comp)
).
%%-------------------------------------------------------------------------
%% @doc Return infos after application directory analysis
%% @since 1.0.0
%% @end
%%-------------------------------------------------------------------------
-spec info(list()) -> tuple().

info(Dir) when is_list(Dir) ->
  {ok, CurPwd} = file:get_cwd(),
  put(geas_cwd, CurPwd),
  Conf = get_config(),
  try
      % Fatal tests
      is_valid_dir(Dir),
      is_valid_erlang_project(Dir),
      % Variables
      EbinDir   = filename:join(Dir, "ebin"),
      CsrcDir   = filename:join(Dir, "c_src"),
      SrcDir   = filename:join(Dir, "src"),
      Driver    = is_using_driver(CsrcDir),
      ?COMMON_DIR(EbinDir) ,
      InfoDir = case Conf#config.use_src of
                  false  -> EbinDir ;
                  true   -> SrcDir
                end,
      Compat    = get_erlang_compat(InfoDir),
      % Commands to be done in Dir
      ok = file:set_cwd(Dir),

      VCS       = get_vcs(),
      VcsVsn    = get_vcs_version(VCS),
      VcsUrl    = get_vcs_url(VCS),

      Maint     = get_maintainer(VCS),
      CL        = get_changelog(),
      RN        = get_releasenotes(),
      Current   = case Erlang of
                  {_, Current_, _} -> Current_ ;
                  undefined        -> undefined
                  end,
      Patches   = list_installed_patches(Current),

      {ok,
      [{name, AppName},
       {version, Version},
       {description, Desc},
       {type, AppType},
       {datetime, Date},
       {native, Native},
       {arch, Arch},
       {os, Os},
       {word, Word},
       {compile, Comp},
       {erlang, Erlang},
       {compat, Compat},
       {author, Author},
       {vcs, {VCS, VcsVsn, VcsUrl}},
       {maintainer, Maint},
       {changelog, CL},
       {releasenotes, RN},
       {driver, Driver},
       {patches, Patches}]}
 catch
     _:Reason -> {error, Reason}
 after
     ok = file:set_cwd(CurPwd)
 end.
       
%%-------------------------------------------------------------------------
%% @doc Return infos on .beam file(s) only
%% @since 1.1.0
%% @end
%%-------------------------------------------------------------------------
-spec what(list()) -> tuple().

what(Dir) when is_list(Dir) ->
   {ok, CurPwd} = file:get_cwd(),
   put(geas_cwd, CurPwd),
   try
      Output = case filelib:is_regular(Dir) of
                  true  -> what_beam(Dir);
                  false -> what_dir(Dir)
               end,
      Output
   catch
      _:Reason -> {error, Reason}
   after
      ok = file:set_cwd(CurPwd)
   end.

%%-------------------------------------------------------------------------
%% @doc Return infos on .beam file(s) only
%% @end
%%-------------------------------------------------------------------------
-spec what_dir(list()) -> tuple().

what_dir(Dir) ->
   ?COMMON_DIR(Dir) ,
   Compat    = get_erlang_compat(Dir),
   Current   = case Erlang of
               {_, Current_, _} -> Current_ ;
               undefined        -> undefined
               end,
   Patches   = list_installed_patches(Current),

   {ok,
      [{name, AppName},
      {version, Version},
      {description, Desc},
      {type, AppType},
      {datetime, Date},
      {native, Native},
      {arch, Arch},
      {os, Os},
      {word, Word},
      {compile, Comp},
      {erlang, Erlang},
      {compat, Compat},
      {author, Author},
      {patches, Patches}]}.

%%-------------------------------------------------------------------------
%% @doc what/1 on a single beam file
%% @end
%%-------------------------------------------------------------------------
-spec what_beam(list()) -> tuple().

what_beam(File) ->
   try
      {Name, Version} = case beam_lib:version(File) of
                     {ok,{N,[V]}} -> {N, V} ;
                     _            -> {filename:basename(File, ".beam"), undefined}
                  end,

      AppType   = get_app_type_beam(File),
      Native    = is_native_from_file(File),
      Arch      = get_arch_from_file(File),
      Word      = get_word(),
      Date      = get_date(File),
      Author    = get_author(File),
      Os        = get_os(),
      %Erts      = get_erts_version(AppBeam),
      Comp      = get_compile_version(File),
      Erlang    = get_erlang_version(Comp),
      Compat    = get_erlang_compat_file(File),
      Current   = case Erlang of
                  {_, Current_, _} -> Current_ ;
                  undefined        -> undefined
                  end,
      Patches   = list_installed_patches(Current),
      _BeamComp  = beam_opcode_compat(File),
      {ok,
         [{name, Name},
         {version, Version},
         {type, AppType},
         {datetime, Date},
         {native, Native},
         {arch, Arch},
         {os, Os},
         {word, Word},
         {compile, Comp},
         {erlang, Erlang},
         {compat, Compat},
         {author, Author},
         {patches, Patches},
         {max_opcode, get_beam_max_opcode(File)}]}
   catch
      _:Reason -> {error, Reason}
   end.

%%-------------------------------------------------------------------------
%% @doc Give the offending modules/functions that reduce release window
%% @end
%%-------------------------------------------------------------------------
offending(File) ->
   try
   % check it is a file or exit
      case filelib:is_regular(File) of
         false -> {error, invalid_argument} ;
         true  -> % Apply geas:what on the beam, bring lowest and highest if necessary
                  {ok, L } = geas:what(File),
                  {compat, Compat} = lists:keyfind(compat, 1, L),
                  {A, B, C, D} = Compat,
                  MinOff = case A =/= B of
                                 true -> get_min_offending(B, File) ;
                                 false -> []
                           end,
                  MaxOff = case C =/= D of
                                 true -> get_max_offending(C, File) ;
                                 false -> []
                           end,
                  {ok, {MinOff, MaxOff}}
      end
   catch
      _:Reason -> {error, Reason}
   end.

%%-------------------------------------------------------------------------
%% @doc Give the offending min modules/functions that reduce release window
%% @end
%%-------------------------------------------------------------------------
get_min_offending(Rel, File) ->
   Abs = get_abstract(File),
   Mod = get_module_name_from_file(File),
   Exports = lists:filter(fun({M, _F, _A}) -> (M == Mod) end, get(geas_exports)),
   X = lists:usort(lists:flatten(lists:flatmap(fun(A) -> [get_remote_call(A, Exports)] end, Abs))),
   % From list get the release of offending functions
   R = lists:flatmap(fun(F) -> [{rel_min(F), F}] end , X),
   % Extract only the release we search
   R1 = lists:filter(fun({Relx, _A}) -> (Rel == Relx) end, R),
   %% Extract the function
   [{Rel, lists:flatmap(fun({_, FF}) -> [FF] end, R1)}].

%%-------------------------------------------------------------------------
%% @doc Give the offending max modules/functions that reduce release window
%% @end
%%-------------------------------------------------------------------------
get_max_offending(Rel, File) ->
   Abs = get_abstract(File),
   Mod = get_module_name_from_file(File),
   Exports = lists:filter(fun({M, _F, _A}) -> (M == Mod) end, get(geas_exports)),
   X = lists:usort(lists:flatten(lists:flatmap(fun(A) -> [get_remote_call(A, Exports)] end, Abs))),
   % From list get the release of offending functions
   R = lists:flatmap(fun(F) -> [{rel_max(F), F}] end , X),
   % Extract only the release we search
   R1 = lists:filter(fun({Relx, _A}) -> (Rel == Relx) end, R),
   %% Extract the function
   [{Rel, lists:flatmap(fun({_, FF}) -> [FF] end, R1)}].

%%-------------------------------------------------------------------------
%% @doc Compat output on stdout, mainly for erlang.mk plugin
%% @since 2.0.4
%% @end
%%-------------------------------------------------------------------------

compat(RootDir) -> compat(RootDir, print).

compat(RootDir, deps) -> {_, _, Deps} = compat(RootDir, term),
						 Deps;

compat(RootDir, global) -> {{_, MinGlob, MaxGlob, _}, _, _} = compat(RootDir, term),
						   {?GEAS_MIN_REL, MinGlob, MaxGlob, ?GEAS_MAX_REL};

compat(RootDir, term) ->
   % reset at each run
   put(geas_logs,[]),
   put(geas_calls,[]),
   put(geas_disc,[]),
   put(geas_minrels,[]),
   put(geas_maxrels,[]),
   put(geas_exports,[]),
   put(geas_attributes,[]),
   % Get all .beam (or .erl) files recursively
   Ext = ext_to_search(),

   Dir = case get(geas_caller) of
            'rebar'     -> "_build";
            'erlang.mk' -> "deps";
            _ ->   case filelib:is_dir(filename:join(RootDir, "_build")) of
                     true  -> "_build" ;
                     false -> "deps"
                  end
         end,
   PP = filelib:fold_files(filename:absname(filename:join(RootDir, Dir)), Ext, true,
         fun(X, Y) -> 
            P = filename:dirname(filename:dirname(X)),
            case  lists:member("_rel",re:split(P,"/",[{return,list}])) of
               false -> 
                  case filename:basename(P) of
                     "rebar"   -> Y ; % Exclude rebar from results
                     "geas"    -> Y ; % Exclude geas from results
                     "geas_rebar3" -> Y ; % Exclude geas_rebar3 from results
                     "samovar" -> Y ; % Exclude samovar from results
                     "src"  -> 
                        case lists:member(filename:dirname(P), Y) of
                           true  -> Y ;
                           false -> Y ++ [filename:dirname(P)]
                        end;
                     _  -> 
                        case lists:member(P, Y) of
                           true  -> Y ;
                           false ->  Y ++ [P]
                        end
                  end;
               true -> Y
            end
         end, []),
   % Get all upper project
   Ps = lists:usort(PP),
   ?LOG(geas_logs, {debug, dir_list, Ps }),
   Global = case get(geas_caller) of
               'erlang.mk' -> Ps ++ [filename:absname(RootDir)];
               _ -> Ps
            end,
   Fun = 
      fun(X) ->
         case (catch info(X)) of
            {ok, I} ->
               Compat = lists:keyfind(compat, 1, I),
               {compat,{MinDb, Min, Max, MaxDb}} = Compat,
               N = lists:keyfind(native, 1, I),
               {native, Native} = N,
               A = lists:keyfind(arch, 1, I),
               {arch, Arch} = A,
               ArchString = case Native  of
                                 true -> atom_to_list(Arch) ;
                                 false -> ""
                              end,
               Left  = case (MinDb =:= Min) of
                              true  -> "" ;
                              false -> Min
                        end,
               Right = case (MaxDb =:= Max) of
                              true  -> "" ;
                              false -> Max
                        end,
               Name = case filename:basename(X) of
                           "." -> filename:basename(filename:dirname(X)) ;
                           NN  -> NN
                        end,
               [{Left , ArchString, Right, Name}] ;
            {error, Reason} -> ?LOG(geas_logs, {warning, compat, Reason}), 
                               throw(3),
                               []
         end
      end,
   D = lists:keysort(4, lists:flatmap(Fun, Global)),
   % Get global info
   MinList = lists:usort([?GEAS_MIN_REL] ++ lists:flatmap(fun({X, _, _, _}) -> [X] end, D)),
   MinGlob = highest_version(MinList) ,
   ArchList = lists:usort(lists:flatmap(fun({_, X, _, _}) -> [X] end, D)),
   ArchGlob = safetl(ArchList), % Assuming only one arch localy !
   MaxList = lists:usort([?GEAS_MAX_REL] ++ lists:flatmap(fun({_, _, X, _}) -> [X] end, D)),
   MaxGlob = lowest_version(MaxList) ,
   {{?GEAS_MIN_REL, MinGlob, MaxGlob, ?GEAS_MAX_REL}, ArchGlob, D};

compat(RootDir, print) ->
   compat(RootDir, print, []).

safetl([]) -> [];
safetl(X)  -> tl(X).


%compat(RootDir, print, []) -> compat(RootDir, print);

compat(RootDir, print, Config) ->
   put(geas_exit_code, 0),
   do_log({debug, geas_rebar3, Config}),
   set_config(Config),
   Conf=get_config(),
   do_log({debug, config, Conf}),
   {{_, MinGlob, MaxGlob, _}, ArchGlob, D} = compat(RootDir, term),
   % Display log if needed
   Project = 
      case RootDir of
         "." -> filename:basename(filename:dirname(filename:absname(RootDir)));
         _   -> filename:basename(RootDir)
      end,
   geas:log(),
   c:flush(),
   % Display header
   io:format("   ~-10s            ~-10s ~-20s ~20s~n",[?GEAS_MIN_REL , ?GEAS_MAX_REL, "Geas database", get_version(geas)]),
   io:format("~s~s~n",["---Min--------Arch-------Max--",string:copies("-",50)]),
   lists:foreach(fun({LD, AD, RD, FD}) -> io:format("   ~-10s ~-10s ~-10s ~-20s ~20s~n",[LD, AD, RD, FD, get_version(FD)]) end, D),
   io:format("~80s~n",[string:copies("-",80)]),
   io:format("   ~-10s ~-10s ~-10s ~-20s ~20s~n",[MinGlob , ArchGlob, MaxGlob, "Global project", get_version(Project)]),
   Rels = w2l({?GEAS_MIN_REL, MinGlob, MaxGlob, ?GEAS_MAX_REL}), % Might return empty list, if MinGlob is "higher" than MaxGlob.
   io:format("~n",[]),
   % Always display current version detected and patches found
   Current = get_current_erlang_version(),
   io:format("C : ~ts~n",[Current]),
   Patches = list_installed_patches(Current),
   case Patches of
      [] -> ok ;
      _  -> io:format("P : ~ts~n", [string:join(Patches, " ")])
   end,
   % Display Recommended Erlang release if requested
   case Conf#config.tips of
      false -> ok ;
      true  -> Rec = get_recommanded_patches(Current),
               case Rec of
                  []  -> ok;
                  Rec -> io:format("R : ~ts~n", [string:join(Rec, " ")])
               end
   end,
   case Conf#config.my_rels of
      false -> {ok, SV} = geas_semver:l2s(lists:usort(Rels)),
               io:format("T : ~ts~n",[SV]);
      ""    -> io:format("T : ~ts~n",[string:join(Rels, " ")]);
      _     -> io:format("L : ~ts~n",[string:join(Rels, " ")])
   end,
   case Conf#config.exc_rels of
      false -> ok ;
      Exc   -> case pickup_rel(w2l({?GEAS_MIN_REL, MinGlob, MaxGlob, ?GEAS_MAX_REL}, false), string:tokens(Exc, " ")) of
                  []       -> ok ;
                  InWindow -> io:format("E : ~ts~n",[string:join(InWindow," ")])
               end
   end,
   % Display Discarded release due to known bugs
   case erlang:get(geas_disc) of
      undefined -> ok ;
      Disc      -> 
         case Conf#config.disc_rels of
            true -> % Do not display if outside of window
               DL = in_window(MinGlob, distinct_disc_rels(Disc), MaxGlob),
               case DL of
                  [] -> ok ;
                  DL -> io:format("D : ~ts~n",[string:join(DL," ")])
               end;
            false -> ok
         end
   end,
   %% Set the plugin exit code
   try      
      % Current version is incompatible with release window
      check_current_rel_vs_window(Current, Rels),
      % Check versions against semver range set
      check_window_vs_semver_range(Rels, Conf#config.range),
      % Check versions against semver frame set
      check_window_vs_semver_frame(Rels, Conf#config.frame)
   catch
      _:Exit when is_integer(Exit) 
         ->  put(geas_exit_code, Exit),
             case ( Exit > 0 ) of
               true ->
                  Err = format_error(Exit),
                  ?LOG(geas_logs, {error, Exit, Err});
               false ->
                  ok
             end;
      _:Reason
         -> put(geas_exit_code, -1),
            ?LOG(geas_logs, {error, unexpexted_error, Reason})
   after
      geas:log()
   end,
   ok.

%%-------------------------------------------------------------------------
%% @doc Offending output on stdout, mainly for erlang.mk plugin
%% @end
%%-------------------------------------------------------------------------

guilty(RootDir) ->
   Ext = ext_to_search(),
   Dir = case get(geas_caller) of
         'rebar'     -> "_build";
         'erlang.mk' -> "deps";
         _ ->   case filelib:is_dir(filename:join(RootDir, "_build")) of
                  true  -> "_build" ;
                  false -> "deps"
               end
      end,
   DirS = dir_to_search(),
   Bs1 = filelib:fold_files(filename:join(RootDir, Dir), Ext, true,
                           fun(X, Y) -> Y ++ [X] end, []),
   Bs2 = filelib:fold_files(filename:join(RootDir, DirS), Ext, true,
                           fun(X, Y) -> Y ++ [X] end, []),
   Bs_ = Bs1 ++ Bs2,
   Bs = lists:flatmap(fun(X) -> case lists:member("_rel",re:split(X,"/",[{return,list}])) of
                                    true  -> [] ;
                                    false -> [X]
                                end end, Bs_),
   % Check Offendings for each beam / erl
   Fun =
      fun(X) -> Off = offending(X),
         case Off of
            {ok, []}          -> [] ;
            {ok, {[],[]}}     -> [] ;
            {ok, {[{[],[]}],[]}} -> [] ;
            {ok, {[],[{[],[]}]}} -> [] ;
            {ok,{[{[],[]}],[{[],[]}]}} -> [] ;
            {ok,{Left,Right}} -> [{X, Left, Right}] ;
            _E                -> []
         end
   end,
   All = lists:flatmap(Fun, Bs),
   Fun2 =
      fun({_F, L, R}) ->
         io:format("~n~ts", [_F]) ,
         case L of
            [{Min, MinList}] -> io:format("~n~-10ts", [Min]),
                           LL = lists:flatmap(fun({M, F, A}) ->
                                          [atom_to_list(M) ++ ":" ++
                                             atom_to_list(F) ++ "/" ++
                                             integer_to_list(A)] end, MinList),
                           io:format("~ts~n", [string:join(LL, ", ")]) ;
            _  -> ok
         end,
         case R of
            [{Max, MaxList}] -> io:format("~n~-10ts", [Max]),
                           RR = lists:flatmap(fun({M, F, A}) ->
                                          [atom_to_list(M) ++ ":" ++
                                             atom_to_list(F) ++ "/" ++
                                             integer_to_list(A)] end, MaxList),
                           io:format("~ts~n", [string:join(RR, ", ")]) ;
            _  -> ok
         end
      end,
   lists:foreach(Fun2, lists:usort(All)) .

%%-------------------------------------------------------------------------
%% @doc Translate compat window to list
%% @since 2.0.5
%% @end
%%-------------------------------------------------------------------------

w2l({A, MinRel, MaxRel, B}) -> w2l({A, MinRel, MaxRel, B}, true).

w2l({_, MinRel, MaxRel, _}, Exc) ->
   Conf = get_config(),
   L = geas_db:get_rel_list(),
   Lower = lists:filter(fun(X) -> 
                           case (geas_semver:versionize(X) >= geas_semver:versionize(MinRel) ) of
                                 true  -> true ;
                                 false -> false
                           end
                        end, L),
   Res = lists:filter(fun(X) -> 
                           case ( geas_semver:versionize(X) =< geas_semver:versionize(MaxRel) ) of
                                 true  -> true ;
                                 false -> false
                           end
                        end, Lower),
   Exclude = 
      case Conf#config.exc_rels of
         false -> [] ;
         Excs -> case Exc of
                  true -> string:tokens(Excs, " ") ;
                  _    -> []
               end
      end,
   Local = 
      case Conf#config.my_rels of
         false -> Res -- Exclude;
         ""    -> Res -- Exclude;
         MyRel -> MyRelList = string:tokens(MyRel, " "),
               pickup_rel(MyRelList, Res) -- Exclude
      end,
   case erlang:get(geas_disc) of
      undefined -> Local ;
      Disc  ->
         case Conf#config.disc_rels of
            true  -> Local -- distinct_disc_rels(Disc);
            false -> Local
         end
   end.

%%-------------------------------------------------------------------------
%% @doc Set prefixe for Erlang git tag
%% @end
%%-------------------------------------------------------------------------
git_tag(X) -> 
   case string:substr(X, 1, 1) of
      "R" -> "OTP_" ++ X;
      _   -> "OTP-" ++ X
   end.

%%-------------------------------------------------------------------------
%% @doc Return exit code
%% @end
%%-------------------------------------------------------------------------
exit_code()
   -> get(geas_exit_code).

%%=========================================================================
%%=== Local functions                                                   ===
%%=========================================================================

%%-------------------------------------------------------------------------
%% @doc Analyze compatibility of all beam in directory
%% @end
%%-------------------------------------------------------------------------
-spec get_erlang_compat(list()) -> {list(), list(), list(), list()}.

get_erlang_compat(Dir) ->
   Conf = get_config(),
   NbBeams =  length(filelib:wildcard("*.beam", Dir)),
   Joker = case Conf#config.use_src  of
                  _ when (NbBeams == 0) -> "**/*.erl" ;
                  false -> "*.beam" ;
                  true  -> "**/*.erl"
           end,
   Beams = filelib:wildcard(Joker, Dir),
   Fun = fun(F) ->   
            File = filename:join(Dir,F),
            Comp = get_erlang_compat_beam(File),
            Ign  = case get(geas_ignore) of
                     undefined -> [];
                     I         -> I
                  end,
            Res = case lists:member(File, Ign) of
                     false -> Comp ;
                     true  -> {?GEAS_MIN_REL, ?GEAS_MAX_REL, {File, []}}
                  end,
            [Res] end,
   X = lists:usort(lists:flatmap(Fun, Beams)),
   {MinList, MaxList, DiscListRaw} = lists:unzip3(X),
   DiscList = lists:filter(fun(XX) -> {_, T} = XX, T =/= [] end, DiscListRaw),
   % Get the highest version of Min releases of all beam
   MinR = highest_version(MinList),
   % Get the lowest version of Max releases of all beam
   MaxR = lowest_version(MaxList),
   % Keep memory of releases to discard
   ?STORE(geas_disc, DiscList),
   {?GEAS_MIN_REL , MinR, MaxR, ?GEAS_MAX_REL}.

%%-------------------------------------------------------------------------
%% @doc Analyze compatibility of a beam from file
%% @end
%%-------------------------------------------------------------------------
-spec get_erlang_compat_file(list()) -> {list(), list(), list(), list()}.

get_erlang_compat_file(File) ->
   X = lists:usort(lists:flatmap(fun(F) -> [get_erlang_compat_beam(F)] end, [File])),
   {MinList, MaxList, DiscListRaw} = lists:unzip3(X),
   DiscList = lists:filter(fun(XX) -> {_, T} = XX, T =/= [] end, DiscListRaw),
   % Get the highest version of Min releases of all beam
   MinR = highest_version(MinList),
   % Get the lowest version of Max releases of all beam
   MaxR = lowest_version(MaxList),
   % Keep memory of releases to discard
   ?STORE(geas_disc, DiscList),
   {?GEAS_MIN_REL , MinR, MaxR, ?GEAS_MAX_REL}.

%%-------------------------------------------------------------------------
%% @doc Get module name from file
%% @end
%%-------------------------------------------------------------------------
get_module_name_from_file(File) -> 
   case (filelib:is_regular(File)) of
      true  -> erlang:list_to_atom(filename:basename(File, filename:extension(File)));
      false -> ''
   end.

%%-------------------------------------------------------------------------
%% @doc Analyze compatibility of a beam file
%% @end
%%-------------------------------------------------------------------------
-spec get_erlang_compat_beam(list()) -> {list(), list()}.

get_erlang_compat_beam(File) ->
   % Extract all Erlang MFA in Abstract code
   Abs1 = case get_abstract(File) of
            AA when is_tuple(AA) -> [AA];
            AA -> AA
         end,
   Mod = get_module_name_from_file(File),
   Exports = lists:filter(fun({M, _F, _A}) -> (M == Mod) end, get(geas_exports)),
   X = lists:usort(lists:flatten(lists:flatmap(fun(A) -> [get_remote_call(A, Exports)] end, Abs1))),
   ?STORE(geas_calls, X),
   %io:format("~p~n", [X)])
   % Get the min and max release for each MFA
   % Get the min release compatible
   Min = lists:flatmap(fun(A) -> [{rel_min(A), A}] end, X),
   {MinRelss, _} = lists:unzip(Min),
   MinRels = lists:filter(fun(XX) -> case XX of undefined -> false; [] -> false;_ -> true end end, lists:usort(MinRelss)),
   % Get the max release compatible
   Max = lists:flatmap(fun(A) -> [{rel_max(A), A}] end, X),
   {MaxRelss, _} = lists:unzip(Max),
   MaxRels = lists:filter(fun(XX) -> case XX of undefined -> false; [] -> false; _ -> true end end, lists:usort(MaxRelss)),
   % Get the releases to discard
   Fun = fun(A) -> 
            case rel_disc(A) of
               ok -> [] ;
               D  -> [{A, D}]
            end end,
   DiscRels = lists:usort(lists:flatten(lists:flatmap(Fun, X))),
   Highest = highest_version(MinRels),
   Lowest  = lowest_version(MaxRels),
   % Some code syntax may find a Lowest version higher than Highest version, for instance :
   % ./_build/default/lib/edown/ebin/edown_doclet.beam
   % 18.0      edoc_lib:write_info_file/3
   % 17.5      edoc_lib:write_file/5, edoc_lib:write_info_file/4
   % (Due to a test if a function is existing, otherwise use another one)
   case highest_version(Highest, Lowest) of
                  Highest when (Highest =/= []),(Lowest =/= [])
                           -> ?STORE(geas_ignore, [File]),
                              ?LOG(geas_logs, {debug, {Highest, Lowest}, File}),
                              ?LOG(geas_logs, {warning, ignoring, File});
                  _        -> ok
   end,
   ?STORE(geas_minrels, Max),
   ?STORE(geas_maxrels, Min),
   {Highest, Lowest, {File, DiscRels}}.

%%-------------------------------------------------------------------------
%% @doc Check current Erlang release against project release window
%% @end
%%-------------------------------------------------------------------------
check_current_rel_vs_window(_Current, [] = _Window) ->
   throw(5);
check_current_rel_vs_window(Current, Window)
   -> 
   Start = hd(Window),
   End   = hd(lists:reverse(Window)),
   Range = lists:flatten(">=" ++ Start ++ " <=" ++ End),
   %io:format("~n~p ~p ~p ~p", [Current, Start, End, Range]),
   case geas_semver:check(Current, Range) of
      true  -> 0 ;
      false -> throw(1)
   end.

%%-------------------------------------------------------------------------
%% @doc Check project release window against required semver range
%%      All releases of window have to be inside range
%% @end
%%-------------------------------------------------------------------------
check_window_vs_semver_range(Window, Range)
   when is_list(Range)
   ->  
   ?LOG(geas_logs, {notice, range, Range}),
   case lists:all(fun(Rel) -> geas_semver:check(Rel, Range) end, Window) of
      false -> %
               do_log({error, range, "Range does not match"}),
               throw(2);
      true  -> 0
   end;

check_window_vs_semver_range(_Window, _Range) % No range set
   -> 0.

%%-------------------------------------------------------------------------
%% @doc Check project release window against required semver frame
%%      Frame have to be inside release window
%% @end
%%-------------------------------------------------------------------------
check_window_vs_semver_frame(Window, Frame)
   when is_list(Frame)
   -> 
   ?LOG(geas_logs, {notice, frame, Frame}),
   % Get all releases known at this time
   All = geas_db:get_rel_list(),
   % Extract all releases in the semver Frame
   Ext = lists:filter(fun(Rel) -> geas_semver:check(Rel, Frame) end, All),
   % Check if window is a subset of this
   D1 = Window -- Ext,
   D2 = Ext -- Window,
   %erlang:display({Ext, Window, D1, D2}),
   case ((erlang:length(D1) >= 0) and (erlang:length(D2) == 0)) of
      false -> do_log({error, frame, "Frame does not match"}),
               throw(5);
      true  -> 0
   end; 

check_window_vs_semver_frame(_Window, _Frame) % No frame set
   -> 0.
%%-------------------------------------------------------------------------
%% @doc Format exit code to error string
%% @end
%%-------------------------------------------------------------------------
format_error(0) ->  "Success" ;
format_error(1) ->  "Current Erlang/OTP release is incompatible with project release window" ; 
format_error(2) ->  "Release window do not match the required semver version range" ;
format_error(3) ->  "Beam files are incompatible with current Erlang/OTP release (May need recompilation)" ;
format_error(4) ->  "Maximum opcode is higher in Beam files (May need recompilation)" ;
format_error(5) ->  "The release window does not match the required SemVer version frame" ;
format_error(_) ->  "Unexpected exit code".
