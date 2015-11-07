%%%-------------------------------------------------------------------
%%% File:      geas.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2014 crownedgrouse.com
%%% @doc  
%%% Guess Erlang Application Scattering
%%% @end  
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2014-09-07
%%%-------------------------------------------------------------------
-module(geas).
-author("Eric Pailleau <geas@crownedgrouse.com>").

-export([info/1, what/1]).
-export([release_infos/1, release_infos/2]).
-export([release_diff/2, release_diff_yaml/2, release_diff_files/2, release_diff_yaml_files/2]).

-export([relinfo/1, relinfo/2, reldiff/4]).

-define(COMMON_DIR(Dir),
            AppFile   = get_app_file(Dir),
            % Informations inventory
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
%% @end
%%-------------------------------------------------------------------------
-spec info(list()) -> tuple().

info(Dir) when is_list(Dir) -> 
        {ok, CurPwd} = file:get_cwd(),
        try
            % Fatal tests
            is_valid_dir(Dir),
            is_valid_erlang_project(Dir),
            % Variables
            EbinDir   = filename:join(Dir, "ebin"),
            CsrcDir   = filename:join(Dir, "c_src"),
            Driver    = is_using_driver(CsrcDir),
            ?COMMON_DIR(EbinDir) ,
            % Commands to be done in Dir
            ok = file:set_cwd(Dir),

            VCS       = get_vcs(),
            VcsVsn    = get_vcs_version(VCS),
            VcsUrl    = get_vcs_url(VCS),

            Maint     = get_maintainer(VCS),
            CL        = get_changelog(),
            RN        = get_releasenotes(),

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
             {author, Author},             
             {vcs, {VCS, VcsVsn, VcsUrl}},
             {maintainer, Maint},
             {changelog, CL},
             {releasenotes, RN},
             {driver, Driver}]}
       catch
           throw:Reason -> {error, Reason}
       after
           ok = file:set_cwd(CurPwd)
       end.

%%-------------------------------------------------------------------------
%% @doc Return infos on .beam file(s) only
%% @end
%%-------------------------------------------------------------------------
-spec what(list()) -> tuple().

what(Dir) when is_list(Dir) -> 
        {ok, CurPwd} = file:get_cwd(),
        try
            Output = case filelib:is_regular(Dir) of  
                        true  -> what_beam(Dir);
                        false -> what_dir(Dir)
                     end,
            Output
        catch
           throw:Reason -> {error, Reason}
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
                 {author, Author}]}.

%%-------------------------------------------------------------------------
%% @doc what/1 on a single beam file
%% @end
%%-------------------------------------------------------------------------
-spec what_beam(list()) -> tuple().

what_beam(File) ->         
            {ok,{Name,[Version]}} = beam_lib:version(File),   
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
                 {author, Author}]}.

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
    {ok, L} = file:list_dir(Dir),
    Src  = lists:any(fun(X) -> (X =:= "src") end, L),
    Ebin = lists:any(fun(X) -> (X =:= "ebin") end, L),
    case ((Src =:= true) and (Ebin =:= true)) of
        true  -> ok ;
        false -> throw("Invalid Erlang project : "++Dir) , error
    end.

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
-spec get_app_type(list(), list()) -> otp | app | lib | esc .

get_app_type(AppFile, Ebindir) ->
        % 'otp' ?
        % Search 'mod' in .app file
        {ok,[{application, App, L}]} = file:consult(AppFile),
        case lists:keyfind(mod, 1, L) of
             {mod, {Callback, _}} -> 
                     % start/2 and stop/1 ?
                     {ok,{_,[{exports, L2}]}} = beam_lib:chunks(filename:join(Ebindir, Callback), [exports]),
                     case (lists:member({start, 2}, L2) and 
                           lists:member( {stop, 1}, L2)) of
                            true  -> otp ;
                            false -> lib   % Strange but... anyway.
                     end;
             false -> % 'app' ?
                      {ok,{_,[{exports, L3}]}} = beam_lib:chunks(filename:join(Ebindir, atom_to_list(App)), [exports]),
                      case ((lists:member({start, 0}, L3) or lists:member({start, 1}, L3)) and
                            (lists:member( {stop, 0}, L3) or lists:member( {stop, 1}, L3))) of
                                  true  -> app ;
                                  false -> % 'escript' ?
                                           case lists:member({main, 1}, L3) of
                                                true  -> esc ;
                                                false -> lib 
                                           end  
                      end
        end.

%%-------------------------------------------------------------------------
%% @doc  get type from file
%% @end
%%-------------------------------------------------------------------------
-spec get_app_type_beam(list()) -> atom().

get_app_type_beam(File) ->
                 % start/2 and stop/1 ?
                     {ok,{_,[{exports, L}]}} = beam_lib:chunks(File, [exports]),
                     case (lists:member({start, 2}, L) and 
                           lists:member( {stop, 1}, L)) of
                            true  -> otp ;
                            false -> case ((lists:member({start, 0}, L) or lists:member({start, 1}, L)) and
                                           (lists:member( {stop, 0}, L) or lists:member( {stop, 1}, L))) of
                                            true  -> app ;
                                            false -> % 'escript' ?
                                                     case lists:member({main, 1}, L) of
                                                        true  -> esc ;
                                                        false -> lib 
                                                    end  
                                    end
                     end .

%%-------------------------------------------------------------------------
%% @doc Find .app filename in ebin 
%% @end
%%-------------------------------------------------------------------------
-spec get_app_file(list()) -> list().

get_app_file(Dir) -> case filelib:wildcard("*.app", Dir) of
                        []    -> throw("Application Resource File (.app) not found. Aborting."),
                                 "" ;
                        [App] -> filename:join(Dir, App) ;
                        _     -> throw("More than one .app file found in : "++ Dir), "" 
                     end.
                        
%%-------------------------------------------------------------------------
%% @doc 
%% os:type() -> {Osfamily, Osname}
%% Types:
%%       Osfamily = unix | win32 | ose
%%       Osname = atom()
%%
%% os:version() -> VersionString | {Major, Minor, Release}
%% @end
%%-------------------------------------------------------------------------
-spec get_os() -> tuple().

get_os() -> {A, B} = os:type(),
            V = case os:version() of
                    {X, Y, Z} -> integer_to_list(X)++"."++integer_to_list(Y)++
                                 "."++integer_to_list(Z) ;
                    C         -> C
                end,
            {A, B, V}.

%%-------------------------------------------------------------------------
%% @doc Get the VCS of the project
%% 
%% Vcs-Arch,               Hum... Who cares ?
%% Vcs-Bzr (Bazaar),       directory : ../trunk ! 
%% Vcs-Cvs,                directory : CVS
%% Vcs-Darcs,              directory : _darcs
%% Vcs-Git,                directory : .git
%% Vcs-Hg (Mercurial),     directory : .hg
%% Vcs-Mtn (Monotone),     directory : _MTN
%% Vcs-Svn (Subversion)    directory : ../trunk !
%% @end
%%-------------------------------------------------------------------------
-spec get_vcs() -> atom().

get_vcs() -> try
                    case filelib:is_dir(".git") of
                        true  -> throw(git) ;
                        false -> false
                    end,
                    case filelib:is_dir(".hg") of
                        true  -> throw(hg) ;
                        false -> false
                    end,
                    case filelib:is_dir("_darcs") of
                        true  -> throw(darcs) ;
                        false -> false
                    end,
                    case filelib:is_dir("_MTN") of
                        true  -> throw(mtn) ;
                        false -> false
                    end,
                    case filelib:is_dir("CVS") of
                        true  -> throw(cvs) ;
                        false -> false
                    end,
                    case filelib:is_dir("../trunk") of
                        false -> throw(undefined) ;
                        true  -> case os:find_executable("bzr") of
                                      false -> case os:find_executable("svn") of
                                                    false -> throw(undefined) ; % strange...
                                                    _     -> throw(svn)
                                               end;
                                      _     -> case os:find_executable("svn") of
                                                    false -> throw(bzr) ;
                                                    _     -> throw(undefined) % cannot guess
                                               end
                                 end
                    end,
                    undefined
                catch
                    throw:VCS -> VCS
                end.

%%-------------------------------------------------------------------------
%% @doc Get VCS version / branch  TODO
%% @end
%%-------------------------------------------------------------------------
-spec get_vcs_version(atom()) -> string().

get_vcs_version(git)   -> string:strip(os:cmd("git log -1 --format='%H'"), right, $\n);
get_vcs_version(hg)    -> "TODO";
get_vcs_version(darcs) -> "TODO";
get_vcs_version(mtn)   -> "TODO";
get_vcs_version(cvs)   -> "TODO";
get_vcs_version(bzr)   -> "TODO";
get_vcs_version(svn)   -> "TODO";
get_vcs_version(_)     -> "".

%%-------------------------------------------------------------------------
%% @doc Get maintainer  TODO
%% @end
%%-------------------------------------------------------------------------
-spec get_maintainer(atom()) -> string().

get_maintainer(git)   -> string:strip(os:cmd("git config --get user.name"), right, $\n)++
                        "  <" ++ string:strip(os:cmd("git config --get user.email"), right, $\n)++ ">";
get_maintainer(hg)    -> "TODO";
get_maintainer(darcs) -> "TODO";
get_maintainer(mtn)   -> "TODO";
get_maintainer(cvs)   -> "TODO";
get_maintainer(bzr)   -> "TODO";
get_maintainer(svn)   -> "TODO";
get_maintainer(_)     -> "".

%%-------------------------------------------------------------------------
%% @doc Get VCS URL TODO
%% @end
%%-------------------------------------------------------------------------
-spec get_vcs_url(atom()) -> list().

get_vcs_url(git) -> vcs_git_translate(os:cmd("git config remote.origin.url"));
get_vcs_url(_) -> "".

%%-------------------------------------------------------------------------
%% @doc Translate URL from config to public URL 
%% @end
%%-------------------------------------------------------------------------
-spec vcs_git_translate(list()) -> list().

vcs_git_translate("https://" ++ A)   -> string:strip("https://"++A, right, $\n);
vcs_git_translate("git://" ++ A)     -> string:strip("https://"++A, right, $\n);
vcs_git_translate("git@" ++ A)       -> string:strip("https://"++A, right, $\n);
vcs_git_translate("file://" ++ _)    -> local;
vcs_git_translate(_)                 -> undefined.

%%-------------------------------------------------------------------------
%% @doc Get changelog file if any
%% return the name of the file found, if exists, otherwise 'undefined'. 
%% @end
%%-------------------------------------------------------------------------
-spec get_changelog() -> list() | undefined .

get_changelog() -> case filelib:wildcard("*") of
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

get_releasenotes() -> case filelib:wildcard("*") of
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

is_using_driver(Dir) -> case filelib:is_dir(Dir) of
                            true  -> % Empty ?
                                     case filelib:wildcard("*") of
                                          [] -> false ;
                                          _  -> true
                                     end ;
                            false -> false
                        end.

%%-------------------------------------------------------------------------
%% @doc 
%% erlang:system_info({wordsize, internal}).
%%    Returns the size of Erlang term words in bytes as an integer, 
%%    i.e. on a 32-bit architecture 4 is returned, 
%%    and on a pure 64-bit architecture 8 is returned. 
%%    On a halfword 64-bit emulator, 4 is returned, as the Erlang terms are 
%%    stored using a virtual wordsize of half the system's wordsize.
%% {wordsize, external}
%%    Returns the true wordsize of the emulator, i.e. the size of a pointer, 
%%    in bytes as an integer. 
%%    On a pure 32-bit architecture 4 is returned, on both a halfword and 
%%    pure 64-bit architecture, 8 is returned.
%%
%%    internal | external |  architecture
%% ------------+----------+-----------------------------
%%       4     |    4     |  32-bit
%%       8     |    8     |  64-bit
%%       4     |    8     |  halfword 64-bit emulator
%% See also :
%% dpkg-architecture -L
%% @end
%%-------------------------------------------------------------------------
-spec get_word() -> integer().

get_word() -> N = erlang:system_info({wordsize, internal}) + 
                  erlang:system_info({wordsize, external}),
              case N of
                   8 -> 32 ;
                  16 -> 64 ;
                  12 -> 64 
              end.

%%-------------------------------------------------------------------------
%% @doc Get arch of native compiled modules, or local architecture otherwise
%% @end
%%-------------------------------------------------------------------------
-spec get_arch(boolean(), term()) -> atom().

get_arch(false, _) -> map_arch(erlang:system_info(hipe_architecture));

get_arch(true, EbinDir)  -> Beams = filelib:wildcard("*.beam", EbinDir),
                            [C] = lists:flatmap(fun(F) -> [get_arch_from_file(filename:join(EbinDir, F))] end, Beams),
                            C.

%%-------------------------------------------------------------------------
%% @doc Get arch of a native compiled module
%% @end
%%-------------------------------------------------------------------------
-spec get_arch_from_file(list()) -> atom().

get_arch_from_file(File) -> Bn = filename:rootname(File, ".beam"),
                            [{file,_}, {module,_}, {chunks,C}] = beam_lib:info(Bn),
                            Fun = fun({X, _, _}) -> case X of
                                                "Atom" -> true ;
                                                "Code" -> true ;
                                                "StrT" -> true ;
                                                "ImpT" -> true ;
                                                "ExpT" -> true ;
                                                "FunT" -> true ;
                                                "LitT" -> true ;
                                                "LocT" -> true ;
                                                "Attr" -> true ;
                                                "CInf" -> true ;
                                                "Abst" -> true ;
                                                "Line" -> true ;
                                                _      -> false 
                                            end end,
                            case lists:dropwhile(Fun, C) of
                                 [] ->  get_arch(false, File) ;
                                 [{H, _, _}] -> case H of
                                                    "HS8P" -> ultrasparc ;
                                                    "HPPC" -> powerpc ;
                                                    "HP64" -> ppc64 ;
                                                    "HARM" -> arm ;
                                                    "HX86" -> x86 ;
                                                    "HA64" -> x86_64 ;
                                                    "HSV9" -> ultrasparc ;
                                                    "HW32" -> x86
                                                end
                            end.

%%-------------------------------------------------------------------------
%% @doc Map internal architecture atom to something else if needed
%% @end
%%-------------------------------------------------------------------------
-spec map_arch(atom()) -> atom().

map_arch(X) -> X.


%%-------------------------------------------------------------------------
%% @doc Check if something is compiled native
%% @end
%%-------------------------------------------------------------------------
-spec is_native(list()) -> boolean().

is_native(EbinDir) ->  Beams = filelib:wildcard("*.beam", EbinDir),
                       Check = lists:flatmap(fun(F) -> [is_native_from_file(filename:join(EbinDir, F))] end, Beams),
                       lists:any(fun(X) -> (X =:= true) end, Check).

%%-------------------------------------------------------------------------
%% @doc Check if a beam file is compiled native
%% See also : code:is_module_native(module). 
%% @end
%%-------------------------------------------------------------------------
-spec is_native_from_file(list()) -> boolean().

is_native_from_file(File) ->   Bn = filename:rootname(File, ".beam"),
                               {ok,{_,[{compile_info, L}]}} = beam_lib:chunks(Bn, [compile_info]),
                               {options, O} = lists:keyfind(options, 1, L),
                               lists:member(native, O).

%%-------------------------------------------------------------------------
%% @doc Get compile module version
%% @end
%%-------------------------------------------------------------------------
-spec get_compile_version(list()) -> list().

get_compile_version(File) ->   Bn = filename:rootname(File, ".beam"),
                               {ok,{_,[{compile_info, L}]}} = beam_lib:chunks(Bn, [compile_info]),
                               {version, E} = lists:keyfind(version, 1, L),
                               E.

%%-------------------------------------------------------------------------
%% @doc Get application name, version, desc from .app file in ebin/ directory
%% @end
%%-------------------------------------------------------------------------
-spec get_app_infos(list()) -> tuple().

get_app_infos(File) ->  {ok,[{application, App, L}]} = file:consult(File),
                        VSN  = case lists:keyfind(vsn, 1, L) of
                                    {vsn, V} -> V ;
                                    _        -> undefined
                               end,
                        Desc = case lists:keyfind(description, 1, L) of
                                    {description, D} -> D ;
                                    _                -> undefined
                               end,
                        {App, VSN, Desc}.


%%-------------------------------------------------------------------------
%% @doc Get application creation date
%% @end
%%-------------------------------------------------------------------------
-spec get_date(list()) -> list().

get_date(File) ->  Bn = filename:rootname(File, ".beam"),
                   {ok,{_,[{compile_info, L}]}} = beam_lib:chunks(Bn, [compile_info]),
                   {time, T} = lists:keyfind(time, 1, L),
                   T.

%%-------------------------------------------------------------------------
%% @doc Get application author
%% @end
%% beam_lib:chunks("/home/eric/git/swab/ebin/swab", [attributes]).
%% {ok,{swab,[{attributes,[{author,"Eric Pailleau <swab@crownedgrouse.com>"},
%%                         {vsn,[29884121306770099633619336282407733599]}]}]}}
%%-------------------------------------------------------------------------
-spec get_author(list()) -> list() | undefined.

get_author(File) -> Bn = filename:rootname(File, ".beam"),
                    {ok,{_,[{attributes, L}]}} = beam_lib:chunks(Bn, [attributes]),
                    A = case lists:keyfind(author, 1, L) of
                                {author, B} -> B ;
                                _           -> undefined
                        end,
                    A.

%%-------------------------------------------------------------------------
%% @doc Get {min, recommanded, max} Erlang version from erts version
%% Look into https://github.com/erlang/otp/blob/maint/lib/compiler/vsn.mk
%%-------------------------------------------------------------------------
get_erlang_version("6.0.1")     -> {"18.1", "18.1", "18.1"};
get_erlang_version("6.0")       -> {"18.0-rc2", "18.0-rc2", "18.0-rc2"};
get_erlang_version("5.0.4")     -> {"17.5", "17.5.3", "17.5.3"};
get_erlang_version("5.0.3")     -> {"17.4", "17.4.1", "18.0-rc1"};
get_erlang_version("5.0.2")     -> {"17.3", "17.3.4", "17.3.4"};
get_erlang_version("5.0.1")     -> {"17.1", "17.2.2", "17.2.2"};
get_erlang_version("5.0")       -> {"17.0", "17.0.2", "17.0.2"};
get_erlang_version("4.9.4")     -> {"R16B03", "R16B03-1", "17.0-rc2"};
get_erlang_version("4.9.3")     -> {"R16B02", "R16B02", "R16B02"};
get_erlang_version("4.9.2")     -> {"R16B01", "R16B01", "R16B01"};
get_erlang_version("4.9.1")     -> {"R16B", "R16B", "R16B"};
get_erlang_version("4.8.2")     -> {"R15B02", "R15B03-1", "R15B03-1"};
get_erlang_version("4.8.1")     -> {"R15B01", "R15B01", "R15B01"};
get_erlang_version("4.8")       -> {"R15B", "R15B", "R15B"};
get_erlang_version("4.7.5.pre") -> {"R15A", "R15A", "R15A"};
get_erlang_version("4.7.5")     -> {"R14B04", "R14B04", "R14B04"};
get_erlang_version("4.7.4")     -> {"R14B03", "R14B03", "R14B03"};
get_erlang_version("4.7.3")     -> {"R14B02", "R14B02", "R14B02"};
get_erlang_version("4.7.2")     -> {"R14B01", "R14B01", "R14B01"};
get_erlang_version("4.7.1")     -> {"R14B", "R14B", "R14B"};
get_erlang_version("4.7")       -> {"R14B", "R14B", "R14B"};
get_erlang_version("4.6.5")     -> {"R13B04", "R13B04", "R13B04"};
get_erlang_version("4.6.4")     -> {"R13B03", "R13B03", "R13B03"};
get_erlang_version(_)           -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Guessing the minimal Erlang release usable with a .beam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------
%% @doc Return Erlang Release info
%%-------------------------------------------------------------------------

release_infos(Rel) -> R = [{release, list_to_atom(Rel)},
                           {otp_release, list_to_atom(erlang:system_info(otp_release))}, 
                           {version, list_to_atom(erlang:system_info(version))},
                           {driver_version, list_to_atom(erlang:system_info(driver_version))},
                           {nif_version, list_to_atom(case catch erlang:system_info(nif_version) of
                                                           {'EXIT', _} -> "undefined";
                                                           V           -> V
                                                      end)}],
                      MF = release_fa(),
                      lists:flatten([R, MF]) .

release_infos(Rel, yaml) -> yamlize(release_infos(Rel));

release_infos(yaml, File) ->  {ok, Io} = file:open(File, [write]),
                              yamlize(release_infos(filename:basename(File)), Io),
                              file:close(Io);

release_infos(term, File) ->  {ok, Io} = file:open(File, [write]),
                              io:format(Io,"~p.~n",[release_infos(filename:basename(File))]),
                              file:close(Io).

%%-------------------------------------------------------------------------
%% @doc List module:function/arity of Erlang Release
%%-------------------------------------------------------------------------

release_fa() ->    {ok, AppsVsn} = file:list_dir(code:lib_dir()),
                   Apps = lists:map(fun(A) -> [H | _] = string:tokens(A,[$-]), list_to_atom(H) end, AppsVsn),
                   lists:foreach(fun(A) -> application:load(A),
                                           case application:get_key(A,modules) of
                                                {ok, List} -> lists:foreach(fun(X) -> catch X:module_info() end, List) ;
                                                undefined  -> ok
                                           end
                                 end, Apps),
                   M = lists:sort(lists:delete(?MODULE,erlang:loaded())),
                   % For each module get the functions and arity
                   MF = lists:map(fun(X) ->  {X, lists:map(fun({F, A}) -> 
                                                            list_to_atom(atom_to_list(F)++"/"++integer_to_list(A)) 
                                                           end, lists:sort(X:module_info(exports)))} 
                                  end, M),
                   {mfa, MF}.

%%-------------------------------------------------------------------------
%% @doc Release difference from two Release infos files
%%-------------------------------------------------------------------------

release_diff_files(F1, F2) ->   {ok, [AT]} = file:consult(F1),
                                {ok, [BT]} = file:consult(F2),
                                release_diff(AT, BT).

%%-------------------------------------------------------------------------
%% @doc Release difference from two files returned as YAML
%%-------------------------------------------------------------------------

release_diff_yaml_files(F1, F2) ->   {ok, [AT]} = file:consult(F1),
                                     {ok, [BT]} = file:consult(F2),
                                     release_diff_yaml(AT, BT).

%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations
%%-------------------------------------------------------------------------

release_diff(R1, R2) -> % List new/deleted modules
                        {NM, RM} = diff_modules(R1, R2),
                        % List new/deleted functions in common modules
                        {_, From} = lists:keyfind(release, 1 , R1),
                        {_, To} = lists:keyfind(release, 1 , R2),
                        {NFCM, RFCM } = diff_funcs(R1, R2),
                        [{from, From}, {to, To}
                        ,{modules, [{new, NM}, {removed, RM}]}
                        ,{functions, [{new, NFCM}, {removed, RFCM}]}
                        ].

%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations in YAML
%%-------------------------------------------------------------------------

release_diff_yaml(R1, R2) -> yamlize(release_diff(R1, R2)). 

%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations in YAML file
%%-------------------------------------------------------------------------
release_diffs(yaml, Rel1, Rel2, File) ->  {ok, Io} = file:open(File, [write]),
                                          yamlize(release_diff(Rel1, Rel2), Io),
                                          file:close(Io);
%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations in term file
%%-------------------------------------------------------------------------
release_diffs(term, Rel1, Rel2, File) ->  {ok, Io} = file:open(File, [write]),
                                          io:format(Io,"~p~n",[release_diff(Rel1, Rel2)]),
                                          file:close(Io).
%%-------------------------------------------------------------------------
%% @doc List modules difference between two releases infos
%%-------------------------------------------------------------------------

diff_modules(R1, R2) ->  {mfa, M1} = lists:keyfind(mfa, 1, R1),
                         {mfa, M2} = lists:keyfind(mfa, 1, R2),
                         LM1 = lists:flatmap(fun({X, _}) -> [X] end, M1),
                         LM2 = lists:flatmap(fun({X, _}) -> [X] end, M2),
                         {LM2 -- LM1, LM1 -- LM2}.

%%-------------------------------------------------------------------------
%% @doc List functions difference between two releases infos
%%-------------------------------------------------------------------------

diff_funcs(R1, R2) -> 
                        {mfa, M1} = lists:keyfind(mfa, 1, R1),
                        {mfa, M2} = lists:keyfind(mfa, 1, R2),

                        NF = lists:flatmap(fun({X, L1}) -> case lists:keyfind(X, 1 , M2) of
                                                            false   -> [] ;
                                                            {_, L2} -> D = L2 -- L1,
                                                                       case D of
                                                                            [] -> [];
                                                                            _  -> [{X, D}]
                                                                       end
                                                           end
                                           end, M1),
                        RF = lists:flatmap(fun({X, L2}) -> case lists:keyfind(X, 1 , M1) of
                                                            false   -> [] ;
                                                            {_, L1} -> D = L1 -- L2,
                                                                       case D of
                                                                            [] -> [];
                                                                            _  -> [{X, D}]
                                                                       end
                                                           end
                                           end, M2),
                        {NF, RF}.


%%-------------------------------------------------------------------------
%% @doc YAMLize an Erlang Term
%%-------------------------------------------------------------------------

yamlize(X) -> yamlize(X, standard_io ).

yamlize(X, Io) -> io:format(Io, "%YAML 1.2~n---~n", []),
                  yamlize(X, -1, Io),
                  io:format(Io, "...~n", []) .

yamlize(A, D, Io) when is_atom(A) 
                  -> io:format(Io,"~s- ~s~n",[string:copies("   ", D), A]);

yamlize({A, B}, D, Io) when is_list(B) 
                       -> io:format(Io,"~s~s:~n",[string:copies("   ", D), A]),
                          yamlize(B, D, Io);

yamlize({A, B}, D, Io) -> io:format(Io,"~s~s: ~s~n",[string:copies("   ", D ) , A, B]);

yamlize([H | T], D, Io) when is_atom(H) 
                        -> yamlize(H, D, Io),
                           yamlize(T, D, Io);

yamlize([H | T], D, Io) -> yamlize(H, D + 1, Io),
                           lists:map(fun(B) -> yamlize(B, D + 1, Io) end, T);

yamlize([], D, Io) -> ok.

%%-------------------------------------------------------------------------
%% @doc Create relinfo files in priv directory
%%-------------------------------------------------------------------------

relinfo([Rel, Dir]) -> relinfo(Rel, Dir).

relinfo(Rel, Dir) -> relinfo_term(Rel, Dir),
                     relinfo_yaml(Rel, Dir).

relinfo_term(Rel, Dir) -> DirTerm = filename:join(Dir,"term"),
                          filelib:ensure_dir(filename:join(DirTerm,"fakedir")),
                          Target = filename:join(DirTerm, Rel),
                          release_infos(term, Target).

relinfo_yaml(Rel, Dir) -> DirYaml = filename:join(Dir,"yaml"),
                          filelib:ensure_dir(filename:join(DirYaml,"fakedir")),
                          Target = filename:join(DirYaml, Rel),
                          release_infos(yaml, Target).

%%-------------------------------------------------------------------------
%% @doc Create relinfo files in priv directory
%%-------------------------------------------------------------------------

reldiff([{Rel1, Rel2}, DirSource, Dir]) -> reldiff(content(filename:join(DirSource, Rel1)), content(filename:join(DirSource, Rel2)), Dir).

reldiff(Rel1, Rel2, DirSource, Dir) -> reldiff([{Rel1, Rel2}, DirSource, Dir]).

reldiff(Rel1, Rel2, Dir) -> reldiff_term(Rel1, Rel2, Dir),
                            reldiff_yaml(Rel1, Rel2, Dir).

reldiff_term(Rel1, Rel2, Dir) -> DirTerm = filename:join(Dir,"term"),
                                 filelib:ensure_dir(filename:join(DirTerm,"fakedir")),
                                 {_, From} = lists:keyfind(release, 1 , Rel1),
                                 {_, To} = lists:keyfind(release, 1 , Rel2),
                                 Target = filename:join(DirTerm, atom_to_list(From) ++ "~" ++ atom_to_list(To)),
                                 release_diffs(term, Rel1, Rel2, Target).

reldiff_yaml(Rel1, Rel2, Dir) -> DirYaml = filename:join(Dir,"yaml"),
                                 filelib:ensure_dir(filename:join(DirYaml,"fakedir")),
                                 {_, From} = lists:keyfind(release, 1 , Rel1),
                                 {_, To} = lists:keyfind(release, 1 , Rel2),
                                 Target = filename:join(DirYaml, atom_to_list(From) ++ "~" ++ atom_to_list(To)),
                                 release_diffs(yaml, Rel1, Rel2, Target).

content(X) ->  {ok, [Res]} = file:consult(X), Res.



