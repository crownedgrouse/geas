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

-export([info/1]).

% beam_lib:version(Beam) ->
%           {ok, {module(), [Version :: term()]}} |
%           {error, beam_lib, chnk_rsn()}

% build_type
% c_compiler_used
% driver_version
% machine
% otp_release
% smp_support
% erlang:system_info(system_architecture).
%  "i686-pc-linux-gnu"
% threads
% version
% 

%    Solaris 8, 9
%        Sparc32
%        Sparc64
%    Solaris 10
%        Sparc32
%        Sparc64
%        x86
%    SuSE Linux/GNU 9.4, 10.1
%        x86 
%    SuSE Linux/GNU 10.0, 10.1, 11.0
%        x86
%        x86_64
%    openSuSE 11.4 (Celadon)
%        x86_64 (valgrind) 
%    Fedora 7
%        PowerPC 
%    Fedora 16
%        x86_64 
%    Gentoo Linux/GNU 1.12.11.1
%        x86 
%    Ubuntu Linux/GNU 7.04, 10.04, 10.10, 11.04, 12.04
%        x86_64 
%    MontaVista Linux/GNU 4.0.1
%        PowerPC 
%    FreeBSD 10.0
%        x86 
%    OpenBSD 5.4
%        x86_64 
%    OS X 10.5.8 (Leopard), 10.7.5 (Lion), 10.9.1 (Mavericks)
%        x86 
%    Windows XP SP3, 2003, Vista, 7
%        x86 
%    Windows 7
%        x86_64 

% Daily Cross Builds:
%    SuSE Linux/GNU 10.1 x86 -> SuSE Linux/GNU 10.1 x86_64
%    SuSE Linux/GNU 10.1 x86_64 -> Linux/GNU TILEPro64

% Daily Cross Build Tests:
%    SuSE Linux/GNU 10.1 x86_64 



% beam_lib:chunks("/home/eric/git/swab/ebin/swab", [compile_info]). 
%{ok,{swab,[{compile_info,[{options,[{outdir,"/home/eric/git/swab/ebin"},
%                                    {i,"/home/eric/git/swab/include"},
%                                    warn_obsolete_guard,warn_shadow_vars,warn_export_vars,
%                                    warn_export_all,debug_info]},
%                          {version,"5.0"},
%                          {time,{2014,9,7,20,49,46}},
%                          {source,"/home/eric/git/swab/src/swab.erl"}]}]}}

% erlang modules {attributes,[{app_vsn,"snmp-4.25.1"}
% erlang modules {ok, C, _}= erl_scan:string(binary_to_list(B)).

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
            AppFile   = get_app_file(EbinDir),
            % Informations inventory
            {AppName, Version , Desc}  = get_app_infos(AppFile),
            AppType   = get_app_type(AppFile, EbinDir),
            Driver    = is_using_driver(CsrcDir),
            Native    = is_native(EbinDir),
            Arch      = get_arch(),
            AppBeam   = filename:join(EbinDir, atom_to_list(AppName)++".beam"),
            Date      = get_date(AppBeam),
            Author    = get_author(AppBeam),
            Os        = get_os(),
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
-spec get_arch() -> integer().

get_arch() -> N = erlang:system_info({wordsize, internal}) + 
                  erlang:system_info({wordsize, external}),
              case N of
                   8 -> 32 ;
                  16 -> 64 ;
                  12 -> 64 
              end.

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
%% beam_lib:chunks("/home/eric/git/swab/ebin/swab", [attributes]).
%% {ok,{swab,[{attributes,[{author,"Eric Pailleau <swab@crownedgrouse.com>"},
%%                         {vsn,[29884121306770099633619336282407733599]}]}]}}
%% @end
%%-------------------------------------------------------------------------
-spec get_author(list()) -> list() | undefined.

get_author(File) -> Bn = filename:rootname(File, ".beam"),
                    {ok,{_,[{attributes, L}]}} = beam_lib:chunks(Bn, [attributes]),
                    A = case lists:keyfind(author, 1, L) of
                                {author, B} -> B ;
                                _           -> undefined
                        end,
                    A.


