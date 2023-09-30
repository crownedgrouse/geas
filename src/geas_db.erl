%%%-------------------------------------------------------------------
%%% File:      geas_doc.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2023 crownedgrouse.com
%%% @doc
%%% Guess Erlang Application Scattering
%%% Database generation module
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
%%% Created : 2015-11-05
%%%-------------------------------------------------------------------
-module(geas_db).
-author("Eric Pailleau <geas@crownedgrouse.com>").

-export([generate/2, get_rel_list/0, newgenerate/2]).

% List of known releases
-define(REL_LIST, ["R15", "R15B", "R15B01", "R15B02", "R15B03", "R15B03-1",
                   "R16B", "R16B01", "R16B02", "R16B03", "R16B03-1",
                   "17.0", "17.1", "17.3", "17.4", "17.5",
                   "18.0", "18.1", "18.2", "18.3",
		   "19.0", "19.1", "19.2", "19.3",
                   "20.0", "20.1", "20.2", "20.3",
                   "21.0", "21.1", "21.2", "21.3",
                   "22.0", "22.1", "22.2", "22.3",
                   "23.0", "23.1", "23.2", "23.3",
                   "24.0", "24.1", "24.2", "24.3",
		   "25.0", "25.1", "25.2", "25.3",
		   "26.0", "26.1"]).

%% This module generate the geas_db.hrl
%% providing the min and max release of any Erlang/OTP function

newgenerate(DirSource, DirTarget)
  ->
  try 
    true = filelib:is_dir(DirSource),
    true = filelib:is_dir(DirTarget),
    Target = filename:join(DirTarget, "geas_db.dets"),
    % Create Target as a DETS, remove if any
    file:delete(Target),
    {ok, geas_db} = dets:open_file(geas_db, [{file, Target},{ram_file, true}]),
    % List all files under Dir/doc/relinfos/term
    WildcardR = filename:join([DirSource, "doc/relinfos/term/R*"]),
    WildcardN = filename:join([DirSource, "doc/relinfos/term/[0-9]*"]),
    FilesR = lists:sort(filelib:wildcard(WildcardR)),
    FilesN = lists:sort(filelib:wildcard(WildcardN)),
    Files  = FilesR ++ FilesN,
    %io:format("Files: ~p~n", [Files]),
    Fun = fun(F) ->  
        io:format("Treating: ~ts~n", [F]),
        % Load file
        {ok, [Terms]} = file:consult(F),
        %io:format("Terms: ~p~n", [Terms]),
        R = erlang:atom_to_list(proplists:get_value(release, Terms, "")),
        %_O = erlang:atom_to_list(proplists:get_value(version, Terms, "")),
        %_D = erlang:atom_to_list(proplists:get_value(driver_version, Terms, "")),
        %_N = erlang:atom_to_list(proplists:get_value(nif_version, Terms, "")),

        X = proplists:get_value(mfa, Terms),
        Fun2 = fun({M, L}) ->
            Fun3 = fun(FA, Acc) ->
                      FAS = erlang:atom_to_list(FA),
                      [AS | Rest] = lists:reverse(filename:split(FAS)),
                      FS = filename:join(lists:reverse(Rest)),
                      Acc ++ [{M, erlang:list_to_atom(FS), erlang:list_to_integer(AS)}]
                   end,
            Acc = lists:foldl(Fun3, [], L),
            %io:format("Treating: ~p~n", [Acc]),
            % Insert in Dets
            Fun4 = fun(Key) ->
                      case dets:lookup(geas_db, Key) of
                        {error, Reason} -> throw({Key, Reason}) ;
                        []  -> % Insert
                                ok = dets:insert(geas_db, {Key, [R]}) ;
                        [{Key, V}] -> % 
                                ok = dets:insert(geas_db, {Key, V ++ [R]}) 
                      end
                   end,
            ok = lists:foreach(Fun4, Acc)
        end,
        ok = lists:foreach(Fun2, X)        
    end,
    ok = lists:foreach(Fun, Files)
  catch
    %_:Error:Stack ->  io:format("Error: ~p~n~p~n", [Error, Stack])
    _:Error ->  io:format("Error: ~p~n", [Error])
  after
    ok = dets:sync(geas_db),
    io:format("Converting release list to {min, max, semver, cont}~n", []),
    % Traverse for Semver and continuous flag
    FunS = fun({Key, L}) ->
            case is_list(L) of
              true ->
                %io:format("~p~n", [L]),
                Min    = geas:lowest_version(L),
                Max    = geas:highest_version(L),
                {ok, SemVer} = geas_semver:l2s(lists:usort(L)),
                Cont = case string:find(SemVer, "||") of
                    nomatch -> true ;
                    _       -> false
                end,
                % trace some module here
                % {M,_,_} = Key,
                % case M =:= pg of
                %   true -> 
                %       io:format("~p~n", [{Key, {Min, Max, SemVer, Cont}}]);
                %   false ->
                %       ok
                % end,
                V = {Key, {Min, Max, SemVer, Cont}},
                %ok = dets:insert(geas_db, {Key, {Min, Max, SemVer, Cont}}),
                {continue, V};
              false 
                -> continue 
            end
           end,
    Res = lists:sort(dets:traverse(geas_db, FunS)),
    ok = dets:close(geas_db),

    {C, U} = lists:partition(fun({_, {_,_,_,Y}}) -> Y end, Res) ,

    NbO = length(Res),
    io:format("Number of MFAs              : ~p~n", [NbO]),
    % Traverse for functions
    NbC = length(C),
    NbD = length(U),
    io:format("Number of continuous MFAs   : ~p~n", [NbC]),
    io:format("Number of discontinuous MFAs: ~p~n", [NbD]),
    % Writing a Report
    Report = filename:join(DirTarget, "geas_db.txt"),
    file:delete(Report),
    {ok, ReportIo} = file:open(Report, [write]),
    lists:foreach(fun({{M,F,A}, {_,_, SemVer, _}}) -> io:format(ReportIo,"~p:~p/~p : ~p~n", [M,F,A,SemVer]) end, U),
    % Writing geas_db.hrl
    Hrl = filename:join(DirTarget, "geas_db.hrl"),
    file:delete(Hrl),
    {ok, TargetIo} = file:open(Hrl, [write]),
    do_header(TargetIo),
    do_defines(TargetIo),
    % Add former rel_min / rel_max functions
    io:format(TargetIo, "rel_min(X) -> {Min, _, _, _} = rel_info(X), Min.~n", []),
    io:format(TargetIo, "rel_max(X) -> {_, Max, _, _} = rel_info(X), Max.~n~n", []),
    lists:foreach(fun({K, {Min, Max, SemVer, Cont}}) -> 
      io:format(TargetIo, "rel_info(~w) -> {~p, ~p, ~p, ~p};~n", [K, Min, Max, SemVer, Cont]) 
      end, Res),
    io:format(TargetIo, "rel_info({_, _, _}) -> {?GEAS_MIN_REL, ?GEAS_MAX_REL, \">=\" ++ ?GEAS_MIN_REL ++ \"<=\" ++ ?GEAS_MAX_REL, true}.~n", []),
    file:close(TargetIo)
  end.

%%-------------------------------------------------------------------------
%% @doc Generate target geas database from inventory root path
%% @end
%%-------------------------------------------------------------------------
-spec generate(list(), list()) -> ok.

generate(Dir, Target) -> generate(filename:join([Dir, "relinfos", "term"]),
                                  filename:join([Dir, "reldiffs", "term"]),
                                  Target).

%%-------------------------------------------------------------------------
%% @doc Generate target geas database from info and diff directories
%% @end
%%-------------------------------------------------------------------------
-spec generate(list(), list(), list()) -> ok | {'error',atom()}.

generate(IDir, DDir, Target) 
  -> 
    put(trace, pg),
    {ok, TargetIo} = file:open(Target, [write]),
    do_header(TargetIo),
    do_defines(TargetIo),
    % List all functions removed in what release [{module, release}, ...]
    Data11 = lists:flatten(get_removed_functions(DDir)),
    do_max_functions(TargetIo, Data11),
    % list of modules removed in what release [{module, release}, ...]
    Data12 = lists:flatten(get_removed_modules(DDir)),
    do_max_modules(TargetIo, Data12),
    io:nl(TargetIo),
    % List all functions added in what release [{module, release}, ...]
    Data21 = lists:flatten(get_added_functions(DDir)),
    do_min_functions(TargetIo, Data21),
    % List the whole modules of oldest known release
    {ok, [R1]} = file:consult(filename:join(IDir, hd(?REL_LIST))),
    {mfa, Data22} = lists:keyfind(mfa, 1, R1),
    do_min_modules(TargetIo, Data22),
    % Close file
    file:close(TargetIo).

%%-------------------------------------------------------------------------
%% @doc Get all removed functions from diff directory
%% @end
%%-------------------------------------------------------------------------
-spec get_removed_functions(list()) -> list().

get_removed_functions(DDir) ->
 % List all reldiffs files
 {ok, Reldiffs} = file:list_dir(DDir),
 % Loop over list and pick up all removed functions, linked to 'From'
 _RMF = lists:map(fun(F) -> % Load file
                        {ok, [R]} = file:consult(filename:join(DDir, F)),
                        % Pick 'From' where last presence of function is
                        {from, From} = lists:keyfind(from, 1, R),
                        From_s =  atom_to_list(From),
                        % Pick 'functions' entry
                        {functions, Fs} = lists:keyfind(functions, 1, R),
                        % Pick 'removed' entry
                        {removed, RF} = lists:keyfind(removed, 1, Fs),

                        % Compose [{module, function, arity, release}, ...]
                        lists:map(fun({Mm, FL}) ->
                                      lists:map(fun(Fa) ->
                                                    Fa_s = atom_to_list(Fa),
                                                    % Extract function and arity
                                                    [Ff, Aa] = string:tokens(Fa_s, "/"),
                                                    {Int, _} = string:to_integer(Aa),
                                                    [{Mm, list_to_atom(Ff), Int, From_s}]
                                                 end, FL)
                                  end, RF)
              end, Reldiffs).

%%-------------------------------------------------------------------------
%% @doc Get all added functions from diff directory
%% @end
%%-------------------------------------------------------------------------
-spec get_added_functions(list()) -> list().

get_added_functions(DDir) ->
 % List all reldiffs files
 {ok, Reldiffs} = file:list_dir(DDir),
 % Loop over list and pick up all removed functions, linked to 'From'
 _AMF = lists:map(fun(F) -> % Load file
                        {ok, [R]} = file:consult(filename:join(DDir, F)),
                        % Pick 'To' where new presence of function is
                        {to, To} = lists:keyfind(to, 1, R),
                        To_s =  atom_to_list(To),
                        % Pick 'functions' entry
                        {functions, Fs} = lists:keyfind(functions, 1, R),
                        % Pick 'added' entry
                        {new, NF} = lists:keyfind(new, 1, Fs),

                        % Compose [{module, function, arity, release}, ...]
                        lists:map(fun({Mm, FL}) ->
                                      lists:map(fun(Fa) ->
                                                    Fa_s = atom_to_list(Fa),
                                                    % Extract function and arity
                                                    [Ff, Aa] = string:tokens(Fa_s, "/"),
                                                    {Int, _} = string:to_integer(Aa),
                                                    [{Mm, list_to_atom(Ff), Int, To_s}]
                                                 end, FL)
                                  end, NF)
              end, Reldiffs).

%%-------------------------------------------------------------------------
%% @doc Get all removed modules from diff directory
%% @end
%%-------------------------------------------------------------------------
-spec get_removed_modules(list()) -> list().

get_removed_modules(DDir) ->
 % List all reldiffs files
 {ok, Reldiffs} = file:list_dir(DDir),
 % Loop over list and pick up all removed modules, linked to 'To'
 _RMS = lists:map(fun(F) -> % Load file
                          {ok, [R]} = file:consult(filename:join(DDir, F)),
                          % Pick 'From'
                      	  {from, From} = lists:keyfind(from, 1, R),
                          % Pick 'modules' entry
                          {modules, M} = lists:keyfind(modules, 1, R),
                          % Pick 'removed' entry
                          {removed, RM} = lists:keyfind(removed, 1, M),
                          % Compose [{module, release}, ...]
                          lists:map(fun(X) -> [{X, atom_to_list(From)}] end, RM)
                end, Reldiffs) .

%%-------------------------------------------------------------------------
%% @doc Geas database header
%% @end
%%-------------------------------------------------------------------------
do_header(Io) ->  
  Header = ["%% File: geas_db.hrl"
           ,"%% @author    Generated by geas_db module "
           ,"%% @warning   DO NOT EDIT BY HAND OR YOUR CHANGE WILL BE LOST"
           ,"%% @copyright Eric Pailleau <geas@crownedgrouse.com>"
           ,"%% @licence   https://github.com/crownedgrouse/geas/blob/master/LICENCE"
           ,"%% @doc "
           ,"%% Geas database "
           ,"%% @end "
           ],
  io:put_chars(Io, erl_prettypr:format(erl_syntax:comment(Header))).
%%-------------------------------------------------------------------------
%% @doc defines in top of Geas database
%% @end
%%-------------------------------------------------------------------------
do_defines(Io) ->  
  DefMin = erl_syntax:attribute(
                    erl_syntax:atom('define'),
                    [erl_syntax:atom('GEAS_MIN_REL'),
                     erl_syntax:string(hd(?REL_LIST))]),
  DefMax = erl_syntax:attribute(
                    erl_syntax:atom('define'),
                    [erl_syntax:atom('GEAS_MAX_REL'),
                     erl_syntax:string(hd(lists:reverse(?REL_LIST)))]),
   io:put_chars(Io, erl_prettypr:format(DefMin)),
   io:nl(Io),
   io:put_chars(Io, erl_prettypr:format(DefMax)),
   io:nl(Io),
   io:nl(Io).

%%-------------------------------------------------------------------------
%% @doc Oldest modules in min release
%% @end
%%-------------------------------------------------------------------------
do_min_modules(Io, Data) -> 
  lists:foreach(fun({M, _}) -> 
    case ( get(trace) =:= M ) of
      true -> erlang:display({?FUNCTION_NAME, M}) ;
      _    -> ok
    end,
    io:format(Io,"rel_min({~p, _, _}) -> ?GEAS_MIN_REL ;~n", [M]) end, 
  Data),
  % If no match, MFA is probably from a non core Erlang (i.e. private) module
  io:format(Io,"rel_min({_, _, _}) -> ~p .~n", [undefined]) .

%%-------------------------------------------------------------------------
%% @doc Add all removed modules in reldiffs
%% @end
%%-------------------------------------------------------------------------
do_max_modules(Io, Data) -> 
  lists:foreach(fun({M, R}) -> 
    case ( get(trace) =:= M ) of
      true -> erlang:display({?FUNCTION_NAME, M, R}) ;
      _    -> ok
    end,
    io:format(Io,"rel_max({~p, _, _}) -> ~p ;~n", [M, R]) end, 
  Data),
  % If no match, MFA is still available in max release
  io:format(Io,"rel_max({_, _, _}) -> ?GEAS_MAX_REL.~n", []) .

%%-------------------------------------------------------------------------
%% @doc Oldest modules in min release
%% @end
%%-------------------------------------------------------------------------
do_min_functions(Io, Data) ->
   lists:foreach(fun({M, F, A, R}) ->
      case ( get(trace) =:= M ) of
        true -> erlang:display({?FUNCTION_NAME, M, F, A, R}) ;
        _    -> ok
      end,
      case dooblon(min, {M, F, A}) of
         true  -> exception(min, {M, F, A, R}),
                  io:format(Io,"%rel_min({~p, ~p, ~p}) -> ~p ;~n", [M, F, A, R]);
         false -> minlist({M, F, A}),
                  io:format(Io,"rel_min({~p, ~p, ~p}) -> ~p ;~n", [M, F, A, R])
      end end, Data).

%%-------------------------------------------------------------------------
%% @doc Add all removed modules in reldiffs
%% @end
%%-------------------------------------------------------------------------
do_max_functions(Io, Data) ->
   lists:foreach(fun({M, F, A, R}) ->
      case ( get(trace) =:= M ) of
        true -> erlang:display({?FUNCTION_NAME, M, F, A, R}) ;
        _    -> ok
      end,
      case dooblon(max, {M, F, A}) of
         true  -> exception(max, {M, F, A, R}),
                  io:format(Io,"%rel_max({~p, ~p, ~p}) -> ~p ;~n", [M, F, A, R]);
         false -> maxlist({M, F, A}),
                  io:format(Io,"rel_max({~p, ~p, ~p}) -> ~p ;~n", [M, F, A, R])
      end end, Data).

%%-------------------------------------------------------------------------
%% @doc Give release list
%% @end
%%-------------------------------------------------------------------------
get_rel_list() -> ?REL_LIST.

%%-------------------------------------------------------------------------
%% @doc Give release list
%% @end
%%-------------------------------------------------------------------------
exception(min, X) ->
   case get(exception_min) of
      undefined ->
         put(exception_min, [X]);
      L when is_list(L) ->
         put(exception_min, L ++ [X] ) ;
      _ ->
         throw(exception_min_error)
   end;

exception(max, X) ->
   case get(exception_max) of
      undefined ->
         put(exception_max, [X]);
      L when is_list(L) ->
         put(exception_max, L ++ [X] ) ;
      _ -> throw(exception_max_error)
   end.

dooblon(min, X) ->
   case get(minlist) of
      undefined ->
         put(minlist, [X]),
         false;
      L when is_list(L) ->
         lists:member(X, L)
   end;

dooblon(max, X) ->
   case get(maxlist) of
      undefined ->
         put(maxlist, [X]),
         false;
      L when is_list(L) ->
         lists:member(X, L)
   end.

minlist(X) ->
   case get(minlist) of
      undefined ->
         put(minlist, [X]);
      L when is_list(L) ->
         put(minlist, L ++ [X] ) ;
      _ -> throw(exception_minlist_error)
   end.

maxlist(X) ->
   case get(maxlist) of
      undefined ->
         put(maxlist, [X]);
      L when is_list(L) ->
         put(maxlist, L ++ [X] ) ;
      _ -> throw(exception_maxlist_error)
   end.
