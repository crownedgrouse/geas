%%%-------------------------------------------------------------------
%%% File:      geas_doc.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2015 crownedgrouse.com
%%% @doc  
%%% Geas' Erlang releases inventory generator
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
-module(geas_doc).
-author("Eric Pailleau <geas@crownedgrouse.com>").

-export([release_infos/1, release_infos/2]).
-export([release_diff/2, release_diff_yaml/2, release_diff_files/2, release_diff_yaml_files/2]).

-export([relinfo/1, relinfo/2, reldiff/4, relinfo_term/2]).

%%-------------------------------------------------------------------------
%% @doc Return Erlang Release version infos
%% @end
%%-------------------------------------------------------------------------
-spec release_infos(list()) -> list().

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

%%-------------------------------------------------------------------------
%% @doc Return Erlang Release info
%% @end
%%-------------------------------------------------------------------------
-spec release_infos(list() | atom(), atom() | list()) -> ok | {'error',atom()}.

release_infos(Rel, yaml) -> yamlize(release_infos(Rel));

release_infos(yaml, File) ->  {ok, Io} = file:open(File, [write]),
                              yamlize(release_infos(filename:basename(File)), Io),
                              file:close(Io);

release_infos(term, File) ->  {ok, Io} = file:open(File, [write]),
                              io:format(Io,"~p.~n",[release_infos(filename:basename(File))]),
                              file:close(Io).

%%-------------------------------------------------------------------------
%% @doc List module:function/arity of Erlang Release
%% @end
%%-------------------------------------------------------------------------
-spec release_fa() -> {mfa, list()}.

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
%% @end
%%-------------------------------------------------------------------------
-spec release_diff_files(list(), list()) ->  list().

release_diff_files(F1, F2) ->   {ok, [AT]} = file:consult(F1),
                                {ok, [BT]} = file:consult(F2),
                                release_diff(AT, BT).

%%-------------------------------------------------------------------------
%% @doc Release difference from two files returned as YAML
%% @end
%%-------------------------------------------------------------------------
-spec release_diff_yaml_files(list(), list()) -> ok.

release_diff_yaml_files(F1, F2) ->   {ok, [AT]} = file:consult(F1),
                                     {ok, [BT]} = file:consult(F2),
                                     release_diff_yaml(AT, BT).

%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations
%% @end
%%-------------------------------------------------------------------------
-spec release_diff(list(), list()) -> list().

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
%% @end
%%-------------------------------------------------------------------------
-spec release_diff_yaml(list(), list()) -> ok.

release_diff_yaml(R1, R2) -> yamlize(release_diff(R1, R2)). 

%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations in YAML file
%% @end
%%-------------------------------------------------------------------------
-spec release_diffs(yaml | term, list(), list(), list()) -> ok.

release_diffs(yaml, Rel1, Rel2, File) ->  {ok, Io} = file:open(File, [write]),
                                          yamlize(release_diff(Rel1, Rel2), Io),
                                          file:close(Io), ok;
%%-------------------------------------------------------------------------
%% @doc Release difference from two Release informations in term file
%% @end
%%-------------------------------------------------------------------------
release_diffs(term, Rel1, Rel2, File) ->  {ok, Io} = file:open(File, [write]),
                                          io:format(Io,"~p.~n",[release_diff(Rel1, Rel2)]),
                                          file:close(Io).
%%-------------------------------------------------------------------------
%% @doc List modules difference between two releases infos
%% @end
%%-------------------------------------------------------------------------
-spec diff_modules(list(), list()) -> {list(), list()}.

diff_modules(R1, R2) ->  {mfa, M1} = lists:keyfind(mfa, 1, R1),
                         {mfa, M2} = lists:keyfind(mfa, 1, R2),
                         LM1 = lists:flatmap(fun({X, _}) -> [X] end, M1),
                         LM2 = lists:flatmap(fun({X, _}) -> [X] end, M2),
                         {LM2 -- LM1, LM1 -- LM2}.

%%-------------------------------------------------------------------------
%% @doc List functions difference between two releases infos
%% @end
%%-------------------------------------------------------------------------
-spec diff_funcs(list(), list()) -> {list(), list()}.

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
%% @end
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

yamlize([], _D, _Io) -> ok.

%%-------------------------------------------------------------------------
%% @doc Create relinfo files in doc directory
%% @end
%%-------------------------------------------------------------------------

relinfo([Rel, Dir]) -> relinfo(Rel, Dir).

relinfo(Rel, Dir) -> relinfo_term(Rel, Dir),
                     relinfo_yaml(Rel, Dir).

%%-------------------------------------------------------------------------
%% @doc Create relinfo files in doc directory
%% @end
%%-------------------------------------------------------------------------

relinfo_term(Rel, Dir) -> DirTerm = filename:join(Dir,"term"),
                          filelib:ensure_dir(filename:join(DirTerm,"fakedir")),
                          Target = filename:join(DirTerm, Rel),
                          release_infos(term, Target).

relinfo_yaml(Rel, Dir) -> DirYaml = filename:join(Dir,"yaml"),
                          filelib:ensure_dir(filename:join(DirYaml,"fakedir")),
                          Target = filename:join(DirYaml, Rel),
                          release_infos(yaml, Target).

%%-------------------------------------------------------------------------
%% @doc Create reldiff files in doc directory
%% @end
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
%%-------------------------------------------------------------------------
%% @doc Get content of a file
%% @end
%%-------------------------------------------------------------------------

content(X) ->  {ok, [Res]} = file:consult(X), Res.


