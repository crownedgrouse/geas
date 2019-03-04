%%%-------------------------------------------------------------------
%%% File:      geas_semver.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2019 crownedgrouse.com
%%% @doc
%%%     semver library for geas
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
%%% Created : 2019-02-10
%%%-------------------------------------------------------------------
-module(geas_semver).
-author("Eric Pailleau <geas@crownedgrouse.com>").

-include_lib("eunit/include/eunit.hrl").

-export([parse/1,simple_range/1, parse_range/1]).

-define(SEMVER, "(\\*|[^0-9]{0,})([0-9\\*]+){0,1}(\.[0-9\\*]+){0,1}(\\.[0-9]+){0,1}(\\-.*){0,}").

parse(V) when is_list(V) ->
      case re:run(V, ?SEMVER, [global, {capture, all_but_first, list}, notempty]) of
         {match, Captured} -> C = case Captured of
                                   [["*",[],[]],[[],[],[]]] -> {"*", "", "", "", ""};
                                   [[Co, Maj]]              -> {Co, Maj, "", "", ""};
                                   [[Co, Maj, Min]]         -> {Co, Maj, supdot(Min), "", ""};
                                   [[Co, Maj, Min, Pat]]    -> {Co, Maj, supdot(Min), supdot(Pat), ""};
                                   [[Co, Maj, Min, Pat, P]] -> {Co, Maj, supdot(Min), supdot(Pat), supdash(P)};
                                   _  -> {error, {parse_error, Captured}}
                                  end,
                              case C of
                                 {error, E} -> {error, E} ;
                                 _          -> {Comp, Major, Minor, Patch, Pre} = C,
                                               {ok, #{comp => Comp, major => Major, minor => Minor, patch => Patch, pre => Pre}}
                              end;
         match             -> {error, match}; % Should never happen !
         nomatch           -> {error, nomatch};
         {error, ErrType}  -> {error, ErrType}
      end.

%%-------------------------------------------------------------------------
%% @doc
%% @since
%% @end
%%-------------------------------------------------------------------------
simple_range(X) ->
   case parse(X) of
      {error, E} -> {error, E};
      {ok, M}    -> simple_range_translate(M)
   end.

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------
simple_range_translate(X) when is_map(X) ->
   Comp = maps:get(comp, X),
   simple_range_translate(X, Comp).

simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, C)
   when (Major == "") ->
   simple_range_translate(#{major => "0", minor => Minor, patch => Patch}, C);
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, C)
   when (Major =/= ""),(Minor == "") ->
   simple_range_translate(#{major => Major, minor => "0", patch => Patch}, C);
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, C)
   when (Major =/= ""),(Minor =/= ""),(Patch == "") ->
   simple_range_translate(#{major => Major, minor => Minor, patch => "0"}, C);
% ~ 	means “reasonably close to”
% ~1.2.3 	is >=1.2.3 <1.3.0
% ~1.2 	   is >=1.2.0 <1.3.0 	(like ~1.2.0)
% ~1 	      same
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "~")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor}, "~")
   when (Major =/= ""),(Minor =/= "") ->
   Min = {">=",Major, Minor, "0"},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#{major := Major}, "~")
   when (Major =/= "") ->
   Min = {"=", Major},
   Max = {"=", Major},
   {ok, Min, Max};
% * 	      any version
simple_range_translate(#{major := _Major, minor := _Minor, patch := _Patch}, "*")
   -> {ok, lowest, highest};

% ^ 	means “compatible with”
% ^1.2.3 	is >=1.2.3 <2.0.0
% ^0.2.3 	is >=0.2.3 <0.3.0 	(0.x.x is special)
% ^0.0.1 	is =0.0.1 	         (0.0.x is special)
% ^1.2 	   is >=1.2.0 <2.0.0 	(like ^1.2.0)
% ^1 	      is >=1.0.0 <2.0.0
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "^")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "^")
   when (Major =/= ""),(Major == "0"),(Minor =/= ""),(Minor =/= "0"),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "^")
   when (Major =/= ""),(Major == "0"),(Minor =/= ""),(Minor == "0"),(Patch =/= "") ->
   Min = {"=", Major, Minor, Patch},
   Max = {"=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "^")
   when (Major =/= ""),(Minor =/= ""),(Patch == "") ->
   Min = {">=", Major, Minor, "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#{major := Major}, "^")
   when (Major =/= "") ->
   Min = {">=", Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
%% = >= > <
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "=")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = {"=", Major, Minor, Patch},
   Max = {"=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, ">=")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = highest,
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, ">")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = {">", Major, Minor, Patch},
   Max = highest,
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "<=")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = lowest,
   Max = {"<=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := Patch}, "<")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = lowest,
   Max = {"<", Major, Minor, Patch},
   {ok, Min, Max};
% 1.x 	   same
% 1.* 	   same
% 1 	      same
% x 	      same
simple_range_translate(#{major := Major, minor := Minor, patch := _Patch}, "")
   when (Major =/= ""),(Minor =/= "") ->
  Min = {"=", Major, Minor},
  Max = {"=", Major, Minor},
  {ok, Min, Max};
simple_range_translate(#{major := Major, minor := Minor, patch := _Patch}, "")
   when (Major =/= ""),(Minor =/= "*") ->
   Min = {"=", Major, Minor},
   Max = {"=", Major, Minor},
   {ok, Min, Max};
simple_range_translate(#{major := Major, minor := _Minor, patch := _Patch}, "")
   when (Major =/= "") ->
   Min = {"=", Major},
   Max = {"=", Major},
   {ok, Min, Max};
simple_range_translate(X, Y)
   -> {error, [X, Y]}.


%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------
% Hyphenated ranges
% 1.2.3 - 2.3.0 	is >=1.2.3 <=2.3.0
% Partial right
% 1.2.3 - 2.3 	is >=1.2.3 <2.4.0
% 1.2.3 - 2 	is >=1.2.3 <3.0.0
% Partial left
% 1.2 - 2.3.0 	is 1.2.0 - 2.3.0
% When the right is partial (eg, 2.3), missing pieces are assumed to be x (eg, 2.3.x).
% When the left is partial (eg, 1.2), missing pieces are assumed to be 0 (eg, 1.2.0).
% Combining ranges
% >=0.14 <16 	      And (space-separated)
% 0.14.x || 15.x.x 	Or (pipe-separated)
parse_range(V) when is_list(V) ->
   case string:tokens(V, " ") of
      [Left, "-", Right]  -> {ok, #{major := Major, minor := Minor, patch := Patch} = X} = parse(Right),
                             case X of
                                 X when (Patch =/= "")
                                    -> parse_range(io_lib:format(">=~ts <=~ts", [Left, Right]));
                                 % Partial right
                                 X when (Minor == ""),(Patch == "")
                                    -> parse_range(io_lib:format(">=~ts <~ts.~ts.~ts", [Left, erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"]));
                                 X when (Patch == "")
                                    -> parse_range(io_lib:format(">=~ts <~ts.~ts.~ts", [Left, Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"]))

                             end ;
      [Left, "||", Right] -> {ok, L, _} = simple_range(Left),
                             {ok, R} = simple_range(Right),
                             {'or', L, R};
      [Left, Right]       -> {ok, L, _} = simple_range(Left),
                             {ok, _, R} = simple_range(Right),
                             {'and', L, R};
      [Single]            -> simple_range(Single)
   end.

%%% TESTS %%%
parse_range_test() ->
    ?assertEqual({'and',{">=","1","2","3"},{"<=","2","3","0"}}, parse_range("1.2.3 - 2.3.0"))
   ,?assertEqual({'and',{">=","1","2","3"},{"<=","2","3","4"}}, parse_range("1.2.3 - 2.3.4"))
   % Partial right
   ,?assertEqual({'and',{">=","1","2","3"},{"<","2","4","0"}}, parse_range("1.2.3 - 2.3"))
   ,?assertEqual({'and',{">=","1","2","3"},{"<","3","0","0"}}, parse_range("1.2.3 - 2"))
   ,ok.

%%% Local functions %%%
supdot(S) -> supchar(S, $.).

supdash(S) -> supchar(S, $-).

supchar(S, C) -> case S of
                     [C | Rest] -> Rest ;
                     _ -> S
                 end.
