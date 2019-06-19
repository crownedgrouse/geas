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
-export([check/2]).

-define(SEMVER, "(\\*|x|[^0-9]{0,})([0-9\\*x]+){0,1}(\.[0-9\\*x]+){0,1}(\\.[0-9x]+){0,1}(\\-.*){0,}").

-record(version, { comp = "="
                 , major
                 , minor
                 , patch
                 , pre}).

%%-------------------------------------------------------------------------
%% @doc  Check a version against a semver range
%% @since 2.5
%% @end
%%-------------------------------------------------------------------------
%({version,[],"1","4",[],[]},{"=","1","4"})

check(Version, Range)
   when is_list(Version), is_list(Range) ->
   {ok, L} = parse(Version),
   R = parse_range(Range),
   %io:format("~p~n~p~n", [L, R]),
   check(L, R);
check(Version, {'and', L, R})
   when is_record(Version, version) ->
   (check(Version, L) and check(Version, R));
check(Version, {'or', L, R})
   when is_record(Version, version) ->
   (check(Version, L) or check(Version, R));
check(_Version, {_, lowest})  -> true;
check(_Version, {_, highest}) -> true;
check(Version, {Comp}) -> check(Version, Comp, [], [], []);
check(Version, {Comp, Major}) -> check(Version, Comp, Major, [], []);
check(Version, {Comp, Major, Minor}) -> check(Version, Comp, Major, Minor, []);
check(Version, {Comp, Major, Minor, Patche}) -> check(Version, Comp, Major, Minor, Patche).


check(Version, Comp, Major, Minor, Patche)
   when (Comp==[]) -> check(Version, "=", Major, Minor, Patche);
check(Version, Comp, Major, Minor, Patche)
   when (Major==[]) -> check(Version, Comp, "0", Minor, Patche);
check(Version, Comp, Major, Minor, Patche)
   when (Minor==[]) -> check(Version, Comp, Major, "0", Patche);
check(Version, Comp, Major, Minor, Patche) 
   when (Patche==[]) -> check(Version, Comp, Major, Minor, "0");
check(Version, Comp, Major, Minor, Patche)
   when is_record(Version, version), is_list(Comp), is_list(Major), is_list(Minor), is_list(Patche) ->

   io:format("~p~n", [{Version, Comp, Major, Minor, Patche}]),
   C = case (Version#version.major == Major) of
        false -> case (safe_list_to_integer(Version#version.major) > safe_list_to_integer(Major)) of
                     true  -> ">" ;
                     false -> "<"
                 end;
        true   -> case (Version#version.minor == Minor) of
                     false -> case (safe_list_to_integer(Version#version.minor) > safe_list_to_integer(Minor)) of
                                    true  -> ">" ;
                                    false -> "<"
                              end;
                     true  -> case (Version#version.patch == Patche) of
                                 true  -> "=" ;
                                 false -> case (safe_list_to_integer(Version#version.patch) > safe_list_to_integer(Patche)) of
                                                true  -> ">" ;
                                                false -> "<"
                                          end
                              end
                 end
      end,
   case Comp of
        "="  when (C == "=") -> true ;
        ">=" when (C == "=") -> true ;
        "<=" when (C == "=") -> true ;
        ">=" when (C == ">") -> true ;
        ">"  when (C == ">") -> true ;
        "<=" when (C == "<") -> true ;
        "<"  when (C == "<") -> true ;
        _ -> false
   end.

%%-------------------------------------------------------------------------
%% @doc  Parse semver string
%% @since 2.5
%% @end
%%-------------------------------------------------------------------------
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
                                               {ok, #version{comp = Comp, major = Major, minor = Minor, patch = Patch, pre = Pre}}
                              end;
         match             -> {error, match}; % Should never happen !
         nomatch           -> {error, nomatch};
         {error, ErrType}  -> {error, ErrType}
      end.

%%-------------------------------------------------------------------------
%% @doc  Extract simple range
%% @since 2.5
%% @end
%%-------------------------------------------------------------------------
simple_range(X) ->
   case parse(X) of
      {error, E} -> {error, E};
      {ok, M}    -> simple_range_translate(M)
   end.

%%-------------------------------------------------------------------------
%% @doc Translate simple range
%% @end
%%-------------------------------------------------------------------------
simple_range_translate(X) when is_record(X, version) ->
   simple_range_translate(X, X#version.comp).

simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, C)
   when (Major == "") ->
   simple_range_translate(#version{major = "0", minor = Minor, patch = Patch}, C);
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, C)
   when (Major =/= ""),(Minor == "") ->
   simple_range_translate(#version{major = Major, minor = "0", patch = Patch}, C);
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, C)
   when (Major =/= ""),(Minor =/= ""),(Patch == "") ->
   simple_range_translate(#version{major = Major, minor = Minor, patch = "0"}, C);
simple_range_translate(#version{major = Major, minor = _Minor, patch = _Patch}, _)
   when (Major == "x") ->
   simple_range_translate(#version{major = "0", minor = "0", patch = "0"}, ">=");
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, _)
   when (Minor == "x") ->
   parse_range(io_lib:format(">=~ts.0.0 <~ts.0.0", [Major, erlang:integer_to_list(erlang:list_to_integer(Major) + 1)]));
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, _)
   when (Patch == "x") ->
   parse_range(io_lib:format(">=~ts.~ts.0 <~ts.~ts.0", [Major, Minor, Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1)]));
% ~ 	means “reasonably close to”
% ~1.2.3 	is >=1.2.3 <1.3.0
% ~1.2 	   is >=1.2.0 <1.3.0 	(like ~1.2.0)
% ~1 	      same
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "~")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor}, "~")
   when (Major =/= ""),(Minor =/= "") ->
   Min = {">=",Major, Minor, "0"},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major}, "~")
   when (Major =/= "") ->
   Min = {"=", Major},
   Max = {"=", Major},
   {ok, Min, Max};
% * 	      any version
simple_range_translate(#version{major = _Major, minor = _Minor, patch = _Patch}, "*")
   -> {ok, lowest, highest};

% ^ 	means “compatible with”
% ^1.2.3 	is >=1.2.3 <2.0.0
% ^0.2.3 	is >=0.2.3 <0.3.0 	(0.x.x is special)
% ^0.0.1 	is =0.0.1 	         (0.0.x is special)
% ^1.2 	   is >=1.2.0 <2.0.0 	(like ^1.2.0)
% ^1 	      is >=1.0.0 <2.0.0
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Major == "0"),(Minor =/= ""),(Minor =/= "0"),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<", Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Major == "0"),(Minor =/= ""),(Minor == "0"),(Patch =/= "") ->
   Min = {"=", Major, Minor, Patch},
   Max = {"=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "^")
   when (Major =/= ""),(Minor =/= ""),(Patch == "") ->
   Min = {">=", Major, Minor, "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
simple_range_translate(#version{major = Major}, "^")
   when (Major =/= "") ->
   Min = {">=", Major, "0", "0"},
   Max = {"<", erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"},
   {ok, Min, Max};
%% = >= > <
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "=")
   when (Major =/= ""),(Major =/= "0"),(Minor =/= ""),(Patch =/= "") ->
   Min = {"=", Major, Minor, Patch},
   Max = {"=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, ">=")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=", Major, Minor, Patch},
   Max = {"<=", highest},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, ">")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") ->
   Min = {">", Major, Minor, Patch},
   Max = {"<=", highest},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "<=")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=",lowest},
   Max = {"<=", Major, Minor, Patch},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = Patch}, "<")
   when (Major =/= ""),(Minor =/= ""),(Patch =/= "") ->
   Min = {">=",lowest},
   Max = {"<", Major, Minor, Patch},
   {ok, Min, Max};
% 1.x 	   same
% 1.* 	   same
% 1 	      same
% x 	      same
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, "")
   when (Major =/= ""),(Minor =/= "") ->
  Min = {"=", Major, Minor},
  Max = {"=", Major, Minor},
  {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = Minor, patch = _Patch}, "")
   when (Major =/= ""),(Minor =/= "*") ->
   Min = {"=", Major, Minor},
   Max = {"=", Major, Minor},
   {ok, Min, Max};
simple_range_translate(#version{major = Major, minor = _Minor, patch = _Patch}, "")
   when (Major =/= "") ->
   Min = {"=", Major},
   Max = {"=", Major},
   {ok, Min, Max};
simple_range_translate(X, Y)
   -> {error, [X, Y]}.


%%-------------------------------------------------------------------------
%% @doc Parse range into 'and' and 'or' logic
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
      [Left, "-", Right]  -> {ok, #version{major = Major, minor = Minor, patch = Patch} = X} = parse(Right),
      io:format("ici ~p~n",[X]),
                             case X of
                                 X when (Patch =/= "")
                                    -> parse_range(io_lib:format(">=~ts <=~ts", [Left, Right]));
                                 % Partial right
                                 X when (Minor == ""),(Patch == "")
                                    -> parse_range(io_lib:format(">=~ts <~ts.~ts.~ts", [Left, erlang:integer_to_list(erlang:list_to_integer(Major) + 1), "0", "0"]));
                                 X when (Patch == "")
                                    -> parse_range(io_lib:format(">=~ts <~ts.~ts.~ts", [Left, Major, erlang:integer_to_list(erlang:list_to_integer(Minor) + 1), "0"]))

                             end ;
      [Left, "||", Right] -> {ok, L1, R1} = strip_range(simple_range(Left)),
                             {ok, L2, R2} = strip_range(simple_range(Right)),
                             {'or', {'and', L1, R1}, {'and', L2, R2}};
      [Left, "||" | Right] -> {ok, L1, R1} = strip_range(simple_range(Left)),
                              R = parse_range(lists:flatten(join(" ", Right))),
                              {'or', {'and', L1, R1}, R};
      [Left, Right]       -> {ok, L1, R1} = strip_range(simple_range(Left)),
                             {ok, L2, R2} = strip_range(simple_range(Right)),
                             {'and', {'and', L1, R1}, {'and', L2, R2}};
      [Single]            -> {ok, L1, R1} = strip_range(simple_range(Single)),
                             {'and', L1, R1}
   end.


%%% Local functions %%%
supdot(S) -> supchar(S, $.).

supdash(S) -> supchar(S, $-).

supchar(S, C) -> case S of
                     [C | Rest] -> Rest ;
                     _ -> S
                 end.

strip_range({ok, L, R})    -> {ok, L, R};
strip_range({'and', L, R}) -> {ok, L, R}.

%% join is available since 19.0 only
-spec join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

safe_list_to_integer([]) -> safe_list_to_integer("0");
safe_list_to_integer(L) -> erlang:list_to_integer(L).


 %%% TESTS %%%
parse_range_test() ->
    ?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<=","2","3","0"}}},
                 parse_range("1.2.3 - 2.3.0"))
   ,?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<=","2","3","4"}}},
                 parse_range("1.2.3 - 2.3.4"))
   % Partial right
   ,?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<","2","4","0"}}},
                 parse_range("1.2.3 - 2.3"))
   ,?assertEqual({'and',{'and',{">=","1","2","3"},{"<=",highest}},
                        {'and',{">=",lowest},{"<","3","0","0"}}},
                 parse_range("1.2.3 - 2"))
   % Partial left
   ,?assertEqual({'and',{'and',{">=","1","2","0"},{"<=",highest}},
                        {'and',{">=",lowest},{"<=","2","3","0"}}},
                 parse_range("1.2 - 2.3.0"))
   % Combining ranges
   ,?assertEqual({'and',{'and',{">=","0","14","0"},{"<=",highest}},
                        {'and',{">=",lowest},{"<","16","0","0"}}},
                 parse_range(">=0.14 <16"))
   ,?assertEqual({'or', {'and',{'and',{">=","0","14","0"},{"<=",highest}},
                               {'and',{">=",lowest},{"<","0","15","0"}}},
                        {'and',{'and',{">=","15","0","0"},{"<=",highest}},
                               {'and',{">=",lowest},{"<","16","0","0"}}}},
                 parse_range("0.14.x || 15.x.x"))
   ,?assertEqual({'or',{'and',{">=",lowest},{"<","11","0","0"}},
                       {'and',{">","13","0","0"},{"<=",highest}}},
                 parse_range("<11 || >13"))
   ,?assertEqual({'or',{'and',{">=",lowest},{"<","11","0","0"}},
                       {'or',{'and',{">","13","0","0"},{"<=",highest}},
                             {'and',{'and',{">=","12","3","0"},{"<=",highest}},
                                    {'and',{">=",lowest},{"<","12","4","0"}}}}},
                 parse_range("<11 || >13 || 12.3.x"))
   ,ok.

check_test() ->
    ?assertEqual(true, check("1.4","~1.2 || 1.4"))
   ,?assertEqual(false, check("1.4.1","~1.2 || 1.4"))
   ,?assertEqual(true, check("1.2.7","~1.2 || 1.4"))
   ,?assertEqual(true, check("1.4.0","~1.2 || 1.4"))
   ,?assertEqual(false, check("1.4.2","~1.2 || 1.4"))
   ,?assertEqual(true, check("1.4.2","~1.2 || ~1.4"))
   ,ok.