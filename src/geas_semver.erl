%%%-------------------------------------------------------------------
%%% File:      geas_semver.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2019 crownedgrouse.com
%%% @doc
%%%     semver library wrapper for geas
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

-export([check/2, versionize/1, l2s/1]).

%%-------------------------------------------------------------------------
%% @doc Check a version against a range
%% @end
%%-------------------------------------------------------------------------
check(V, R) -> samovar:check(V, R).

%%-------------------------------------------------------------------------
%% @doc Transform old release format into a valid semver version
%% @end
%%-------------------------------------------------------------------------
versionize(V) -> samovar:versionize(V).

%%-------------------------------------------------------------------------
%% @doc Release list to semver range representation
%% @since 2.6.3
%% @end
%%-------------------------------------------------------------------------
-spec l2s(list()) -> {ok, list()} | {error, atom()}.

l2s(L) when is_list(L)
	-> 
	try
		% Get reference release list
		Ref = lists:sort(fun(A, B) ->  case geas:lowest_version(A, B) of A -> true ; B -> false end end, geas_db:get_rel_list()),
		% Check that all given releases are existing in reference
		case lists:all(fun(R) -> lists:member(R, Ref) end, L) of 
			false -> throw(invalid_release);
			true  -> ok
		end,
		% Sort given release list
		SL = lists:sort(fun(A, B) ->  case geas:lowest_version(A, B) of A -> true ; B -> false end end, L),
        % Find all continuous sequence of input against reference
        CS = continuous_seq(SL, Ref),
        Final = seq2range(CS),
		{ok, Final}
	catch
		_:E -> {error, E}
	end.

continuous_seq(A, B) -> continuous_seq(A, B, []).

continuous_seq([], _, Acc) -> [{Acc}] ;

continuous_seq([A | T], [B | Z], Acc) ->
	%erlang:display({A, T, B, Z, Acc}),
	case lists:prefix([A], [B | Z]) of
		true  -> continuous_seq(T, [B | Z] -- [A], Acc ++ [A]) ;
		false -> case Acc of
					[] -> continuous_seq([A | T], Z, Acc);
					_  -> [{Acc}] ++ continuous_seq([A | T], Z, [])
				 end
	end.


seq2range(S) 
	-> lists:flatten(seq2range(S, [])).

seq2range([], Acc)
	-> Acc;

seq2range([H | T], Acc) 
	when is_tuple(H)
	-> 
	   {L} = H,
	   Start = hd(L),
	   End   = hd(lists:reverse(L)),
	   R_ = case length(L) of
	   		1 -> io_lib:format("~ts", [Start]);
	   		_ -> io_lib:format(">=~ts <=~ts", [Start, End])
	   end,
	   R = lists:flatten(R_),
	   case Acc of
	   		[] -> seq2range(T, R);
	   		_  -> seq2range(T, string:join([Acc, R], " || "))
	   end.

%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ù
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

l2s_test()
	-> ?assertEqual({ok,">=18.1 <=18.3 || >=19.1 <=19.2 || 22.0 || 23.0"}, 
		            geas_semver:l2s(["18.1","18.2","18.3","19.1","19.2","22.0","23.0"])),
	   ?assertEqual({error,invalid_release}, 
		            geas_semver:l2s(["failure", "18.1"])),
	ok.
-endif.
