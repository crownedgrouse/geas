%%%-------------------------------------------------------------------
%%% File:      geas_tests.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2014 crownedgrouse.com
%%% @doc
%%% Guess Erlang Application Scattering EUnit module
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
%%% Created : 2016-02-14
%%%-------------------------------------------------------------------
-module(geas_tests).
-author("Eric Pailleau <geas@crownedgrouse.com>").

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------------
%% @doc Test get lowest release version from two release versions
%%-------------------------------------------------------------------------
lowest_version_test()  ->
   "R15"      = geas:lowest_version("R15","R16"),
   "R15"      = geas:lowest_version("R15","R16B03"),
   "R15B03"   = geas:lowest_version("R15B03","17.5"),
   "17.5"     = geas:lowest_version("17.5","18.2"),
   "R15"      = geas:lowest_version("R16","R15"),
   "R15"      = geas:lowest_version("R16B03","R15"),
   "R15B03"   = geas:lowest_version("17.5","R15B03"),
   "17.5"     = geas:lowest_version("18.2","17.5"),
   "R16B03"   = geas:lowest_version("R16B03","R16B03-1"),
   "R16B03-1" = geas:lowest_version("17.5","R16B03-1"),
   ok.

%%-------------------------------------------------------------------------
%% @doc Test get highest release version from two release versions
%%-------------------------------------------------------------------------
highest_version_test() ->
   "R16"      = geas:highest_version("R15","R16"),
   "R16B03"   = geas:highest_version("R15","R16B03"),
   "17.5"     = geas:highest_version("R15B03","17.5"),
   "18.2"     = geas:highest_version("17.5","18.2"),
   "R16"      = geas:highest_version("R16","R15"),
   "R16B03"   = geas:highest_version("R16B03","R15"),
   "17.5"     = geas:highest_version("17.5","R15B03"),
   "18.2"     = geas:highest_version("18.2","17.5"),
   "R16B03-1" = geas:highest_version("R16B03","R16B03-1"),
   "17.5"     = geas:highest_version("17.5","R16B03-1"),
   ok.

%%-------------------------------------------------------------------------
%% @doc Test releases included in a releases window
%%-------------------------------------------------------------------------
in_window_test() ->
   ["R15B03","R16B03-1","17.5"] = geas:in_window("R15", ["R15B03","R16B03-1","17.5"], "18.2"),
   ["R16B03-1"] = geas:in_window("R16", ["R15B03","R16B03-1","17.5"], "17.1"),
   ["R16B03-1","17.5"] = geas:in_window("R16", ["R15B03","R16B03-1","17.5"], "17.5"),
   ["R15B03","R16B03-1","17.5"] = geas:in_window("R15B03", ["R15B03","R16B03-1","17.5"], "17.5"),
   ["R15B03","R16B03-1"] = geas:in_window("R15B03", ["R15B03","R16B03-1","17.5"], "17.2"),
   ok.



