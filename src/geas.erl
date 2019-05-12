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

% API : exported functions for non plugin use
-export([info/1, what/1, offending/1, compat/1, compat/2, guilty/1]).
-export([w2l/1, lowest_version/2, highest_version/2, git_tag/1]).

-export([log/0]).

-export([in_window/3]).

% rebar2 plugin
-export([geas/2]).

% Geas database
-include("geas_db.hrl").

% Discard or discourage usage of some release when some module or functions used
-include("geas_disc.hrl").

% Code
-include("geas_defs.hrl").    % Concerning definitions, records
-include("geas_logs.hrl").    % Concerning logs
-include("geas_path.hrl").    % Concerning paths and directories
-include("geas_beam.hrl").    % Concerning BEAM
-include("geas_app.hrl").     % Concerning Erlang applications
-include("geas_vcs.hrl").     % Concerning VCS
-include("geas_version.hrl"). % Concerning Erlang versions
-include("geas_misc.hrl").    % Concerning misc things
-include("geas_calls.hrl").   % Concerning calls in abstract code
-include("geas_patches.hrl"). % Concerning Erlang releases patches
-include("geas_api.hrl").     % Concerning Geas API
-include("geas_plugins.hrl"). % Concerning Geas plugins





