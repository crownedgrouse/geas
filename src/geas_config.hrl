%%-------------------------------------------------------------------------
%% @doc Get cached config, otherwise cache it
%% @since 2.5
%% @end
%%-------------------------------------------------------------------------
get_config() ->
  case get(geas_conf) of
    C when is_record(C, config) 
      -> C ;
    _ -> set_config()
  end.

%%-------------------------------------------------------------------------
%% @doc Set config from rebar3 config and/or environment variables
%% @since 2.5
%% GEAS_USE_SRC    boolean   [0 / 1]               Use source code instead beam files  ...
%% GEAS_MY_RELS    string    Erlang release list   List possible releases  ...
%% GEAS_EXC_RELS   string    Erlang release list   Exclude some releases   ...
%% GEAS_DISC_RELS  boolean   [0 / 1]               Show discarded buggy Erlang releases  ...
%% GEAS_LOG        string    Log level list     Log informations   ...
%% GEAS_TIPS       boolean   [0 / 1]               Give tips on patches to apply
%% GEAS_RANGE      string
%% GEAS_FRAME      string
%% GEAS_UPDATE     boolean   [0 / 1]               Force OTP table to be downloaded
%% GEAS_HTTP_OPTS  string    Http options          Options for inets (a dot required at end)
%% GEAS_SEMVER     boolean   [0 / 1]               Display list of release as semver range
%% @end
%%-------------------------------------------------------------------------
set_config()     -> set_config([]).

set_config(Conf) 
   when is_tuple(Conf)
   -> set_config(record_to_proplist(Conf));

set_config(Conf) 
   when is_list(Conf)
   ->

   Use_src   = normalize_boolean(proplists:get_value(use_src, Conf, os:getenv("GEAS_USE_SRC"))),
   My_rels   = proplists:get_value(my_rels, Conf, os:getenv("GEAS_MY_RELS")),
   Exc_rels  = proplists:get_value(exc_rels, Conf, os:getenv("GEAS_EXC_RELS")),
   Disc_rels = normalize_boolean(proplists:get_value(disc_rels, Conf, os:getenv("GEAS_DISC_RELS"))),
   Log       = proplists:get_value(log, Conf, os:getenv("GEAS_LOG")),
   Tips      = normalize_boolean(proplists:get_value(tips, Conf, os:getenv("GEAS_TIPS"))),
   Range     = proplists:get_value(range, Conf, os:getenv("GEAS_RANGE")),
   Frame     = proplists:get_value(frame, Conf, os:getenv("GEAS_FRAME")),
   Update    = normalize_boolean(proplists:get_value(update, Conf, os:getenv("GEAS_UPDATE"))),
   Http_opts = proplists:get_value(http_opts, Conf, os:getenv("GEAS_HTTP_OPTS")),
   Semver    = normalize_boolean(proplists:get_value(semver, Conf, os:getenv("GEAS_SEMVER"))),

   C = #config{ use_src = Use_src
              , my_rels = My_rels
              , exc_rels = Exc_rels
              , disc_rels = Disc_rels
              , log = Log
              , tips = Tips
              , range = Range
              , frame = Frame
              , update = Update
              , http_opts = Http_opts
              , semver = Semver
              },
    put(geas_conf, C),
    C.


record_to_proplist(#config{} = Rec) ->
  lists:zip(record_info(fields, config), tl(tuple_to_list(Rec))).