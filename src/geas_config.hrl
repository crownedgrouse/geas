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
%% @end
%%-------------------------------------------------------------------------
set_config()     -> set_config([]).

set_config(Conf) ->

   Use_src   = normalize_boolean(proplists:get_value(use_src, Conf, os:getenv("GEAS_USE_SRC"))),
   My_rels   = proplists:get_value(my_rels, Conf, os:getenv("GEAS_MY_RELS")),
   Exc_rels  = normalize_boolean(proplists:get_value(exc_rels, Conf, os:getenv("GEAS_EXC_RELS"))),
   Disc_rels = normalize_boolean(proplists:get_value(disc_rels, Conf, os:getenv("GEAS_DISC_RELS"))),
   Log       = proplists:get_value(log, Conf, os:getenv("GEAS_LOG")),
   Tips      = normalize_boolean(proplists:get_value(tips, Conf, os:getenv("GEAS_TIPS"))),
   Range     = proplists:get_value(range, Conf, os:getenv("GEAS_RANGE")),
   C = #config{ use_src = Use_src
              , my_rels = My_rels
              , exc_rels = Exc_rels
              , disc_rels = Disc_rels
              , log = Log
              , tips = Tips
              , range = Range
              },
    put(geas_conf, C),
    C.