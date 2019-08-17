%%-------------------------------------------------------------------------
%% @doc rebar plugin call function
%% @since 2.0.2
%% @end
%%-------------------------------------------------------------------------

geas(_, _ ) -> 
   {ok, Dir} = file:get_cwd(),
   compat(Dir),
   guilty(Dir).

%%-------------------------------------------------------------------------
%% @doc Get config from rebar3 config and/or environment variables
%% @since 2.5
%% @end
%%-------------------------------------------------------------------------
get_config() ->
  case get(geas_conf) of
    C when is_record(C, config) 
      -> C ;
    _ -> set_config()
  end.

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
              , range = Range},
    put(geas_conf, C),
    C.

%%-------------------------------------------------------------------------
%% @doc Facility function for boolean normalization
%% @end
%%-------------------------------------------------------------------------
normalize_boolean(0) -> false ;
normalize_boolean("0") -> false ;
normalize_boolean(false) -> false ;
normalize_boolean("false") -> false ;
normalize_boolean([]) -> false ;
normalize_boolean(1) -> true ;
normalize_boolean("1") -> true ;
normalize_boolean(true) -> true ;
normalize_boolean("true") -> true.






