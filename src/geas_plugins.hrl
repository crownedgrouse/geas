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
get_config()     -> get_config([]).

get_config(Conf) ->
   Use_src_   = case os:getenv("GEAS_USE_SRC") of
                        false -> false ;
                        "0"   -> false ;
                        "1"   -> true ;
                        _     -> false
                end,
   My_rels_   = case os:getenv("GEAS_MY_RELS") of
                     false -> [] ;
                     ""    -> [];
                     M     -> M
                end,
   Exc_rels_  = case os:getenv("GEAS_EXC_RELS") of
                     false -> [] ;
                     ""    -> [];
                     E     -> E
                end,
   Disc_rels_ = case os:getenv("GEAS_DISC_RELS") of
                        false -> false ;
                        "0"   -> false ;
                        "1"   -> true ;
                        _     -> false
                end,
   Log_       = case os:getenv("GEAS_LOG") of
                     false -> [] ;
                     ""    -> all;
                     L     -> L
                end,
   Tips_      = case os:getenv("GEAS_TIPS") of
                        false -> false ;
                        "0"   -> false ;
                        "1"   -> true ;
                        _     -> false
                end,
   Range_     = case os:getenv("GEAS_RANGE") of
                     false -> [] ;
                     ""    -> [] ;
                     F     -> F
                end,
   Use_src   = proplists:get_value(use_src, Conf, Use_src_),
   My_rels   = proplists:get_value(my_rels, Conf, My_rels_),
   Exc_rels  = proplists:get_value(exc_rels, Conf, Exc_rels_),
   Disc_rels = proplists:get_value(disc_rels, Conf, Disc_rels_),
   Log       = proplists:get_value(log, Conf, Log_),
   Tips      = proplists:get_value(tips, Conf, Tips_),
   Range     = proplists:get_value(range, Conf, Range_),
   #config{ use_src = Use_src
          , my_rels = My_rels
          , exc_rels = Exc_rels
          , disc_rels = Disc_rels
          , log = Log
          , tips = Tips
          , range = Range}.


