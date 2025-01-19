-define(SPLIT_OPTS, case ( geas:lowest_version("17.5", erlang:system_info(otp_release))) of
                         "17.5" -> [trim_all,global];
                         _  -> [global]
                    end).

-define(STORE(X, Y),
        case erlang:get(X) of
			 undefined when is_list(Y)  -> erlang:put(X, Y) ;
			 _         when is_list(Y)  -> erlang:put(X, lists:usort(erlang:get(X) ++ Y));
			 undefined when is_tuple(Y) -> erlang:put(X, [Y]) ;
			 _         when is_tuple(Y) -> erlang:put(X, lists:usort(erlang:get(X) ++ [Y]))
		end
).

% GEAS_USE_SRC 	boolean 	[0 / 1] 	            Use source code instead beam files 	...
% GEAS_MY_RELS 	string 	Erlang release list 	List possible releases 	...
% GEAS_EXC_RELS 	string 	Erlang release list 	Exclude some releases 	...
% GEAS_DISC_RELS 	boolean 	[0 / 1] 	            Show discarded buggy Erlang releases 	...
% GEAS_LOG 	      string 	Log level list 	   Log information 	...
% GEAS_TIPS 	   boolean 	[0 / 1] 	            Give tips on patches to apply

-record(config, {use_src   = false
                ,my_rels   = []
                ,exc_rels  = []
                ,disc_rels = false
                ,log       = []
                ,tips      = false
                ,range     = []
                ,frame     = []
                ,update    = false
                ,http_opts = []
                ,semver    = false
                ,guilty    = false
                }).
