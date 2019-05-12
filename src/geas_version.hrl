%%-------------------------------------------------------------------------
%% @doc Get {min, recommanded, max} Erlang version from compiler version
%% Look into https://github.com/erlang/otp/blob/maint/lib/compiler/vsn.mk
%% @end
%%-------------------------------------------------------------------------
-spec get_erlang_version(list()) -> {list(), list(), list()} | undefined.

get_erlang_version("7.4")       -> {"22.0","22.0","22.0"};
get_erlang_version("7.3.2")     -> {"21.3","21.3","21.3.6"};
get_erlang_version("7.3.1")     -> {"21.2.3","21.2.7","21.2.7"};
get_erlang_version("7.3")       -> {"21.2","21.2","21.2.2"};
get_erlang_version("7.2.5")     -> {"21.1","21.1","21.1"};
get_erlang_version("7.2.3")     -> {"21.0.5","21.0.8","21.0.8"};
get_erlang_version("7.2.2")     -> {"21.0.2","21.0.4","21.0.4"};
get_erlang_version("7.2.1")     -> {"21.0.1", "21.0.1", "21.0.1"};
get_erlang_version("7.2")       -> {"21.0", "21.0", "21.0"};
get_erlang_version("7.1.5")     -> {"20.3", "20.3", "20.3"};
get_erlang_version("7.1.4")     -> {"20.2", "20.2", "20.2"};
get_erlang_version("7.1.2")     -> {"20.1", "20.1", "20.1"};
get_erlang_version("7.1.1")     -> {"20.0.5", "20.0.5", "20.0.5"};
get_erlang_version("7.1")       -> {"20.0", "20.0", "20.0"};
get_erlang_version("7.0.4")     -> {"19.3", "19.3", "19.3"};
get_erlang_version("7.0.3")     -> {"19.2", "19.2", "19.2"};
get_erlang_version("7.0.2")     -> {"19.1", "19.1", "19.1"};
get_erlang_version("7.0")       -> {"19.0", "19.0", "19.0"};
get_erlang_version("6.0.3")     -> {"18.3", "18.3", "18.3"};
get_erlang_version("6.0.2")     -> {"18.2", "18.2", "18.2"};
get_erlang_version("6.0.1")     -> {"18.1", "18.1", "18.1"};
get_erlang_version("6.0")       -> {"18.0-rc2", "18.0-rc2", "18.0-rc2"};
get_erlang_version("5.0.4")     -> {"17.5", "17.5.3", "17.5.3"};
get_erlang_version("5.0.3")     -> {"17.4", "17.4.1", "18.0-rc1"};
get_erlang_version("5.0.2")     -> {"17.3", "17.3.4", "17.3.4"};
get_erlang_version("5.0.1")     -> {"17.1", "17.2.2", "17.2.2"};
get_erlang_version("5.0")       -> {"17.0", "17.0.2", "17.0.2"};
get_erlang_version("4.9.4")     -> {"R16B03", "R16B03-1", "17.0-rc2"};
get_erlang_version("4.9.3")     -> {"R16B02", "R16B02", "R16B02"};
get_erlang_version("4.9.2")     -> {"R16B01", "R16B01", "R16B01"};
get_erlang_version("4.9.1")     -> {"R16B", "R16B", "R16B"};
get_erlang_version("4.8.2")     -> {"R15B02", "R15B03-1", "R15B03-1"};
get_erlang_version("4.8.1")     -> {"R15B01", "R15B01", "R15B01"};
get_erlang_version("4.8")       -> {"R15B", "R15B", "R15B"};
get_erlang_version("4.7.5.pre") -> {"R15A", "R15A", "R15A"};
get_erlang_version("4.7.5")     -> {"R14B04", "R14B04", "R14B04"};
get_erlang_version("4.7.4")     -> {"R14B03", "R14B03", "R14B03"};
get_erlang_version("4.7.3")     -> {"R14B02", "R14B02", "R14B02"};
get_erlang_version("4.7.2")     -> {"R14B01", "R14B01", "R14B01"};
get_erlang_version("4.7.1")     -> {"R14B", "R14B", "R14B"};
get_erlang_version("4.7")       -> {"R14B", "R14B", "R14B"};
get_erlang_version("4.6.5")     -> {"R13B04", "R13B04", "R13B04"};
get_erlang_version("4.6.4")     -> {"R13B03", "R13B03", "R13B03"};
get_erlang_version(_)           -> undefined.

%%-------------------------------------------------------------------------
%% @doc Get version of app or module
%% @end
%%-------------------------------------------------------------------------
get_version(M) when is_list(M) -> get_version(erlang:list_to_atom(M)) ;
get_version(M) ->
   application:load(M),
   Vapp = case application:get_key(M, vsn) of
               {ok, Y} -> Y;
               _       -> "?"
            end,
   Vmod = case beam_lib:version(code:which(M)) of
            {ok, {M, X}} -> lists:flatten(X) ;
            {error, beam_lib, _} -> "?"
            end,
   case Vapp of
      "?" when (Vmod =/= "?") -> io_lib:format("~20p", [Vmod]);
      "?" -> io_lib:format("~20s", [Vmod]);
      _   -> Vapp
   end.

%%-------------------------------------------------------------------------
%% @doc Get version of current Erlang release
%% @end
%%-------------------------------------------------------------------------
get_current_erlang_version() ->
   F = filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]),
   case file:read_file(F) of
      {ok, B}      -> string:strip(lists:flatten(io_lib:format("~ts", [B])), both, $\n);
      {error, _}   -> 
         application:load(compiler),
         {ok, Cvsn} = application:get_key(compiler, vsn),
         % Bring lowest version from compiler version
         {Current,  _, _} = get_erlang_version(Cvsn),
         Current
   end.

%%-------------------------------------------------------------------------
%% @doc  Give the lowest version from a list of versions
%% @end
%%-------------------------------------------------------------------------
-spec lowest_version(list()) -> list().

lowest_version([]) -> [] ;
lowest_version(L) 
   when is_list(L),
      (length(L) == 1) -> [V] = L, V ;
lowest_version(L) 
   when is_list(L),
      (length(L) > 1)  -> [V | _] = lists:usort(fun(A, B) ->
                                                   X = lowest_version(A, B),
                                                   case (X == A) of
                                                      true -> true ;
                                                      _    -> false
                                                   end
                                                end, L), V.

%%-------------------------------------------------------------------------
%% @doc Give the lowest version from two versions
%% @end
%%-------------------------------------------------------------------------
-spec lowest_version(list(), list()) -> list().

lowest_version([], B) -> B;
lowest_version(A, []) -> A;
lowest_version(A, B) 
   when A =/= B  -> 
      AA = versionize(A),
      BB = versionize(B),
      %[Vmin, _Vmax] = lists:usort([AA,BB]),
      %Vmin
      case lists:usort([AA,BB]) of
         [AA, BB] -> A ;
         [BB, AA] -> B
      end;
lowest_version(A, _B) -> A.

%%-------------------------------------------------------------------------
%% @doc Give the highest version from a list of versions
%% @end
%%-------------------------------------------------------------------------
-spec highest_version(list()) -> list().

highest_version([]) -> [] ;
highest_version(L) 
   when is_list(L),
      (length(L) == 1) -> [V] = L, V ;
highest_version(L) 
   when is_list(L),
      (length(L) > 1) -> 
         Fun = fun(A, B) ->
                  X = highest_version(A, B),
                  case (X == A) of
                     true -> true ;
                     _    -> false
                  end
               end,
         [V | _] = lists:usort(Fun, L), V.

%%-------------------------------------------------------------------------
%% @doc Give the highest version from two versions
%% @end
%%-------------------------------------------------------------------------
-spec highest_version(list(), list()) -> list().

highest_version([], B) -> B;
highest_version(A, []) -> A;
highest_version(A, B) 
   when A =/= B -> 
      AA = versionize(A),
      BB = versionize(B),
      %[_Vmin, Vmax] = lists:usort([AA,BB]),
      %Vmax
      case lists:usort([AA,BB]) of
         [AA, BB] -> B ;
         [BB, AA] -> A
      end;
highest_version(A, _B) -> A.

%%-------------------------------------------------------------------------
%% @doc Translate old release name into new name
%%      Exemple R16B03 into 16.2.3
%% @end
%%-------------------------------------------------------------------------
-spec versionize(list()) -> list().

versionize([]) -> [];
versionize(V)  ->
   [Pref | Tail] = V,
   case Pref of
      $R -> Split = re:split(Tail,"([AB])",[{return,list}]),
            Fun = fun(X) -> 
                     XX = case X of
                        "A" -> "1" ;
                        "B" -> "2" ;
                        X   -> X
                     end,
                     case string:to_integer(XX) of
                        {error, no_integer} -> [] ;
                        {error, not_a_list} -> [] ;
                        {L, []} -> [integer_to_list(L)] ;
                        {L, R} when is_integer(L),
                                    is_list(R) -> [integer_to_list(L) ++ R] ;
                        _                   -> []
                     end
               end,
            New = lists:map(Fun, Split),
               VV = lists:flatten(string:join(New, ".")),
               VV;
      _   -> V
   end.
