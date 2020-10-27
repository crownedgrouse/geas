-include("geas_otp_versions.hrl").

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
      AA = geas_semver:versionize(A),
      BB = geas_semver:versionize(B),
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
      AA = geas_semver:versionize(A),
      BB = geas_semver:versionize(B),
      %[_Vmin, Vmax] = lists:usort([AA,BB]),
      %Vmax
      case lists:usort([AA,BB]) of
         [AA, BB] -> B ;
         [BB, AA] -> A
      end;
highest_version(A, _B) -> A.
