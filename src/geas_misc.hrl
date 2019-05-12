%%-------------------------------------------------------------------------
%% @doc Extract releases inside window
%% @end
%%-------------------------------------------------------------------------
in_window(MinGlob, L, MaxGlob) -> 
LMin = lists:filter(fun(X) -> X =:= highest_version(X, MinGlob) end, L),
LMax = lists:filter(fun(X) -> X =:= lowest_version(X, MaxGlob) end, LMin),
LMax.

%%-------------------------------------------------------------------------
%% @doc
%% os:type() -> {Osfamily, Osname}
%% Types:
%%       Osfamily = unix | win32 | ose
%%       Osname = atom()
%%
%% os:version() -> VersionString | {Major, Minor, Release}
%% @end
%%-------------------------------------------------------------------------
-spec get_os() -> tuple().

get_os() -> 
   {A, B} = os:type(),
   V = case os:version() of
            {X, Y, Z} -> integer_to_list(X)++"."++integer_to_list(Y)++
                        "."++integer_to_list(Z) ;
            C         -> C
         end,
   {A, B, V}.
   
%%-------------------------------------------------------------------------
%% @doc
%% erlang:system_info({wordsize, internal}).
%%    Returns the size of Erlang term words in bytes as an integer,
%%    i.e. on a 32-bit architecture 4 is returned,
%%    and on a pure 64-bit architecture 8 is returned.
%%    On a halfword 64-bit emulator, 4 is returned, as the Erlang terms are
%%    stored using a virtual wordsize of half the system's wordsize.
%% {wordsize, external}
%%    Returns the true wordsize of the emulator, i.e. the size of a pointer,
%%    in bytes as an integer.
%%    On a pure 32-bit architecture 4 is returned, on both a halfword and
%%    pure 64-bit architecture, 8 is returned.
%%
%%    internal | external |  architecture
%% ------------+----------+-----------------------------
%%       4     |    4     |  32-bit
%%       8     |    8     |  64-bit
%%       4     |    8     |  halfword 64-bit emulator
%% See also :
%% dpkg-architecture -L
%% @end
%%-------------------------------------------------------------------------
-spec get_word() -> integer().

get_word() -> 
   N = erlang:system_info({wordsize, internal}) +
      erlang:system_info({wordsize, external}),
   case N of
         8 -> 32 ;
      16 -> 64 ;
      12 -> 64
   end.
   %%-------------------------------------------------------------------------
%% @doc Pick right elements existing in left
%% @end
%%-------------------------------------------------------------------------
pickup_rel(Left, Right) -> {S, _} = lists:partition(fun(X) -> lists:member(X, Left) end, Right),
S.

%%-------------------------------------------------------------------------
%% @doc Give distinct release to discard from usual discard tuple
%% Input is a list of { Filename, ListOfDisc }
%% ListOfDisc is a list of { DiscRels, OTP-issue, MFA}
%% @end
%%-------------------------------------------------------------------------
distinct_disc_rels([])   -> [];
distinct_disc_rels(Disc) ->  
ListOfDisc = lists:flatten(lists:flatmap(fun({_, X}) -> [X] end, Disc)),
AllDisc = lists:flatmap(fun({_, {X, _}}) -> sl_flatten(X) end, ListOfDisc),
UAllDisc = lists:usort(AllDisc),
?LOG(geas_logs, {warning, discarded, UAllDisc}),
UAllDisc.

%%-------------------------------------------------------------------------
%% @doc Flatten list of strings without concatenation
%% @end
%%-------------------------------------------------------------------------
sl_flatten(SL) -> 
Tmp = lists:flatten(lists:flatmap(fun(X) -> [list_to_atom(X)] end, SL)),
lists:flatmap(fun(X) -> [atom_to_list(X)] end, Tmp).

