%%-------------------------------------------------------------------------
%% @doc  get type from file
%% @end
%%-------------------------------------------------------------------------
-spec get_app_type_beam(list()) -> atom().

get_app_type_beam(File) ->
   case filename:extension(File) of
      ".erl" -> ?LOG(geas_logs, {notice, {undefined, app_type}, File}),
               undefined ;
      _  ->
         % start/2 and stop/1 ?
         {ok,{_,[{exports, L}]}} = beam_lib:chunks(File, [exports]),
            case (lists:member({start, 2}, L) and
                  lists:member( {stop, 1}, L)) of
                     true  -> otp ;
                     false -> case ((lists:member({start, 0}, L) or lists:member({start, 1}, L)) and
                                    (lists:member( {stop, 0}, L) or lists:member({stop, 1}, L))) of
                                    true  -> app ;
                                    false -> % 'escript' ?
                                             case lists:member({main, 1}, L) of
                                                true  -> esc ;
                                                false -> lib
                                             end
                              end
            end
   end.

%%-------------------------------------------------------------------------
%% @doc Get arch of a native compiled module
%% @end
%%-------------------------------------------------------------------------
-spec get_arch_from_file(list()) -> atom().

get_arch_from_file(File) ->
   case filename:extension(File) of
      ".erl" -> 
         ?LOG(geas_logs, {notice, {undefined, arch}, File}),
         undefined ;
      _ -> 
         Bn = filename:rootname(File, ".beam"),
         [{file,_}, {module,_}, {chunks,C}] = beam_lib:info(Bn),
         Fun = fun({X, _, _}) -> 
                  case X of
                     "Atom" -> true ;
                     "AtU8" -> true ;
                     "Code" -> true ;
                     "StrT" -> true ;
                     "ImpT" -> true ;
                     "ExpT" -> true ;
                     "FunT" -> true ;
                     "LitT" -> true ;
                     "LocT" -> true ;
                     "Attr" -> true ;
                     "CInf" -> true ;
                     "Abst" -> true ;
                     "Dbgi" -> true ;
                     "Line" -> true ;
                     _      -> false
                  end end,
         case lists:dropwhile(Fun, C) of
            [] ->  get_arch(false, File) ;
            [{H, _, _}] -> 
               case H of
                     "HS8P" -> ultrasparc ;
                     "HPPC" -> powerpc ;
                     "HP64" -> ppc64 ;
                     "HARM" -> arm ;
                     "HX86" -> x86 ;
                     "HA64" -> x86_64 ;
                     "HSV9" -> ultrasparc ;
                     "HW32" -> x86
               end
         end
   end.

%%-------------------------------------------------------------------------
%% @doc Check if a beam file is compiled native
%% See also : code:is_module_native(module).
%% @end
%%-------------------------------------------------------------------------
-spec is_native_from_file(list()) -> boolean().

is_native_from_file(File) ->
   case filename:extension(File) of
      ".erl" -> 
         ?LOG(geas_logs, {notice, {undefined, native}, File}),
         undefined ;
      _  -> 
         Bn = filename:rootname(File, ".beam"),
         case filelib:is_regular(File) of
               true -> {ok,{_,[{compile_info, L}]}} = beam_lib:chunks(Bn, [compile_info]),
                     {options, O} = lists:keyfind(options, 1, L),
                     lists:member(native, O);
               false -> ?LOG(geas_logs, {warning, {undefined, native}, File}),
                        undefined
         end
   end.

%%-------------------------------------------------------------------------
%% @doc Get compile module version
%% @end
%%-------------------------------------------------------------------------
-spec get_compile_version(list()) -> list().

get_compile_version(File) ->
   case filename:extension(File) of
      ".erl" -> 
         ?LOG(geas_logs, {notice, {undefined, compile_version}, File}),
         undefined ;
      _  -> 
         Bn = filename:rootname(File, ".beam"),
         case filelib:is_regular(File) of
               true -> {ok,{_,[{compile_info, L}]}} = beam_lib:chunks(Bn, [compile_info]),
                        {version, E} = lists:keyfind(version, 1, L),
                        E;
               false -> ?LOG(geas_logs, {warning, {undefined, compile_version}, File}),
                        undefined
         end
   end.

%%-------------------------------------------------------------------------
%% @doc Get application creation date
%% @end
%%-------------------------------------------------------------------------
-spec get_date(list()) -> list().

get_date(File) ->
   case filename:extension(File) of
      ".erl" -> 
               ?LOG(geas_logs, {warning, {undefined, 'date'}, File}),
               undefined ;
      _  -> Bn = filename:rootname(File, ".beam"),
            case filelib:is_regular(File) of
            true -> 
               {ok,{_,[{compile_info, L}]}} = beam_lib:chunks(Bn, [compile_info]),
               T = case lists:keyfind(time, 1, L) of
                     {time, X} -> X ;
                     _         -> undefined % not in beam starting 19.0
                  end,
               T;
            false -> 
               ?LOG(geas_logs, {warning, {undefined, 'date'}, File}),
               undefined
            end
   end.
%%-------------------------------------------------------------------------
%% @doc Get application author
%% @end
%% beam_lib:chunks("/home/eric/git/swab/ebin/swab", [attributes]).
%% {ok,{swab,[{attributes,[{author,"Eric Pailleau <swab@crownedgrouse.com>"},
%%                         {vsn,[29884121306770099633619336282407733599]}]}]}}
%% @end
%%-------------------------------------------------------------------------
-spec get_author(list()) -> list() | undefined.

get_author(File) ->
   case filename:extension(File) of
      ".erl" -> 
         ?LOG(geas_logs, {warning, {undefined, author}, File}),
         undefined ;
      _  -> 
         Bn = filename:rootname(File, ".beam"),
         case filelib:is_regular(File) of
         true -> {ok,{_,[{attributes, L}]}} = beam_lib:chunks(Bn, [attributes]),
         ?STORE(geas_attributes, L),
                  A = case lists:keyfind(author, 1, L) of
                              {author, B} -> B ;
                              _           -> undefined
                     end,
                  A;
         false -> ?LOG(geas_logs, {warning, {undefined, author}, File}),
            undefined
         end
   end.

%%-------------------------------------------------------------------------
%% @doc Get abstract file either from beam or src file
%% @end
%%-------------------------------------------------------------------------
get_abstract(File) -> %io:format("~p~n",[File]),
   case filename:extension(File) of
      ".beam" -> get_abstract(File, beam) ;
      ".erl"  -> get_abstract(File, src) ;
      _       -> []
   end.

get_abstract(File, beam) -> %io:format("beam ~p~n",[File]),
   case beam_lib:chunks(File,[abstract_code]) of
      {ok,{_,[{abstract_code, {_, Abs}}]}} -> % Extract also exported functions
         {ok, {M,[{exports, Exp}]}} = beam_lib:chunks(File,[exports]),
         lists:foreach(fun({F, A}) -> ?STORE(geas_exports, {M, F, A}) end, Exp),
         Abs;
      {ok,{_,[{abstract_code, no_abstract_code}]}} ->
         ?LOG(geas_logs,{warning, no_abstract_code, File}),
         % Try on source file as fallback
         SrcFile = get_src_from_beam(File),
         case filelib:is_regular(SrcFile) of
               true  -> case filename:extension(SrcFile) of
                        ".dtl" -> get_abstract(SrcFile, dtl) ;
                        _      -> get_abstract(SrcFile, src)
                     end;
               false -> ?LOG(geas_logs, {error, no_source_file, SrcFile}),
                     []
         end
      end;

get_abstract(File, dtl) ->	% Do not treat for the moment
   ?LOG(geas_logs, {notice, template_file, File}),
   [];
get_abstract(File, src) ->	% Add non conventional include dir for sub-directories in src/
   IncDir = get_upper_dir(filename:dirname(File), "include"),
   SrcDir = get_upper_dir(filename:dirname(File), "src"),
   %io:format("inc ~p~nsrc ~p~n",[IncDir, SrcDir]),
   % DO NOT USE : epp:parse_file/2, because starting "R16B03-1"
   % Geas need to work on the maximal release window
   case epp:parse_file(File, [{includes,[filename:dirname(File), IncDir, SrcDir]}], []) of
      {ok , Form} -> 
         case is_valid_code(Form) of
            true  -> % Extract exported functions
                  store_exported(Form),
                  Form;
            false -> ?LOG(geas_logs, {error, parse_error, File}),
                     []
         end;
      Res -> 
         ?LOG(geas_logs, {error, Res, File}),
         []
   end.

%%-------------------------------------------------------------------------
%% @doc Check if source file can be compiled
%% @end
%%-------------------------------------------------------------------------
is_valid_code(Form) ->
   case lists:keyfind(error, 1, Form) of
      false -> true ;
      _     -> false
   end.

%%-------------------------------------------------------------------------
%% @doc Get source (or dtl) file from beam file
%% @end
%%-------------------------------------------------------------------------
get_src_from_beam(File) ->
   UpperDir = filename:dirname(filename:dirname(File)),
	Basename = filename:rootname(filename:basename(File)),
   case ( string:len(Basename) > 3 ) of
      true ->
         case string:substr(Basename, string:len(Basename) -3 ) of
            "_dtl" -> SrcDir = filename:join(UpperDir, "templates"),
                  Dtl = string:substr(Basename, 1, string:len(Basename) -4 ),
                  filename:join([SrcDir, Dtl ++ ".dtl"]);
            _      -> SrcDir = filename:join(UpperDir, "src"),
                  filename:join([SrcDir, Basename ++ ".erl"])
         end;
      false -> 
         SrcDir = filename:join(UpperDir, "src"),
         filename:join([SrcDir, Basename ++ ".erl"])
   end.
%%-------------------------------------------------------------------------
%% @doc Beam opcode compatibity
%% @end
%%-------------------------------------------------------------------------
beam_opcode_compat(Beam) ->
   Cmo = get_cur_max_opcode(),
   Bmo = get_beam_max_opcode(Beam),
   case (Cmo >= Bmo) of
      true when (Bmo == 0) -> undefined;
      true -> true;
      false -> false   
   end.

%%-------------------------------------------------------------------------
%% @doc Get opcode from current version
%% @end
%%-------------------------------------------------------------------------
get_cur_max_opcode() -> get_cur_max_opcode(0, 100).

get_cur_max_opcode(Int, Step) 
   when is_integer(Int),is_integer(Step) -> 
   Next = Int + Step,
   case catch beam_opcodes:opname(Next) of
      {'EXIT',_} when (Step == 1)-> Int;
      {'EXIT',_} -> get_cur_max_opcode(Int, erlang:round(Step / 2));
      _ -> get_cur_max_opcode(Next, Step)
   end.

%%-------------------------------------------------------------------------
%% @doc Get opcode from a Beam file
%% Try to extract with beam_lib, otherwise 0 (TODO use raw method if error).
%% Thanks to Björn Gustavsson (ERL-875)
%% @end
%%-------------------------------------------------------------------------
get_beam_max_opcode(Beam) -> 
   case catch beam_lib:chunks(Beam, ["Code"]) of
      {ok,{_Mod,[{"Code",Code}]}} 
         -> CodeFormatNumber = 0,
            <<_Size:32,CodeFormatNumber:32,Highest:32,_/binary>> = Code,
            Highest;
      _ -> 0 % TODO raw method
   end.
  
%%-------------------------------------------------------------------------
%% @doc Get arch of native compiled modules, or local architecture otherwise
%% @end
%%-------------------------------------------------------------------------
-spec get_arch(boolean(), term()) -> atom().

get_arch(false, _) -> map_arch(erlang:system_info(hipe_architecture));

get_arch(true, EbinDir)  -> 
   Beams = filelib:wildcard("*.beam", EbinDir),
   C = hd(lists:usort(lists:flatmap(fun(F) -> [get_arch_from_file(filename:join(EbinDir, F))] end, Beams))),
   C.

%%-------------------------------------------------------------------------
%% @doc Map internal architecture atom to something else if needed
%% @end
%%-------------------------------------------------------------------------
-spec map_arch(atom()) -> atom().

map_arch(X) -> X.

%%-------------------------------------------------------------------------
%% @doc Check if something is compiled native
%% @end
%%-------------------------------------------------------------------------
-spec is_native(list()) -> boolean().

is_native(EbinDir) ->  
   Beams = filelib:wildcard("*.beam", EbinDir),
   Check = lists:flatmap(fun(F) -> [is_native_from_file(filename:join(EbinDir, F))] end, Beams),
   lists:any(fun(X) -> (X =:= true) end, Check).

%%-------------------------------------------------------------------------
%% @doc Extract exported function from abstract code
%% @end
%%-------------------------------------------------------------------------
store_exported(Form)
	when  is_list(Form),
		   length(Form) > 0 -> 
   % Get module name
   {[{_, _, module, M}], _} = lists:partition(fun(Y) -> lists:keymember(module,3,[Y]) end, Form),
         % Get exported functions
   {E, _} = lists:partition(fun(Y) -> lists:keymember(export,3,[Y]) end, Form),
   lists:flatmap(fun({attribute, _, export, L})
            -> lists:foreach(fun({F, A})
                           -> ?STORE(geas_exports, {M, F, A})
                           end, L), []
               end, E),
   [];

store_exported(_) -> [].