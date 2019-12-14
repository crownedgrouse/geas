%%-------------------------------------------------------------------------
%% @doc Get the VCS of the project
%%
%% Vcs-Arch,               Hum... Who cares ?
%% Vcs-Bzr (Bazaar),       directory : ../trunk !
%% Vcs-Cvs,                directory : CVS
%% Vcs-Darcs,              directory : _darcs
%% Vcs-Git,                directory : .git
%% Vcs-Hg (Mercurial),     directory : .hg
%% Vcs-Mtn (Monotone),     directory : _MTN
%% Vcs-Svn (Subversion)    directory : ../trunk !
%% @end
%%-------------------------------------------------------------------------
-spec get_vcs() -> atom().

get_vcs() -> 
try
      case filelib:is_dir(".git") of
         true  -> throw(git) ;
         false -> false
      end,
      case filelib:is_dir(".hg") of
         true  -> throw(hg) ;
         false -> false
      end,
      case filelib:is_dir("_darcs") of
         true  -> throw(darcs) ;
         false -> false
      end,
      case filelib:is_dir("_MTN") of
         true  -> throw(mtn) ;
         false -> false
      end,
      case filelib:is_dir("CVS") of
         true  -> throw(cvs) ;
         false -> false
      end,
      case filelib:is_dir("../trunk") of
         false -> throw(undefined) ;
         true  -> 
            case os:find_executable("bzr") of
               false -> 
                  case os:find_executable("svn") of
                        false -> throw(undefined) ; % strange...
                        _     -> throw(svn)
                  end;
               _ -> 
                  case os:find_executable("svn") of
                        false -> throw(bzr) ;
                        _     -> throw(undefined) % cannot guess
                  end
            end
      end,
      undefined
   catch
      throw:VCS -> VCS
   end.

%%-------------------------------------------------------------------------
%% @doc Get VCS version / branch  TODO
%% @end
%%-------------------------------------------------------------------------
-spec get_vcs_version(atom()) -> string().

get_vcs_version(git)   -> string:strip(os:cmd("git log -1 --format='%H'"), right, $\n);
get_vcs_version(hg)    -> "TODO";
get_vcs_version(darcs) -> "TODO";
get_vcs_version(mtn)   -> "TODO";
get_vcs_version(cvs)   -> "TODO";
get_vcs_version(bzr)   -> "TODO";
get_vcs_version(svn)   -> "TODO";
get_vcs_version(_)     -> "".

%%-------------------------------------------------------------------------
%% @doc Get maintainer  TODO
%% @end
%%-------------------------------------------------------------------------
-spec get_maintainer(atom()) -> string().

get_maintainer(git)   -> string:strip(os:cmd("git config --get user.name"), right, $\n)++
                        "  <" ++ string:strip(os:cmd("git config --get user.email"), right, $\n)++ ">";
get_maintainer(hg)    -> "TODO";
get_maintainer(darcs) -> "TODO";
get_maintainer(mtn)   -> "TODO";
get_maintainer(cvs)   -> "TODO";
get_maintainer(bzr)   -> "TODO";
get_maintainer(svn)   -> "TODO";
get_maintainer(_)     -> "".

%%-------------------------------------------------------------------------
%% @doc Get VCS URL TODO
%% @end
%%-------------------------------------------------------------------------
-spec get_vcs_url(atom()) -> list().

get_vcs_url(git) -> vcs_git_translate(os:cmd("git config remote.origin.url"));
get_vcs_url(_) -> "".

%%-------------------------------------------------------------------------
%% @doc Translate URL from config to public URL
%% @end
%%-------------------------------------------------------------------------
-spec vcs_git_translate(list()) -> list().

vcs_git_translate("https://" ++ A)   -> string:strip("https://"++A, right, $\n);
vcs_git_translate("git://" ++ A)     -> string:strip("https://"++A, right, $\n);
vcs_git_translate("git@" ++ A)       -> string:strip("https://"++A, right, $\n);
vcs_git_translate("file://" ++ _)    -> local;
vcs_git_translate(_)                 -> undefined.

