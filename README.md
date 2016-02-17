# geas #

Guess Erlang Application Scattering

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition imposed by an Erlang application or module, which may modify its scattering.

## Overview ##

Geas is a tool that will detect the runnable official Erlang release window for your project.

Geas will tell you what are the offending functions in the beam/source files that reduce the available window.

Geas will tell you if some beam files are compiled native.

For example , if a dependency is using ``application:ensure_all_started/1`` your project can only be used starting ``R16B01`` or if ``maps`` is used somewhere, starting ``17.0``. On contrary, ``pg`` module cannot be used after ``17.5``.

## When using it ? ##

- Each time you prepare a project release or update a dependancy.
- When you plan to add a dependancy to your project
- When writing your README, to inform your project's users on possible release window
- To limit test of CI tools on only possible release window

## Plugins ##

``geas`` is available as a module, but you will probably prefere to use geas plugins with your usual build tool.
A plugin is available for `erlang.mk` and `rebar` .

Example on a test project using cowboy :

```
   R15                   18.2       Geas database
---Min--------Arch-------Max----------------------------------------------------
   R15B01                           cowboy
                                    cowlib
                                    ranch
                                    test
--------------------------------------------------------------------------------
   R15B01                18.2       Global project

Local : R16B03-1 17.3 18.0
Excl. : R16B03

/home/eric/git/test/deps/cowboy/ebin/cowboy_websocket.beam
R15B01    crypto:hash/2
```

The global project can run starting R15B01 up to higher reference in geas database, 18.2 in such case.

Available local releases in the release window is proposed if `GEAS_MY_RELS` is set. User can exclude release(s) with `GEAS_EXC_RELS` environment variable.

See [erlang.mk's geas plugin](https://github.com/crownedgrouse/geas/wiki/Erlang.mk-plugin) or [rebar's geas plugin](https://github.com/crownedgrouse/geas/wiki/Rebar-plugins) documentation for more details. 

## How it works ? ##

Geas extract all function calls in project’s beam files where abstract code is available and compare to its database. Geas keep only the modules existing in its database, assuming other are modules created by the project. Geas search the starting and optionnaly ending Erlang release exporting thoses modules and functions/arity. Geas gives you then the compatibility with the highest minimal Erlang release and the lowest maximal Erlang release, called the release window. Geas detect native compilation that can reduce your project scattering.

Geas, by default, does not use source code for at least three reasons :

- Source code is not always available
- Source code may compile in different targets depending defines
- Native compilation cannot be detected from source

### Using source code ###

Since version 2.0.4, in order to be able to know what Erlang release(s) can compile a project, `geas` can use source files. 

As well, starting this version, `geas` use source file, if available, as fallback when abstract code cannot be extracted from beam file.

Simply set `GEAS_USE_SRC=1` as environment variable. (Unset or set `GEAS_USE_SRC=0` to come back to default).

### Listing possible releases ###

Starting version 2.0.5, environment variable `GEAS_MY_RELS` allow to specify only local releases available. 
It is a blank separated list of official Erlang release names. 

If set, plugin will display the local releases that can compile and run the project.

if set empty, i.e `GEAS_MY_RELS=""`, plugin will display the whole release list included in the computed release window.

Tip : this variable can be automatically set from kerl output :
```
export GEAS_MY_RELS=`kerl list builds | cut -d ',' -f 2 | tr '\n' ' '`
```

### Exclude some releases ###

Alternatively, environment variable `GEAS_EXC_RELS` allow to exclude release(s) from computed release window.
It is a blank separated list of official Erlang release names.

Starting version 2.0.6, Geas will automatically exclude some notoriously buggy Erlang releases if some module/function/arity are concerned. For now only ``R16B03`` with ``syntax_tools``. A big work is to be done to collect Bohrbugs in all Erlang release README (help would be appreciated !).

### Log informations ###

Starting version 2.0.6, environment variable `GEAS_LOG` allow to display analyze logs.
It is a blank separated list of log levels : `debug` `notice` `warning` `error` .

If set, plugin will display logs matching log levels before compat result.

if set empty, i.e `GEAS_LOG=""`, plugin will display all logs.


## Limitations ##

### Abstract code needed ###

Geas ignore beam files where abstract code is not available. 
This may imply to not detect a break in the computed release window. 
However since version 2.0.4, geas extract abstract code from source file in such case, if available, as fallback.

### Geas does not replace tests ###

Geas does not guarantee that your project will run correctly on the whole release window.

Some reasons :
- Bugs may exists in older release, but fixed in newer
- Returned values of some functions may have been changed, reduced or augmented in the release window

It is assumed that project, as well dependencies, are compiling. Parse errors are not raised when using source files.

### Runtime ###

Module compilation and load at project runtime is not covered by `geas` analyze.

### Raw analyze ###

For now, no attempts are done to understand the code. 
So even if a function call is protected by a `catch` to test existence and maybe use an alternative, `geas` won't care.
This may change in futur versions.

## API ##

``geas`` give below informations on an Erlang project repository content or from .beam files :

   - name
   - version
   - description
   - type ``[lib | otp | app | esc]`` for respectively application library, plain OTP application, non OTP application, escript.
   - datetime
   - native ``[true | false]`` true if at least one module compiled native.
   - arch ``[x86 | x86_64 | arm | powerpc | ppc64 | ultrasparc ]`` Architecture of native beam, otherwise local architecture.
   - os  Local OS
   - word ``[32 | 64]`` OS' Word length
   - compile module version string
   - erlang ``{Min, Recommanded, Max}`` Erlang version strings (Recommanded version is the highest non candidate version), guessed from compiler version
   - compat (version >= 2.0) ``{MinDbRel, MinRel, MaxRel, MaxDbRel}`` Compatibility of code with official releases (without dependancies !).    
First and last value of tuple are the lowest and highest reference of geas database.   
Second value is the lowest official Erlang release where the beam(s) can run, while third value is the highest release where the beam(s) can run.   
Note that if first and second values are the same, it may imply that beam(s) file could run, possibly, on older official release, or not. As well if third and fourth value are the same, be sure that your geas database is up to date with the last official release.
   - author (from beam content)
   - vcs information tuple 
   - maintainer (from vcs infos)
   - changelog  Changelog filename if any found
   - releasenotes Releasenotes filename if any found
   - driver  (boolean) Project need to compile C code for a driver or a port   

See [API changelog](https://github.com/crownedgrouse/geas/wiki/API-changelog) for detailed informations on API evolution.

## Example ##
```
1> geas:info(".").
{ok,[{name,geas},
     {version,"2.0.0-alpha"},
     {description,"Guess Erlang Application Scattering"},
     {type,lib},
     {datetime,{2015,5,14,10,56,42}},
     {native,true},
     {arch,x86},
     {os,{unix,linux,"3.13.0"}},
     {word,32},
     {compile,"5.0.3"},
     {erlang,{"17.4","17.4.1","18.0-rc1"}},
     {compat,{"R15","R15","18.1","18.1"}},
     {author,"Eric Pailleau <geas@crownedgrouse.com>"},
     {vcs,{git,"52f7ea4b3c29aa16e337cfd632f786a39d7ecada",
               "https://github.com/crownedgrouse/geas.git"}},
     {maintainer,"crownedgrouse  <eric.pailleau@wanadoo.fr>"},
     {changelog,undefined},
     {releasenotes,undefined},
     {driver,false}]}

2> geas:what("ebin").
{ok,[{name,geas},
     {version,"2.0.0-alpha"},
     {description,"Guess Erlang Application Scattering"},
     {type,lib},
     {datetime,{2015,5,17,20,43,8}},
     {native,true},
     {arch,x86},
     {os,{unix,linux,"3.13.0"}},
     {word,32},
     {compile,"5.0.3"},
     {erlang,{"17.4","17.4.1","18.0-rc1"}},
     {compat,{"R15","R15","18.1","18.1"}},
     {author,"Eric Pailleau <geas@crownedgrouse.com>"}]}

3> geas:what("ebin/geas.beam").
{ok,[{name,geas},
     {version,18399160843177623545190499738944067018},
     {type,lib},
     {datetime,{2015,5,17,20,43,8}},
     {native,true},
     {arch,x86},
     {os,{unix,linux,"3.13.0"}},
     {word,32},
     {compile,"5.0.3"},
     {erlang,{"17.4","17.4.1","18.0-rc1"}},
     {compat,{"R15","R15","18.1","18.1"}},
     {author,"Eric Pailleau <geas@crownedgrouse.com>"}]}

4> geas:offending("test.beam").
{ok,{[{"R16B01",[{application,ensure_all_started,1}]}],[]}}

5> geas:compat(".", global).
{"R15","R15B03-1","18.2","18.2"}

6> geas:w2l(geas:compat(".", global)).
["R15B03-1","R16B","R16B01","R16B02","R16B03-1","17.0",
 "17.1","17.3","17.4","17.5","18.0","18.1","18.2"]

7>geas:highest_version("R15B03","18.2.1").
"18.2.1"

8> geas:lowest_version("R15B03","18.2.1"). 
"R15B03"


```
## Quick Start ##

```
git clone git://github.com/crownedgrouse/geas.git
cd geas
make
erl -pa `pwd`/ebin
```

## Contributing ##

Contributions are welcome. Please use pull-requests.

