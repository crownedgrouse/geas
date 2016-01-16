# geas #

Guess Erlang Application Scattering

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition imposed by an Erlang application or module, which may modify its scattering.

## Overview ##

Geas is a tool that will detect the runnable official Erlang release window for your project.

Geas will tell you what are the offending functions in the beam/source files that reduce the available window.

Geas will tell you if some beam files are compiled native.

## When using it ? ##

- Each time you prepare a project release or update a dependancy.
- When you plan to add a dependancy to your project
- When writing your README, to inform your project's users on possible release window
- To limit test of CI tools on only possible release window

## How it works ? ##

Geas extract all function calls in project’s beam files where abstract code is available and compare to its database. Geas keep only the modules existing in its database, assuming other are modules created by the project. Geas search the starting and optionnaly ending Erlang release exporting thoses modules and functions/arity. Geas gives you then the compatibility with the highest minimal Erlang release and the lowest maximal Erlang release, called the release window. Geas detect native compilation that can reduce your project scattering.

Geas, by default, does not use source code for at least three reasons :

- Source code is not always available
- Source code may compile in different targets depending defines
- Native compilation cannot be detected from source

However, since version 2.0.4, in order to be able to know what Erlang release(s) can compile a project, `geas` can use source files. 

As well, starting this version, `geas` use source file, if available, as fallback when abstract code cannot be extracted from beam file.

Simply set `GEAS_USE_SRC=1` as environment variable. (Unset or set `GEAS_USE_SRC=0` to come back to default).

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

/home/eric/git/test/deps/cowboy/ebin/cowboy_websocket.beam
R15B01    crypto:hash/2
```

The global project can run starting R15B01 up to higher reference in geas database, 18.2 in such case.

### erlang.mk ###

Simply add to your Makefile that includes `erlang.mk` 

```
BUILD_DEPS = geas
DEP_PLUGINS = geas
```

then run 

```
$> make geas
```

See [erlang.mk's geas plugin](https://github.com/crownedgrouse/geas/wiki/Erlang.mk-plugin) documentation for more details. 

### rebar 2.x ###

Simply add those entries in your `rebar.config` .

```
{plugins, [geas]}.

{deps, [{geas, ".*",
        {git, "git://github.com/crownedgrouse/geas.git"}}
]}.
```

then run 

```
$> rebar get-deps
$> rebar compile
```

Geas plugin is then available by typing 

```
$> rebar geas
```

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

### Release >= 2.0.4 ###

Function compat/2 is added. First argument is root directory of a project, second an atom :
- `print` : Same as compat/1. Print global compatibility and dependancies details, like plugins does.
- `global` : Compatibility of code with official releases including dependancies, returned as usual `compat` tuple term ``{MinDbRel, MinRel, MaxRel, MaxDbRel}``.
- `deps` : List of dependancies compatibility tuples.

### Release >= 2.0 ###

A new major feature is added to already existing exported functions: ``compat`` entry, which gives you the official Erlang release window where the beam file(s) can run on. For example : using ``application:ensure_all_started/1`` can only be used starting R16B01 or ``maps`` starting 17.0. As well ``pg`` module cannot be used after 17.5. This can help you to :   
- Write your Travis CI (or equivalent) config file
- Know if beam files can run with an Erlang VM
- Verify that your dependencies can run on same Erlang release window than your project
- Write clear statements in your README project files 
- Modify your code in order to increase runnable release window
- Know if application/module have to be fixed in order to run on the last official release  

A new function is exported : ``geas:offending/1`` which tells you what are the offending functions that reduce the runnable VM versions. 
Works only on a single beam file, i.e argument is the path to this file. Return ``{ok, {MinOffendingList, MaxOffendingList}}`` .

### Release 1.1.0 ###

Another function is added : ``geas:what/1``

Argument is expected to be either a directory containing ``.beam`` files or a path to a single ``.beam`` file.

Output is almost the same than ``geas:info/1`` but several tuple entries are removed instead of setting to ``undefined``.

The purpose of this function is mainly to be used on ``.beam`` files in production environment.

### Release 1.0.0 ###

Only one function is exported : ``geas:info/1`` .

Argument is expected to be a root project directory, and geas will look into ``ebin/`` and use vcs informations if available.
Directories `ebin` and `src` must exist.

``geas:info/1`` won't work on a simple directory containing ``.beam`` files. 
``geas:info/1`` won't work by passing a path to a ``.beam`` file.

Informations may be set to ``undefined`` atom, in some cases, for example if no vcs infos are found.

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

