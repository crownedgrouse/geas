# geas #

Guess Erlang Application Scattering

## Overview ##

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition imposed by an Erlang application or module, which may modify its scattering.


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

See [erlang.mk's geas plugin](https://github.com/crownedgrouse/erlang.mk/blob/geas_plugin/doc/src/guide/geas.asciidoc) documentation for more details. 

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
   - compat (version >= 2.0) ``{MinDbRel, MinRel, MaxRel, MaxDbRel}`` Compatibility of code with official releases. First and last value of tuple are the lowest and highest reference of geas database. Second value is the lowest official Erlang release where the beam(s) can run, while third value is the highest release where the beam(s) can run. Note that if first and second values are the same, it may imply that beam(s) file could run, possibly, on older official release, or not. As well if third and fourth value are the same, be sure that your geas database is up to date with the last official release.
   - author (from beam content)
   - vcs information tuple 
   - maintainer (from vcs infos)
   - changelog  Changelog filename if any found
   - releasenotes Releasenotes filename if any found
   - driver  (boolean) Project need to compile C code for a driver or a port   

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

