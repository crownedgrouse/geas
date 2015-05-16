# geas #

Guess Erlang Application Scattering

## Overview ##

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition imposed by an Erlang application or module, which may modify its scattering.

``geas`` give below informations on an Erlang repository content, even without source code :

   - name
   - version
   - description
   - type [lib | otp | app | esc] for respectively application library, plain OTP application, non OTP application, escript.
   - datetime
   - native [true | false] true if at least one module compiled native.
   - arch [x86 | x86_64 | arm | powerpc | ppc64 | ultrasparc ] Architecture of native beam, otherwise local architecture.
   - os  Local OS
   - word [32 | 64] OS' Word length
   - erts version string
   - erlang {Min, Recommanded, Max} Erlang version string (Recommanded version is the highest non candidate version), guessed from erts version
   - author (from beam content)
   - vcs information tuple 
   - maintainer (from vcs infos)
   - changelog  Changelog filename if any found
   - releasenotes Releasenotes filename if any found
   - driver  Project need to compile C code for a driver or a port    

## Example ##
```
1> geas:info(".").
{ok,[{name,geas},
     {version,"1.0.0"},
     {description,"Guess Erlang Application Scattering"},
     {type,lib},
     {datetime,{2015,5,14,10,56,42}},
     {native,true},
     {arch,x86},
     {os,{unix,linux,"3.13.0"}},
     {word,32},
     {erts,"5.0.3"},
     {erlang,{"17.4","17.4.1","18.0-rc1"}},
     {author,"Eric Pailleau <geas@crownedgrouse.com>"},
     {vcs,{git,"52f7ea4b3c29aa16e337cfd632f786a39d7ecada",
               "https://github.com/crownedgrouse/geas.git"}},
     {maintainer,"crownedgrouse  <eric.pailleau@wanadoo.fr>"},
     {changelog,undefined},
     {releasenotes,undefined},
     {driver,false}]}

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

