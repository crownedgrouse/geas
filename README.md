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
   - author (from beam content)
   - vcs  
   - maintainer (from vcs infos)
   - changelog  Changelog filename if any found
   - releasenotes Releasenotes filename if any found
   - driver  Project need to compile C code for a driver or a port    

## Example ##
```
> geas:info("."). 
{ok,[{name,geas},
     {version,"1.0.0"},
     {description,"Guess Erlang Application Scattering"},
     {type,lib},
     {datetime,{2015,4,18,21,39,9}},
     {native,true},
     {arch,x86},
     {os,{unix,linux,"3.13.0"}},
     {word,32},
     {author,"Eric Pailleau <geas@crownedgrouse.com>"},
     {vcs,{git,"e76c03310b5cc9f87aa8126baae41f9c9ff57e06",
               "https://github.com/crownedgrouse/geas.git"}},
     {maintainer,"crownedgrouse  <geas@crownedgrouse.com>"},
     {changelog,undefined},
     {releasenotes,undefined},
     {driver,false}]}
```


