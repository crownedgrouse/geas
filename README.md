# geas #

Guess Erlang Application Scattering

## Overview ##

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition of an Erlang application or module, which may modify its scattering.

``geas`` is a module giving informations on an Erlang repository content, even without source code.

Name, version, description, date , vcs infos, author, but also what is the repo (application library, OTP application, application, escript),
if it is compiled native or not, if there are drivers, and what is the local OS and architecture.

## Example ##
``
geas:info("."). 
{ok,[{name,geas},
     {version,"1.0.0"},
     {description,"Guess Erlang Application Scattering"},
     {type,lib},
     {datetime,{2015,4,12,16,26,15}},
     {native,false},
     {arch,32},
     {os,{unix,linux,"3.13.0"}},
     {author,"Eric Pailleau <geas@crownedgrouse.com>"},
     {vcs,{git,"abdbfd36e149538fd6d2de7a53656639d622aa56",
               "https://github.com/crownedgrouse/geas.git"}},
     {maintainer,"crownedgrouse  <eric.pailleau@wanadoo.fr>"},
     {changelog,undefined},
     {releasenotes,undefined},
     {driver,false}]}
``


