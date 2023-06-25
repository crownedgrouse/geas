# geas ![Build status](https://github.com/crownedgrouse/geas/actions/workflows/erlang.yml/badge.svg) #

Guess Erlang Application Scattering

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition imposed by an Erlang application or module, which may modify its scattering.

## Overview ##

Geas is a tool detecting the runnable official Erlang release window for your project.

Geas will tell you also :

   - what are the offending functions in the beam/source files that reduce the available window.
   - if some beam files are compiled native.
   - the installed patches and recommend patches that should be installed depending your code.

For example , if a dependency is using ``application:ensure_all_started/1`` your project can only be used starting ``R16B02`` or if ``maps`` is used somewhere, starting ``17.0``. On contrary, ``pg`` module cannot be used after ``17.5`` but is back again in ``23.0``.

For more details, see [Wiki](https://github.com/crownedgrouse/geas/wiki).

Geas is also available on [hex.pm](https://hex.pm/packages/geas).

## Donations ##

Help this project to be maintained ! If you find this project useful, please donate to:

    Bitcoin: 1Ep9VoyNtnNiGoXM52V6NwcMh4SZ9bxkAG
    Ethereum: 0xfac5e46ffe12ec1fc2830d95c22e6583d163d7f9 

## When using it ? ##

- Each time you prepare a project release or update a dependency.
- When you plan to add a dependency to your project
- When writing your README, to inform your project's users on possible release window
- To limit test of CI tools on only possible release window

## Plugins ##

``geas`` is available as a [module](https://github.com/crownedgrouse/geas/wiki/Module-API), but you will probably prefer to use geas plugins with your usual build tool.

Plugin is available for [`erlang.mk`](https://github.com/crownedgrouse/geas/wiki/Erlang.mk-plugin) and [`rebar`](https://github.com/crownedgrouse/geas/wiki/Rebar-plugins) .

## Output example ##

Example on a test project using cowboy :

```
   R15                   26.0       Geas database                      2.7.13
---Min--------Arch-------Max----------------------------------------------------
   R16B                  23.3       cowboy                              1.0.4
                         19.3       cowlib                              1.0.2
                         23.3       ranch                               1.2.1
   R16B03                18.1       test                                0.0.1
--------------------------------------------------------------------------------
   R16B03                18.1       Global project                      0.0.1

C : 26.0
T : >=R16B03 <=18.1

./_build/default/lib/cowboy/ebin/cowboy_clock.beam
R16B      erlang:integer_to_binary/1

./_build/default/lib/cowboy/ebin/cowboy_handler.beam
23.3      erlang:get_stacktrace/0

./_build/default/lib/cowboy/ebin/cowboy_req.beam
R16B      erlang:integer_to_binary/1

./_build/default/lib/cowboy/ebin/cowboy_rest.beam
23.3      erlang:get_stacktrace/0

./_build/default/lib/cowboy/ebin/cowboy_static.beam
R16B      erlang:integer_to_binary/1

./_build/default/lib/cowboy/ebin/cowboy_websocket.beam
R15B02    crypto:hash/2

23.3      erlang:get_stacktrace/0

./_build/default/lib/cowlib/ebin/cow_multipart.beam
19.3      crypto:rand_bytes/1

./_build/default/lib/ranch/ebin/ranch_ssl.beam
23.3      ssl:cipher_suites/0, ssl:ssl_accept/2

./_build/default/lib/test/ebin/test.beam
R16B03    ssh_message:encode_host_key/1

18.1      ssh_message:encode_host_key/1
===> Current Erlang/OTP release is incompatible with project release window

```

Current (C) Erlang release found is `24.2` and no patches (P) was detected.
The global project can run starting R16B03 up to 18.1 in such case, due to use of a function introduced in R16B03 and another one removed in 18.2.
No native compilation was detected on any module.
Recommended patches (R) is optionally proposed when an application used in code is referenced in a not installed patche, even if in this example, current Erlang release cannot run the project.

## Tuning output ##

Output can be tuned depending some [environment variables](https://github.com/crownedgrouse/geas/wiki/Tuning-output).

Note : starting version 2.6, variables can be set in `rebar.config`. 

Environment variables cheatsheet :

**Key**          | **Value type** | **Value**             | **Comment**                           | **Details** | **Since** |
-----------------|----------------|-----------------------|---------------------------------------|-------------|--------|
`GEAS_USE_SRC`   | `boolean`      | [**0** / 1]           | Use source code instead beam files    | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#using-source-code) | |
`GEAS_MY_RELS`   | `string`       | Erlang release list   | List possible releases                | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#listing-possible-releases) | |
`GEAS_EXC_RELS`  | `string`       | Erlang release list   | Exclude some releases                 | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#exclude-some-releases) | |
`GEAS_DISC_RELS` | `boolean`      | [**0** / 1]           | Show discarded buggy Erlang releases  | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#discard-some-releases) | |
`GEAS_LOG`       | `string`       | Log level list        | Log informations                      | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#log-informations) | |
`GEAS_TIPS`      | `boolean`      | [**0** / 1]           | Give tips on patches to apply         | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#tips) | |
`GEAS_RANGE`     | `string`       | SEMVER range          | Set an OTP range for project          | [...](https://github.com/crownedgrouse/geas/wiki/SEMVER-range-and-frame) | |
`GEAS_FRAME`     | `string`       | SEMVER range          | Set an OTP frame for project          | [...](https://github.com/crownedgrouse/geas/wiki/SEMVER-range-and-frame) | 2.6 |
`GEAS_UPDATE`    | `boolean`      | [**0** / 1]           | Force update of OTP version table     |            | 2.6 |
`GEAS_HTTP_OPTS` | `string`       | `httpc` options       | Erlang term string with a final dot   |            | 2.6 |


Under compat table output, some information can be shown depending those environment variables and analyze result.
A single capital letter is used as tag for each information.

   - C = Current : Current Erlang major.minor release used (for example 20.2)
   - P = Patches : List of patches detected to be installed on current release, if any (for example 20.2.2)
   - T = Total   : All releases inside computed release window
      `GEAS_MY_RELS=""`
   - L = Local   : All local releases given by user inside computed window
      `GEAS_MY_RELS="18.1 19.0"`
   - D = Discarded  : Show releases that was automatically discarded
      `GEAS_DISC_RELS=1`
   - E = Excluded  : User excluded release list
      `GEAS_EXC_RELS="R16B03"`
   - R = Recommended : Patches that should be applied, because code is using module impacted by a patch for current version
         `GEAS_TIPS=1`

## Quick Start ##

```
git clone git://github.com/crownedgrouse/geas.git
cd geas
make
make shell
```

## Contributing ##

Contributions are welcome. Please use pull-requests.

