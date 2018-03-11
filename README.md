# geas #

Guess Erlang Application Scattering

``Geas : (In Irish folklore) an obligation or prohibition magically imposed on a person.``

By extension, obligation or prohibition imposed by an Erlang application or module, which may modify its scattering.

## Overview ##

Geas is a tool that will detect the runnable official Erlang release window for your project.

Geas will tell you what are the offending functions in the beam/source files that reduce the available window.

Geas will tell you if some beam files are compiled native.

For example , if a dependency is using ``application:ensure_all_started/1`` your project can only be used starting ``R16B02`` or if ``maps`` is used somewhere, starting ``17.0``. On contrary, ``pg`` module cannot be used after ``17.5``.

## When using it ? ##

- Each time you prepare a project release or update a dependancy.
- When you plan to add a dependancy to your project
- When writing your README, to inform your project's users on possible release window
- To limit test of CI tools on only possible release window

## Plugins ##

``geas`` is available as a [module](https://github.com/crownedgrouse/geas/wiki/Module-API), but you will probably prefere to use geas plugins with your usual build tool.

Plugin is available for [`erlang.mk`](https://github.com/crownedgrouse/geas/wiki/Erlang.mk-plugin) and [`rebar`](https://github.com/crownedgrouse/geas/wiki/Rebar-plugins) .

## Output example ##

Example on a test project using cowboy :

```
   R15                   20.2       Geas database
---Min--------Arch-------Max----------------------------------------------------
   R16B                             cowboy
                         19.3       cowlib
                                    ranch
   R16B       x86        18.1       test
--------------------------------------------------------------------------------
   R16B       x86        18.1       Global project

T : R16B R16B01 R16B02 R16B03 R16B03-1 17.0 17.1 17.3 17.4 17.5 18.0 18.1

/home/eric/git/test/deps/cowboy/ebin/cowboy_clock.beam
R16B      erlang:integer_to_binary/1

/home/eric/git/test/deps/cowboy/ebin/cowboy_req.beam
R16B      erlang:integer_to_binary/1

/home/eric/git/test/deps/cowboy/ebin/cowboy_static.beam
R16B      erlang:integer_to_binary/1

/home/eric/git/test/deps/cowboy/ebin/cowboy_websocket.beam
R15B02    crypto:hash/2

/home/eric/git/test/deps/cowlib/ebin/cow_multipart.beam
19.3      crypto:rand_bytes/1

/home/eric/git/test/ebin/test.beam
R16B      erlang:binary_to_integer/1

18.1      ssh_message:encode_host_key/1
```

The global project can run starting R16B up to 18.1 in such case, due to use of a function introduced in R16B and another one removed in 18.2.
Native x86 compilation was detected on test module.

## Tuning output ##

Output can be tuned depending some [environment variables](https://github.com/crownedgrouse/geas/wiki/Tuning-output).

Environment variables cheatsheet :

**Key**          | **Value type** | **Value**             | **Comment**                           | **Details**
-----------------|----------------|-----------------------|---------------------------------------|-------------
`GEAS_USE_SRC`   | `boolean`      | [**0** / 1]           | Use source code instead beam files    | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#using-source-code)
`GEAS_MY_RELS`   | `string`       | Erlang release list   | List possible releases                | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#listing-possible-releases)
`GEAS_EXC_RELS`  | `string`       | Erlang release list   | Exclude some releases                 | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#exclude-some-releases)
`GEAS_LOG`       | `string`       | Log level list        | Log informations                      | [...](https://github.com/crownedgrouse/geas/wiki/Tuning-output#log-informations)
`GEAS_TIPS`      | `boolean`      | [**0** / 1]           | Give tips on patches to apply         | [WIP]


## Quick Start ##

```
git clone git://github.com/crownedgrouse/geas.git
cd geas
make shell
```

## Contributing ##

Contributions are welcome. Please use pull-requests.

