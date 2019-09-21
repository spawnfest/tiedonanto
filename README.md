# Tiedonanto - SpawnFest 2019

Tiedonanto (who means "communication" in Finnish), is a project to
create a new way to interract with remote end-point in Erlang.

## Goal and purpose

 * Create an application with only application present in default
   Erlang release.
   
 * Create a coherent data-structure to connect to any kind of
   end-point.
   
 * Create tools to alter this data-structure.
 
 * Create a simple view on CLI or/and WEBUI to control the different
   actors.

## Design

```
  ____________     ______     ________      _________
 |            |   |      |   |        |    (         )
 | controller |---| flow |---| client |---( end-point )
 |____________|   |______|   |________|    (_________)

```

## How to compile

```
rebar3 compile

```

## How to test

You can use `eunit` to start unit testing. Simple unit testing are
defined directly in the module to have direcly on one source code,
also some example. Other unit tests have their own file.

```
rebar3 eunit
```

You can also use `common_test` to test the full node.

```
rebar3 ct
```

## How to generate doc

This command will generate all documentation and store in `doc`
directory.

```
rebar3 edoc
```

## How to use

```
rebar3 shell
```

## About the SpawnFest

...
