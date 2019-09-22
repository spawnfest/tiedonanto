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

## About Tiedonanto

A small break during the spawnfest to define the project. One big
issue today is the complexity to communicate to multiple end-point
based on different protocol, for example, if you want to send the same
message on mastodon, twitter, facebook or IRC, each services have
different rules, and method to publish it.

Tiedonanto want to offer a simple abstraction to post and receive
feedback on multiple end-point by create an "essentialist"
interface. Erlang or Elixir are just a langage choice, the main
entry-point should be easily accessible with any kind of API.

### Split the complexity

Creating a common proxy interface to multiple service with not really
the same purpose is hard. So, I think the best way is to split the
complexity on each level.

 * managing connexion pool with coherent data-structure.
 
 * managing controller for each end-point.
 
 * defining dedicated and shared rules for different end-point.
 
 * publish something in a pipeline way.
 
 * doing, by default, the essential.

### Use naming translation

Using a common interface require a naming standard. For exemple, if
you send a tweet on twitter, this message is called "update" in the
official API, the same content on mastodon (a toot) can have another
name, in this case "status". So, a controller must have the capacity
to translate term by using only one for all.

 * idea: twitter(update), mastodon(status), facebook(post)

### Find a solution for common and dedicated rules

So, each end-point uses different name and patterns to post user's
content. Even more, limitation are not the same and each user's
content must be valid on our side. Rules should be applied on
different controller easily, reused anywhere and dynamically created.

### Authentication and sensitive data

Managing multiple account require multiple way to authenticate
users. Each end-point has its own authentication method, even if lot
of them are using oauth1 or oauth2 patterns.

## About the SpawnFest

...
