Slack Prolog Client
=================

This library provides a websocket API to slack

https://github.com/swi-to-yap/slack_prolog/

![](t/slack.png)

## Useful to Me?

* A Prolog client for the Slack [Web](https://api.slack.com/web) and [RealTime Messaging](https://api.slack.com/rtm) APIs. 
* Still has a way to go but it gets users started by seeing how easy it was.
* This piece of the puzzle will help you send messages to Slack via the Web API and send and receive messages via the Real Time API.
* If you're trying to respond to slash commands, just write a basic web application and use this library to call the Slack Web API.

## TODOS

* In development add event hooks for users
* Currently Posts over the https webclient (change this to the RTM)

## Installation

Run `?- pack_install(slack_client)`.

## Usage

### Create a New Bot Integration

This is something done in Slack, under [integrations](https://my.slack.com/services). Create a [new bot](https://my.slack.com/services/new/bot), and note its API token.

![](t/register-bot.png)

### Use the API Token

```bash
export SLACK_API_TOKEN=xoxb-01234567890-xxxxxxxxxxxxxxxx
```

### RealTime Client Config

![First Slack Bot](t/first_slack_bot.pl)


### REPL Examples

The Real Time Messaging API is a WebSocket-based API that allows you to receive events from Slack in real time and send messages as user.

```prolog

% navigats the web api and switches to the real time messaging system
?- Client = slack_client.clients.new().

?- $Client.register(hello ,
  debug.print. ["Successfully connected, welcome ", $Client.self.name,
   "to the '", $Client.team.name,"' team at https://", team.domain, ".slack.com."]).


?- $Client.register(message , 
   data.text.contains("bot hi") -> message(channel: data.channel, text: ["Hi <@",data.user,">!"]) ;
   not(data.text.contains('bot')) -> message( channel: data.channel, text: ["Sorry <@",data.user,">, what?"])).

?- $Client.register(close ,  debug.print( "Client is about to disconnect")).

% can register on the last created client (returned by slack_client.clients.new )
?- slack_client.current.register(closed , 
  debug.print. "Client has disconnected successfully!").

?- $Client.start().
```

You can send typing indicators with `typing`.

```prolog
?- $Client.typing(channel: data.channel).
```

You can send a ping with `ping`.

```prolog
?- $Client.ping().
```


You can configure the RealTime client ping rate.
```prolog
?- $Client.set(websocket_ping: 42).
```

### Prolog Objects

By default, the RealTime client exposes and maintains a local store with the properties of [rtm.start](https://api.slack.com/methods/rtm.start) upon a successful connection.

property | description
---------|-------------------------------------------------------------------------------------------------
url      | A WebSocket Message Server URL.
self     | The authenticated bot user.
team     | Details on the authenticated user's team.
users    | A hash of user objects by user ID.
channels | A hash of channel objects, one for every channel visible to the authenticated user.
groups   | A hash of group objects, one for every group the authenticated user is in.
ims      | A hash of IM objects, one for every direct message channel visible to the authenticated user.
bots     | Details of the integrations set up on this team.
text     | textual utils.
debug    | Debugger fidling.
events   | Registered callbacks.
files    | registered storage.
         |

It also tracks changes, such as users being renamed, added or deleted, therefore `?- $Client.users` is always up-to-date.

Tracking with a local store can be disabled with `slack_client.new_client().(store_class: nil)`. Other stores are also available.


#### Chat Messages

Send messages with [chat_PostMessage](https://api.slack.com/methods/chat.postMessage).

```prolog
?- $Client.chat_postMessage(channel: '#general', text: 'Hello World', as_user: true).
```

See a fully working example in ![First Slack Bot](t/first_slack_bot.pl)

![](t/hi_web.gif)

#### List Channels

List channels with [channels_list](https://api.slack.com/methods/channels.list).

```prolog
?- Channels = $Client.channels_list.channels.

?- General_channel = $Channels.detect{ c.name :< 'general' }.
```

#### Upload a File

Upload a file with [files_upload](https://api.slack.com/methods/files.upload).

```prolog
?- $Client.files_upload(
  channels: '#general',
  as_user: true,
  file: slack_client.files.new_upload('/path/to/avatar.jpg', 'image/jpeg'),
  title: 'My Avatar',
  filename: 'avatar.jpg',
  initial_comment: 'Attached a selfie.'
).
```

### Get Channel Info

You can use a channel ID or name (prefixed with `#`) in all functions that take a `:channel` argument. 
Lookup by name is not supported by the Slack API and the `channels_id` method called invokes `channels_list` 
in order to locate the channel ID.

```prolog
?- $Client.channels_info(channel: 'C04KB5X4D'). # calls channels_info

%% teh same command
?- $Client.channels.channels_info(channel: 'C04KB5X4D'). # calls channels_info
```

```prolog
?- $Client.channels_info(channel: '#general'). # calls channels_list followed by channels_info
```

### Get User Info

You can use a user ID or name (prefixed with `@`) in all functions that take a `:user` argument. Lookup by name is not supported by the Slack API and the `users_id` method called invokes `users_list` in order to locate the user ID.

```prolog
?- $Client.users_info(user: 'U092BDCLV'). # calls users_info
```

```prolog
?- $Client.users_info(user: '@logicmoo'). # calls users_list followed by users_info
```

### Search for a User

Constructs an in-memory index of users and searches it. If you want to use this functionality, add the [picky](https://github.com/floere/picky) gem to your project's Gemfile.

```prolog
?- $Client.users_search(user: "logicmoo").
```

#### Parse incomming message

All text in Slack uses the same [system of escaping](https://api.slack.com/docs/formatting): chat messages, direct messages, file comments, etc. 
Use prolog_client.text to unescape/2 incoming messages. 
This comes handy, for example, you want to treat all input to a real time bot as plain text.

```prolog
?- $Client.text.unescape('Hello &amp; &lt;world&gt;',OUT).
  OUT = "Hello & <world>"
?- prolog_client.text.unescape('Hey <@U024BE7LH|bob>, did you see my file?',OUT).
  OUT = "Hey @bob, did you see my file?"
?- $Client.text.unescape('Hey <@U02BEFY4U>',OUT).
  OUT = "Hey @U02BEFY4U"
?- $Client.text.unescape('This message contains a URL <http://foo.com/>',OUT).
  OUT = "This message contains a URL http://foo.com/"
?- $Client.text.unescape('So does this one: <http://www.foo.com|www.foo.com>',OUT).
  OUT = "So does this one: www.foo.com"
?- $Client.text.unescape('<mailto:bob@example.com|Bob>',OUT).
  OUT = "Bob"
?- $Client.text.unescape('Hello <@U123|bob>, say hi to <!everyone> in <#C1234|general>',OUT).
  OUT = "Hello @bob, say hi to @everyone in #general"
?- $Client.text.unescape('Hello <@U123|bob> &gt; file.txt',OUT).
  OUT = "Hello @bob > file.txt"
?- $Client.text.unescape('“hello”',OUT).
  OUT = "\"hello\""
?- $Client.text.unescape('‘hello’',OUT).
  % sends: "'hello'"
```


## Copyright and License

Copyright (c) 2017, [Douglas Miles](https://twitter.com/logicmoo)

This project is licensed under the [MIT License](LICENSE.md).


# Releasing Slack-Prolog

There're no hard rules about when to release slack_prolog. Release bug fixes frequently, features not so frequently and breaking API changes rarely.

### Release

Run tests, check that all tests succeed locally.

```prolog
?- run_tests(slack_client).

```

Increment the version, modify [pack.pl](pack.pl).

*  Increment the third number if the release has bug fixes and/or very minor features, only (eg. change `0.0.1` to `0.0.2`).
*  Increment the second number if the release contains major features or breaking API changes (eg. change `0.0.1` to `0.2.0`).


```
### 0.0.2 (2/10/2017)
```

Remove the line with "Your contribution here.", since there will be no more contributions to this release.

Remove the "Stable Release" section in README that warns users that they are reading the documentation for an unreleased version.

Commit your changes.

```
git add README.md CHANGELOG.md pack.pl
git commit -m "Preparing for release, 0.0.2."
git push origin master
```

Release.

```
$ @TODO
 
Tagged v0.0.2.
Pushed git commits and tags.
Pushed slack_prolog 0.0.2 to swi-prolog.org.
```

### Prepare for the Next Version

Add the next release to [CHANGELOG.md](CHANGELOG.md).

```
Next Release
============

* Your contribution here.
```

Increment the third version number in [pack.pl](pack.pl).

Commit your changes.

```
git add CHANGELOG.md pack.pl
git commit -m "Preparing for next development iteration, 0.0.2."
git push origin master
```
# Contributing to Slack-Prolog

This project is work of [many contributors](https://github.com/swi-to-yap/slack_prolog/graphs/contributors).

You're encouraged to submit [pull requests](https://github.com/swi-to-yap/slack_prolog/pulls), [propose features and discuss issues](https://github.com/swi-to-yap/slack_prolog/issues).

In the examples below, substitute your Github username for `contributor` in URLs.

### Fork the Project

Fork the [project on Github](https://github.com/swi-to-yap/slack_prolog) and check out your copy.

```
git clone https://github.com/contributor/slack_prolog.git
cd slack_prolog
git remote add upstream https://github.com/swi-to-yap/slack_prolog.git
```

### Bundle Install and Test

Ensure that you can build the project and run tests.

Run tests, check that all tests succeed locally.

```
?- run_tests(slack_client).

```

### Run Examples in Development

Sign up for Slack, create a private slack group for yourself.

Create a new Bot Integration under [services/new/bot](http://slack.com/services/new/bot).

![](t/register-bot.png)

On the next screen, note the API token.

To run [the example in t/](t/), open a terminal, cd into _t/_, run `SLACK_API_TOKEN=<your API token>  swipl -l first_slack_bot.pl`.

[First Slack Bot](t/first_slack_bot.pl)

## Contribute Code

### Create a Topic Branch

Make sure your fork is up-to-date and create a topic branch for your feature or bug fix.

```
git checkout master
git pull upstream master
git checkout -b my-feature-branch
```

### Write Tests

Try to write a test that reproduces the problem you're trying to fix or describes a feature that you want to build. Add tests to [spec](spec).

We definitely appreciate pull requests that highlight or reproduce a problem, even without a fix.

### Write Code

Implement your feature or bug fix.

Ruby style is enforced with [Rubocop](https://github.com/bbatsov/rubocop). Run `bundle call rubocop` and fix any style issues highlighted, auto-correct issues when possible with `bundle call rubocop -a`. To silence generally ingored issues, including line lengths or code complexity metrics, run `bundle call rubocop --auto-gen-config`.

Make sure that `bundle call rake` completes without errors.


### Write Documentation

Document any external behavior in the [README](README.md).

### Update Changelog

Add a line to [CHANGELOG](CHANGELOG.md) under *Next Release*. Don't remove *Your contribution here*.

Make it look like every other line, including a link to the issue being fixed, your name and link to your Github account.

### Commit Changes

Make sure git knows your name and email address:

```
git config --global user.name "Your Name"
git config --global user.email "contributor@example.com"
```

Writing good commit logs is important. A commit log should describe what changed and why.

```
git add ...
git commit
```

### Push

```
git push origin my-feature-branch
```

### Make a Pull Request

Go to https://github.com/contributor/slack_prolog and select your feature branch. Click the 'Pull Request' button and fill out the form. Pull requests are usually reviewed within a few days.

### Update CHANGELOG Again

Update the [CHANGELOG](CHANGELOG.md) with the pull request number. A typical entry looks as follows.

```
* [#666](https://github.com/swi-to-yap/slack_prolog/pull/1): Adds MUD connectors - [@contributor](https://github.com/contributor).
```

Amend your previous commit and force push the changes.

```
git commit --amend
git push origin my-feature-branch -f
```

### Rebase

If you've been working on a change for a while, rebase with upstream/master.

```
git fetch upstream
git rebase upstream/master
git push origin my-feature-branch -f
```

### Check on Your Pull Request

Go back to your pull request after a few minutes and see whether it passed muster with Travis-CI. Everything should look green, otherwise fix issues and amend your commit as described above.

### Be Patient

It's likely that your change will not be merged and that the nitpicky maintainers will ask you to Do more, or fix seemingly benign problems. Hang on there!

## Thank You

Please Do know that we really appreciate and value your time and work. We love you, really.

