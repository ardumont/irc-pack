irc-pack
=========

A pack to setup one's irc account through a .authinfo.gpg.

# Install

This is compatible with [emacs-live-packs](https://github.com/ardumont/emacs-live-packs) and [prelude-packs](https://github.com/ardumont/prelude-packs).

## [emacs-live-packs](https://github.com/ardumont/emacs-live-packs)

Add this snippet in your `.emacs-live.el`:
```elisp
(emacs-live-packs/add-live-packs "~/.emacs-live-packs/" '("irc-pack"))
```

## [prelude-packs](https://github.com/ardumont/prelude-packs)

Add this snippet in your `prelude-packs.el`:
```elisp
(prelude-packs/load-packs "~/.prelude-packs/" '("irc-pack"))
```
# Setup

Adding a line to the file `~/.authinfo(.gpg)`:

```txt
machine irc server <irc-server> port <irc-server-port> login <nickname> password <your-password-for-this-account> (connection-type <something>)
```

*Note*

- `password` and `fullname` are optionals

- Keep `machine irc`, this is the static key which is used to find
  information.

- Adding `connection-type` optional key and some random value will
  trigger the use of a tls connection to connect to your irc server

- By default, the connection type, if not provided, will be unsecure

## Example

### Connection through irc bouncer znc

```txt
machine irc server my-znc-server port 5555 login nick password nick/freenode:pass fullname "Your full name"
```

### Secure connection through irc bouncer znc

```txt
machine irc server my-znc-server port 5556 login nick password nick/freenode:pass fullname "Your full name" connection-type tls
```

### Direct connection to freenode
```txt
machine irc server irc.freenode.net port 6667 login nick password pass fullname "Your full name"
```

## Known limitation

Only one connection at a time is supported.

# Use

<kbd>M-x irc-pack-load-pack</kbd>

This will load and prepare the connection through reading the
configuration file's content.

<kbd>M-x irc-pack-erc-start-or-switch</kbd>

This will actually trigger the connection according to the previous step.

<kbd>M-x irc-pack-disconnect</kbd>

To disconnect, either use /quit in irc prompt or the previous command.
