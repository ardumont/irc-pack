irc-pack
=========

A pack to 'securely' setup one's irc accounts.

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
machine irc server <irc-server> port <irc-server-port> login <nickname> password <your-password-for-this-account>
```

*Note*

- `password` and `fullname` are optionals

- Keep `machine irc`, this is the static key which is used to find information.

## Example

Connection through irc bouncer znc:

```txt
machine irc server my-znc-server port 5555 login nick password nick/freenode:pass fullname "Your full name"
```

Direct connection to freenode:
```txt
machine irc server irc.freenode.net port 6667 login nick password pass fullname "Your full name"
```
