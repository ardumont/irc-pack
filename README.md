irc-pack
=========

A pack to setup one's irc credentials.

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

Adding a line to the file `~/.netrc`:

```txt
machine irc login nickname password your-password-for-this-account
```

*Note* Do not change `machine irc`, this is static and used by this pack.

Example:
```txt
machine irc login ardumont password your-password-for-this-account
```
