docker run --rm -it -v ~/common-lisp/weblocks:/usr/local/share/common-lisp/source/weblocks daewok/lisp-devel:base sbcl

Hooks
=====

HaCRM uses "hooks" to enable plugins to add a code to be executed
on particular events.

TODO: Add an example how hook should be called and how new handler
should be added to a callback's list.

:feed-item-created
------------------

Called when new feed item was created. This could be a new note, or
an email, or tweet, etc.
