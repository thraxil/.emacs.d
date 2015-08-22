## My emacs settings

### setup

Requires emacs 24+

    $ cd ~ && git clone git://github.com/thraxil/.emacs.d.git

You'll probably have to `M-x package-install` a few melpa packages
after that (whatever it complains about on initial load.) I haven't
bothered working out how to do that automatically yet.

I'm sorry, I still have `/home/anders` paths hard-coded in a few
places. You will probably find this more useful for stealing bits from
than from using directly (I put it up here so I can easily keep my
emacs config in sync across machines).

### features

* setups for Python, Go, Erlang, Javascript, Django, Markdown, and BDD
feature files
* magit config
* anal-retentive whitespace cleaning functionality
* cursor line is always vertically centered and highlighted
* hippie-expand configured how I like it
