## My emacs settings

### setup

Requires emacs 25+

    $ cd ~ && git clone git://github.com/thraxil/.emacs.d.git

I'm sorry, I still have `/home/anders` paths hard-coded in a few
places. You will probably find this more useful for stealing bits from
than from using directly (I put it up here so I can easily keep my
emacs config in sync across machines. Pull-requests welcome).

To install emacs 25+ on Ubuntu 14.04, I did the following:

     $ sudo add-apt-repository ppa:ubuntu-elisp/ppa
	 $ sudo apt-get update
	 $ sudo apt-get install emacs-snapshot emacs-snapshot-el

After that, cloning this repo and starting emacs the first time
installs all the necessary packages automatically.

### features

* setups for Python, Go, Erlang, Javascript, Django, Markdown, and BDD
feature files
* magit config
* anal-retentive whitespace cleaning functionality (`C-c n`)
* cursor line is always vertically centered and ghlighted
* hippie-expand configured how I like it (on `M-/`)
* mutt specific config
* projectile + helm
