## My emacs settings

### setup

Requires emacs 25+

    $ cd ~ && git clone git://github.com/thraxil/.emacs.d.git

To install emacs 25+ on Ubuntu 14.04, I did the following:

     $ sudo add-apt-repository ppa:ubuntu-elisp/ppa
	 $ sudo apt-get update
	 $ sudo apt-get install emacs-snapshot emacs-snapshot-el

After that, cloning this repo and starting emacs the first time
installs all the necessary packages automatically.

### features

* setups for Python, Go, Erlang, Javascript, Django, Markdown, C++,
Elm, Elixir, YAML, Groovy, and BDD feature files
* magit config
* anal-retentive whitespace cleaning functionality (`C-c n`)
* cursor line is always vertically centered and highlighted
* hippie-expand configured how I like it (on `M-/`)
* mutt specific config
* projectile + helm
* gnus/bbdb setup (getting as close to my mutt config as possible)
* org-mode runs my entire life. I have my own system based on
  GTD/bullet journals with automatic syncing across multiple computers.
