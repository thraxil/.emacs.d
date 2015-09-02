#!/bin/bash
source ~/.keychain/$HOSTNAME-sh
cd ~/org/
~/.emacs.d/bin/git-sync sync
