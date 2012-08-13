# Dotfiles

This is my dot-files repository. There are many like it, but this one is mine.

Some of this (particularly the install script and lesskeys file) taken from
Marcel Grünauer's repo (which does not appear to be on GitHub any longer).

Odds are good that this is of very little interest to anyone else.

## Spin-Offs

There are two other repos of mine that are spin-offs of this one. If you're
genuinely interested in this repo, then I suppose you may be interested in
them, as well:

*   [vim-config](https://github.com/rjray/vim-config)

    This is my [Vim](http://www.vim.org) configuration. It isn't that
    exciting right now, because I'm not really a Vim power-user. But I
    do use it in some places where I prefer it over emacs, so I am trying
    to become better with it.

*   [emacs-config](https://github.com/rjray/emacs-config)

    This is my [emacs](https://www.gnu.org/software/emacs/) configuration.
    There is more here than there is in the Vim config, because I'm
    primarily an emacs user (probably about 80% of my text editing). I have
    over 20 years' worth of various tweaks and modifications in my emacs
    set-up, though I have recently done a large-scale overhaul of it and
    pruned out a lot of cruft I no longer used.

Both of these spin-off repos make use of git's
[submodule](http://git-scm.com/book/en/Git-Tools-Submodules) feature. I first
spun off the emacs configuration, largely using Jason Filsinger's blog post
["Emacs, Git, Business & Pleasure"](http://filsinger.me/workflow/emacs-git-business-and-pleasure/)
as a guide. I then followed suit with the Vim stuff.

## The `install` and `expandtags.pl` Files

As mentioned previously, the file `install` owes its roots to Marcel
Grünauer, but I have added two things to it that were not in the original:

*   If there is a file under the `hosts` directory that matches the (short)
    name of this host, I install it in ~ as `.bash-NAME`. There is login in
    `.bashrc` that looks for such a file and sources it if found. This is
    where I keep stuff that should actually be different between hosts.
*   If there is a file called `dotfile_tags.yml` in ~, then it is passed to
    the `expandtags.pl` script. The file is simple YAML, and it just defines
    basic substitutions/expansions for "tags" in files from this repo. You
    see, the first draft of this repo, I left my GitHub auth token in my
    `.gitconfig` file... anyway, I now keep such secrets in non-tracked files
    on each host and let `expandtags.pl` take care of them for me.

## My God, Are You Still Reading This?

Any suggestions or hints are always welcomed. I also only just switched from
<kbd>tcsh</kbd> to <kbd>bash</kbd> in early 2011, so there are probably things
in the various `.bash*` files that could be done better than they currently
are.
