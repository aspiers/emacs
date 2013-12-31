Adam's emacs config
===================

This is my collection of configuration files and utilities for
[emacs](http://emacs.org/).  I started tracking this in a CVS
repository in 1999, and in 2011 finally migrated it to git.  Due to
its age, there is unsurprisingly a fair bit of cruft, although I'm
gradually replacing the outdated hacks with the help of modern emacs
packages.

OVERVIEW
--------

I use [`el-get`](http://www.emacswiki.org/emacs/el-get) for installing
and compiling packages (even the ones from ELPA repositories such as
[MELPA](http://melpa.milkbox.net/) and
[Marmalade](https://github.com/jwiegley/use-package)), John Wiegley's
nifty [`use-package`](https://github.com/jwiegley/use-package) and
[`bind-key`](https://github.com/jwiegley/use-package/blob/master/bind-key.el)
utilities for setting up autoloads, key bindings, and other
per-package configuration.

I generally adhere to the convention of adding an `as-` prefix to any
files, functions, and variables which are specific to my needs.

*   `.emacs` -- effectively loads all `*.el` files in `.emacs.d/init.d/`.
    These files are discovered via a custom external mechanism
    which I use for several sets of configuration files unrelated to emacs,
    including my [shell](https://github.com/aspiers/shell-env/)
    [mutt](https://github.com/aspiers/mutt/), and
    [ssh](https://github.com/aspiers/ssh-config) configuration files.

    The mechanism is plugin-oriented, and essentially an enhanced
    implementation of the standard UNIX approach of creating a
    directory such as `/etc/profile.d` and then ensuring that any
    files dropped into that directory (which I chose a long time ago
    to call "hooks" - admittedly a bad choice of word considering that
    emacs already uses it for other purposes) automatically get
    loaded.  The mechanism is implemented by shell functions defined
    in the
    [run_hooks](https://github.com/aspiers/shell-env/blob/master/.zsh/functions/run_hooks)
    and
    [find_hooks](https://github.com/aspiers/shell-env/blob/master/.zsh/functions/find_hooks)
    files within my
    [shell-env repository](https://github.com/aspiers/shell-env/).
    You can also see my [very old notes on the design of this "hook" system](https://github.com/aspiers/shell-env/blob/master/doc/ConfigHooks.org).
*   `.emacs.d/`
    *   `init.d/` -- all the files in this directory are automatically
        loaded at startup.
    *   `lib/` -- `.el` files I wrote which could / should at some point
        be converted into packages, and published so that other people
        can benefit from them too.
*   `lib/emacs/` -- stuff to be autoloaded and used on demand
    *   `Makefile` -- byte-compiles files and generates autoload files
    *   `init/GNU_Emacs/` -- contains `custom-file` files for
        each emacs version I use.  Custom variables / faces change
        between emacs versions, so I keep a file per version.  This is
        maybe not the best way of doing it, but it works OK.  One
        other possibility would be a git branch per version (which was
        not an option when I first implemented this way back before
        git existed), with the disadvantage of requiring a lot more
        back- and forward-porting of stuff between branches, since
        with the current system, porting is only required for the
        contents of the custom files.  OTOH the current scheme is
        incapable of tracking per-version differences, and this is a
        problem when emacs' core API changes.
    *   `major-modes/` -- what it sounds like.
    *   `minor-modes/` -- ditto.
    *   `utils/` -- ditto.
*   `bin/` -- various scripts for launching and interacting with emacs
    in different ways.
*   `lib/xmacro` -- key sequences to be fed into emacs via
    [`xmacro`](http://xmacro.sourceforge.net/) or similar in order
    to automate certain tasks.

INSTALLATION
------------

I would not recommend forking / cloning this repo and attempting to
use it directly.  However, you will probably be able to benefit from
reading through it and stealing at least ideas, if not code.  If you
discover something which you think is useful enough to deserve being
properly packaged and published, then feel free to pester me into
doing so.  Stuff in `.emacs.d/lib` is already earmarked for publishing
when I get free time for it.

This repository is designed to be
[stowed](http://www.gnu.org/software/stow/) directly into your home
directory:

    git clone git://github.com/aspiers/emacs.git
    stow -d . -t ~ emacs

LICENSE
-------

The software in this repository is free software: you can redistribute
it and/or modify it under the terms of the GNU General Public License
as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.