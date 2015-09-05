# About

A simple utility for creating journal entries

# Installation

To build the project and install it in $HOME/bin::

    $ runhaskell Setup.lhs configure --prefix=$HOME --user
    $ runhaskell Setup.lhs build
    $ runhaskell Setup.lhs install

# Usage

Create new journal entry::

    $ journal

Edit today's journal entry::

    $ journal

View or Edit a different day's journal entry in yy-mm-dd format::

    $ journal 15-12-01


# Configuration

The configuration file must be placed in the current user's home folder
and must be named .journalrc.

To specify the folder in which the journal entries are to be stored, set
the 'dir' configuration variable::

    dir=~/journal

To specify the editor that will be run, set the 'editor' configuration variable.

    editor=vi
