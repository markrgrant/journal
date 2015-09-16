# About

A simple utility for creating and editing journal entries

# Setup

    $ cabal sandbox init
    $ cabal init
    $ cabal install --enable-tests

# Configuration

The configuration file must be placed in the current user's home folder
and must be named .journalrc.

To specify the folder in which the journal entries are to be stored, set
the 'dir' configuration variable:

    dir=~/journal

To specify the editor that will be run, set the 'editor' configuration variable:

    editor=vi

# Usage

Create new journal entry:

    $ journal

Edit today's journal entry:

    $ journal

View or Edit a different day's journal entry in yy-mm-dd format:

    $ journal 15-12-01

# Testing

To run the tests:

    % cabal test
