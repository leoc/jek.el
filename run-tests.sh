#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l ert.el -l s.el -l jek.el -l tests.el -f ert-run-tests-batch-and-exit
