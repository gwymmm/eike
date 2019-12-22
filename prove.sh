#!/bin/sh

# run SPARK analysis on file given as first argument
gnatprove -P e_invoice_kit.gpr --report=all -u $1