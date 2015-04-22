# SRFI 114 'Comparators' [![Build Status](https://travis-ci.org/ilammy/srfi-114.svg?branch=master)](https://travis-ci.org/ilammy/srfi-114)

This is an implementation of [SRFI 114 'Comparators'](//srfi.schemers.org/srfi-114/srfi-114.html) for R7RS-compliant Schemes.

## Installation

For development I use [Chibi Scheme](//github.com/ashinn/chibi-scheme) (the latest build from Git master).

You can run `./tools/make-snowball` script to package the library into a snowball which can be installed
then by Snow package manager.

## Extending

Read [docs/extending.md](srfi-114/docs/extending.md) on how to extend `default-comparator` with custom types.

## Licensing

This SRFI implementation is distributed under **[3-clause BSD license](LICENSE)**.
