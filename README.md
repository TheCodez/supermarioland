# Super Mario Land Disassembly

This is a disassembly of Super Mario Land.

This repository builds Super Mario Land (World) (Rev A) with SHA1 checksum `418203621b887caa090215d97e3f509b79affd3e`

As of now it requires a copy of the original ROM named `baserom.gb` to be placed in the repository, to fill in sections which have not been disassembled yet. The goal is to make this step obsolete.

## Requirements

* RGBDS 0.3.9
* pypng

## Coverage

A quick and dirty Python script `coverage.py` is provided to estimate how much of the ROM has been disassembled

* Bank 0: 100%
* Bank 1: 33%
* Bank 2: 61%
* Bank 3: 66%

What's left to disassemble are the levels.
