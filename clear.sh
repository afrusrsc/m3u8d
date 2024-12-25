#!/bin/bash

find . -type f -name "*.~*" -delete
find . -type f -name "*.tvsconfig" -delete
find . -type f -name "*.identcache" -delete
find . -type f -name "*.stat" -delete
find . -type f -name "*.local" -delete
find . -type f -name "*.skincfg" -delete
find . -type f -name "*.dcu" -delete
find . -type f -name "*.ddp" -delete

find . -type f -name "*.compiled" -delete
find . -type f -name "*.o" -delete
find . -type f -name "*.or" -delete
find . -type f -name "*.ppu" -delete
find . -type f -name "*.lps" -delete
