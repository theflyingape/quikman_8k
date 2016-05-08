#!/bin/sh
#
set -o xtrace
ca65 --cpu 6502 --listing --include-dir . mazedata.s
ca65 --cpu 6502 --listing --include-dir . -o quikman+8k.o basic+8k.s
ca65 --cpu 6502 --listing --include-dir . VIC-SSS-MMX.s
ld65 -C basic+8k.cfg -Ln quikman+8k.sym -m quikman+8k.map -o quikman+8k.prg quikman+8k.o VIC-SSS-MMX.o mazedata.o
set +o xtrace
sed -e '1,/^Segment/d' -e '1,1d' -e '/^Exports/,$d' quikman+8k.map

echo -n "Press RETURN: " && read N

#mess -debug -skip_gameinfo -window vic20 -ram 16k -quik quikman+8k.prg
xvic -ntsc -memory 8k -autostart quikman+8k.prg

exit

