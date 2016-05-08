@echo on
ca65.exe --cpu 6502 -t vic20 --listing --include-dir . mazedata.s
ca65.exe --cpu 6502 -t vic20 --listing --include-dir . -o quikman+8k.o basic+8k.s
ca65.exe --cpu 6502 -t vic20 --listing --include-dir . vic-sss4.s
ld65.exe -C basic+8k.cfg -Ln quikman+8k.sym -m quikman+8k.map -o quikman+8k.prg quikman+8k.o vic-sss4.o mazedata.o
@echo off

choice /C DMV /D M /T 30 /M "[D]ebug, [M]ESS, or [V]ICE? " /N
set CHOICE=%ERRORLEVEL%

if %CHOICE% EQU 1 mess -debug -window -skip_gameinfo -skip_warnings vic20pal -ramsize 16k -quik quikman+8k.prg
if %CHOICE% EQU 2 mess -skip_gameinfo -skip_warnings -newui vic20 -ramsize 16k -quik quikman+8k.prg
if %CHOICE% EQU 3 xvic -memory 8k -autostart quikman+8k.prg

exit
