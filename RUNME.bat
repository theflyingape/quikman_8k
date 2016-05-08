choice /C DMV /D M /T 30 /M "[D]ebug, [M]ESS, or [V]ICE? " /N
set CHOICE=%ERRORLEVEL%

if %CHOICE% EQU 1 mess -debug -window -skip_gameinfo -skip_warnings vic20pal -ramsize 16k -quik quikman+8k.prg
if %CHOICE% EQU 2 mess -skip_gameinfo -skip_warnings -newui vic20 -ramsize 16k -quik quikman+8k.prg
if %CHOICE% EQU 3 xvic -memory 8k -autostart quikman+8k.prg

exit
