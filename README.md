# quikman_8k
Commodore VIC20: memory expanded version using modern 6502 assembler

Requires the use of an additional 8k of memory expansion, mainly due to the 
software sprite’s requirement to double-buffer the video display.  For those 
not familiar with the term “k”, it was short for “kilobyte” which is 1,024 
bytes.  Yes, read that again — a thousand bytes, not millions as in megabytes 
and certainly not billions as in gigabytes.  So, eight of those are an 
additional 8,192 bytes over the 3,584 bytes available in a stock VIC 20.  In 
8-bit computing, that is a sizable amount to do some nifty programming.  You 
can appreciate these meager numbers more if you compare it to a simple Windows 
cursor file in C:WindowsCursors — some of those simplest of icons would 
challenge the VIC 20’s ability to load it into memory!

With the double-buffering in place, the game play is smoother because of the 
flicker-free animation.  But the extra address space allowed expansion of the 
game’s core to include:

-	an opening splash screen (top)
-	a gaming options menu (left)
-	one or two player mode
-	choice of arcade maze run: original Pac-Man or progression through the 
four Ms. Pac-Man mazes
-	starting fruit level
-	an interlude
-	pressing the STOP key aborts the current game in progress; at the menu, 
it returns the machine back to BASIC

I suppose the theme for all of this is VIC 20 was different — a trailblazer for 
affordable home computing — and a flame should burn forever marking it as a 
pioneering leader in that space. So us “aging geeks” continue to pour what’s 
left of our diminishing IQ into a piece of hardware that has less memory than 
one contact kept in a cellphone — a genuine human attempt to perpetuate 
validation that VIC 20 could do “just about anything”, despite its handicap 
against that next Commodore model which ultimately made a permanent mark in 
history as “people” know it.

