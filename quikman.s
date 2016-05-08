;*********************************************************************
; Quikman+ (8k) for Commodore VIC 20
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 07-Sep-2014
;
; see COMPILE.bat for compile, link, and go instructions
;
; to run the binary using viceteam.org project:
;	xvic -memory 8k -autostart quikman+8k.prg
; to run the binary using mess.org project:
;	mess vic20 -ramsize 16k -quik quikman+8k.prg
; type RUN after READY.
;
; pertinent VIC20 symbols
CLRPAGE		= $F4		; color memory page (unexpanded = $96)
NVRAM		= $9C5E		; 3-bytes for Quikman hi-score
MEGACART	= $9D80		; memory bank register?
;RESET		= $FD22		; warm startup
;
; my symbol / memory map
PPILLTIMER	= $10		; powerpill effectiveness timer
MONMOVE		= $20		; $20-$23 monster array for its next best move
MONXKB		= $25		; $25,$27,$29,$2B monster's knowledge of quikman's "X" coord was
MONYKB		= $26		; $26,$28,$2A,$2C monster's knowledge of quikman's "Y" coord was
FRUITTIMER	= $39		; 0 - 250
FRUITFLAG	= $3A		; zero or non-zero, if fruit has been activated
PPILLFLAG	= $3B		; just ate a powerpill this turn (0=no)
CHOMP		= $3C		; pointer into sound effect for fruit and fleeing monsters
CHEWING		= $3D		; flag whether quikman just ate a dot or not
PLAYERS		= $3E		; 0=single, 1=two
PLAYERUP	= $3F		; current player (2=demo)
OLDDIR		= $40		; direction sprite was last moving in
NEWDIR		= $41		; direction sprite wants to take, if valid by MAZEMOVE
JOYVAL		= $42		; last joystick read value
QMANDIR		= $43		; quikman's current direction (0=right,1=down,2=left,3=up)
FRAME		= $44		; frame number
DEMO		= $45		; demo mode
DEMOQMAN	= $46		; spirit of quikman index (0-3)
FLASHPILL	= $47		; powerpill blink counter (0-30)
GENDER		= $48		; 0=Original(?), 1=Alternate(??)
SPECIAL		= $49		; classic term for an extra play
MAZE		= $4A		; current maze
PENALTY		= $4B		; $4B-$4E monsters are free-to-roam flag
;			= $4F		; $4F-$52 monsters current direction (0=right,1=down,2=left,3=up)
FLEEINGSCORE= $71		; fleeing monster score: 2, 4, 8, 16

;*********************************************************************
; SOFT reset entry point
;
		.segment "CODE"

RESTART:
		LDA SPRITEDEF+1
		ORA #$80		; enable sprite
		STA SPRITEDEF+1
		STA SPRITEDEF+3
		JSR INITVARS
		LDX #$02
		STX DEMO
		STX PLAYERUP
		JSR PUP
		;
@loop:	LDA FRAME
		AND #$7F
		BNE @slow
@skip:	INC LEVEL
		LDX LEVEL
		CPX #$07
		BEQ MENU
		CPX #$0D
		BCC @fruit
		INC QMANDIR
		LDA QMANDIR
		AND #$03
		STA DEMOQMAN
		TAY
		INY
		LDA #$60		; send a monster back to cage
		STA SPRITEIMGL,Y
		STA PPILLFLAG	; demo powerpill
		LDX #$00
		STX LEVEL
@fruit:	LDA #$50
		STA FRUITTIMER
		STA FRUITFLAG
@slow:	JSR NPC			; demo mode
		LDX #$04
@flick:	LDA SPRITEDEF,X
		EOR #$80
		STA SPRITEDEF,X
		DEX
		BNE @flick
@scan:	JSR GETIN		; get keyboard
		CMP #$85		; got F1 ?
		BEQ MENU
@joy:	LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; got FIRE ?
		BNE @loop

;*********************************************************************
; User menu for new game options
;
MENU:
		JSR WAHKA
        LDX $FFFC		; enable RESTORE key as RESET
        LDY $FFFD
        STX $0318
        STY $0319
		LDX #<SSSFLIP	; restore for normal playing speed
		LDY #>SSSFLIP
		STX SKILL+1
		STY SKILL+2
@menu:	LDX #$02
		STX JIFFYM
		STX PLAYERUP
		DEX
		STX DEMO
		JSR PUP
@maze:	JSR NEWMAZE
		LDA #$8B		; brown & high
		STA VIC+$0E		; auxiliary color & volume
		LDA #$08		; black / black
		STA VIC+$0F		; background / border color
@display:
		LDX #<MENUDATA
		LDY #>MENUDATA
		STX VECTORFG
		STY VECTORFG+1
		LDX #$04
		STX R0
		LDY #$06
		STY R1
		INX
		JSR SSSPLOT
@print:
		LDA #$09
		STA COLORCODE
		LDY #$00
@ftop:	LDA (VECTORFG),Y
		JSR SSSPRINT
		INY
		CPY #$0B
		BNE @ftop
		JSR @nl
		;
		LDA #$09
		STA COLORCODE
		LDY #$00
@fmid1:	LDA (VECTORFG),Y
		JSR SSSPRINT
		INY
		CPY #$04
		BNE @fmid1
		LDA #$07
		STA COLORCODE
@fmid2:	LDA (VECTORFG),Y
		JSR SSSPRINT
		INY
		CPY #$0B
		BNE @fmid2
		JSR @nl
		;
		LDA #$09
		STA COLORCODE
		LDY #$00
@fbot:	LDA (VECTORFG),Y
		JSR SSSPRINT
		INY
		CPY #$0B
		BNE @fbot
		JSR @nl
		;
		DEC R0
		BNE @print
		LDY #$00
		JSR SSSFLIP
		JMP @scan
		;
@nl:	INC R1
		LDY R1
		LDX #$05
		JSR SSSPLOT
		LDA VECTORFG
		CLC
		ADC #$0B
		BCC @cc
		INC VECTORFG+1
@cc:	STA VECTORFG
		RTS
		;
@scan:	JSR GETIN		; get keyboard
@f3:	CMP #$86		; got F3 ?
		BNE @f4
		LDA PLAYERS
		EOR #$01
		STA PLAYERS
		LDA #$11
		STA CHOMP
		JMP @menu
@f4:	CMP #$8A		; got F4 ?
		BNE @f5
		LDA F4+1
		EOR #$06		; toggle 3 or 5 lives
		STA F4+1
		CMP #$05
		BEQ @5
		LDX #$31
		BNE @bell
@5:		LDX #$51
@bell:	STX SPECIAL		; ring my bell
		JSR DOLIVES
		JSR DOFRUIT
		JMP @display
@f5:	CMP #$87		; got F5 ?
		BNE @f6
		LDA GENDER
		EOR #$01		; toggle between M/M mazes
		STA GENDER
		LDA #$00
		STA STARTING
		JSR WAHKA
		JMP @menu
@f6:	CMP #$8B		; got F6 ?
		BNE @f7
		LDY #$17
		LDX #$00
		JSR SSSPLOT
		JSR SSSPRINTS	; but does DENIAL love me?
		.byte $F6,$5C,$5D,$5E,$5F,$F2,$D3,$F3,$84,$85,$8E,$89,$81,$8C,$F7,$67,$F4,$67,$00
		JSR SSSFLIP
@f7:	CMP #$88		; got F7 ?
		BNE @f8
		INC STARTING
		LDA STARTING
		LDY GENDER
		BEQ @qm
		CMP #$08
		BCC @fruit
		BCS @msqm
@qm:	CMP #$0D
		BCC @fruit
@msqm:	LDA #$00
		STA STARTING
@fruit:	JSR WAHKA
		JMP @menu
@f8:	CMP #$8C		; got F8 ?
		BNE @f1
		LDX #$00
		STX PLAYERUP
		STX DEMO
		JSR INTERLUDE
		JMP MENU
@f1:	CMP #$85		; got F1 ?
		BEQ @go
@f2:	CMP #$89		; got F2 ?
		BEQ @gofast
@joy:	LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; got FIRE ?
		BEQ @go
		LDA JIFFYM
		AND #$07
		BEQ @attract
		JMP @scan
@attract:
		INC DEMO
		JSR NEWMAZE
		JSR GAMEOVER
		JMP RESTART
		;
@gofast:
		LDX #<SSSFFLIP
		LDY #>SSSFFLIP
		STX SKILL+1
		STY SKILL+2
@go:	LDX #$00
		STX DOTS+1		; flag that player #2 has not started a game
		STX PLAYERUP
		STX DEMO
		TXA
@zero:	STA SCORE1,X
		INX
		CPX #6
		BNE @zero

;*********************************************************************
RESETGAME:
		LDX PLAYERUP
F4:		LDA #$03		; start with 3-lives
		STA LIVES,X
		LDA STARTING
		STA LEVEL,X
		DEC LEVEL,X
		JSR NEWMAZE
		LDA #$AC		; pink & high
		STA VIC+$0E		; auxiliary color & volume
		; introduction
		JSR INITVARS
		LDA #>QANIM		; 1st page where quikman is on
		STA SPRITEIMGH
		LDA #<QANIM		; only quikman appears as a ball
		STA SPRITEIMGL
		LDA SPRITEDEF
		ORA #$80		; enable sprite
		STA SPRITEDEF
		LDA GENDER		; Mr. or Mrs.?  ;)
		BNE @alt
		LDX #<INTRO		; original
		LDY #>INTRO
		BNE @tune
@alt:	LDX #<INTRO2	; alternate
		LDY #>INTRO2
@tune:	STX SHEET
		STY SHEET+1
		LDA #$01
		STA DELAY
		STA NOTES		; init music player
		;
RESETCHR:
		JSR INITVARS
		JSR PUP
		;
		LDX PLAYERUP
		INX
		TXA
		ORA #$B0
		STA @p+2
		LDX #FRUITCELLX-4
		LDY #FRUITCELLY
		JSR SSSPLOT
		JSR SSSPRINTS	; Pn READY!
@p:		.byte	$F7,$90,$B1,$A0,$92,$85,$81,$84,$99,$67,$00
@wait:	LDY #$00
		JSR SSSREFRESH
		JSR SSSFLIP
		LDA NOTES
		BEQ @ok
		CMP #$20
		BCC @wait
		;
@ok:	LDA #>QMAN
		STA SPRITEIMGH
		LDA #<QMAN		; start quikman off with a smug smile
		STA SPRITEIMGL
		LDX #$04
@on:	LDA SPRITEDEF,X
		ORA #$80		; enable sprite
		STA SPRITEDEF,X
		DEX
		BPL @on
		LDX #$0F
@smug:	LDA #$05
		JSR PAUSE		; then he sees there are monsters ...
		DEX
		BPL @smug
		LDX #$04
@loop2:	LDA #<GHOST5
		STA SPRITEIMGL,X	; restore monster caged image (heh)
		LDA #>GHOST5
		STA SPRITEIMGH,X
		DEX
		BNE @loop2
		STX FRUITFLAG
		STX PPILLFLAG
@set:	LDX #>QANIM
		STX SPRITEIMGH
		LDX #<(QANIM+40)
		STX SPRITEIMGL	; quikman gets ready
		LDX #$0F
@ready:	LDA #$05
		JSR PAUSE		; anim loop
		DEX
		BPL @ready
		LDX #FRUITCELLX-4
		LDY #FRUITCELLY
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$00
		LDA #$01
		JSR PAUSE
		;
		LDX #$02
		STX QMANDIR		; start off going LEFT
		STX JOYVAL		; preload last joystick value as going LEFT

;*********************************************************************
PLAYLOOP:
		LDY #$00
		STY $00			; quikman is sprite #0
		STY $01
		LDA QMANDIR
		STA OLDDIR		; save last direction quikman was going in
		LDA SPRITEX
		CMP #$11
		BCC @skip1		; no new moves while in the void
		CMP #$B0
		BCS @skip1		; no new moves while in the void
		LDX JOYVAL		; recall last joystick value
		STY $9113
		LDA #$7F
		STA $9122
		LDA $9120
		AND #$80		; JOY 3
		BNE @joy0
		LDX #$00
@joy0:	LDA #$FF
		STA $9122
		LDY $9111
		TYA
		AND #$08
		BNE @joy1
		LDX #$01
@joy1:	TYA
		AND #$10
		BNE @joy2
		LDX #$02
@joy2:	TYA
		AND #$04
		BNE @joy3
		LDX #$03
@joy3:	STX JOYVAL		; save
		TXA
		STA NEWDIR		; do the same for the joystick
		JSR MAZEMOVE
		BCS @skip1		; is the direction valid?
		LDA JOYVAL		; yes,
		STA QMANDIR		; request quikman to move in direction of joystick
		CLC
		BCC @skip2
@skip1:	LDA QMANDIR
		STA NEWDIR
		JSR MAZEMOVE	; keep the current direction going?
		BCS @sss	; is the direction valid?
@skip2:	LDA SPRITEX
		CMP #$09
		BCS @skip3		; is quikman at end of tunnel left?
		LDX SSSCLIPX
		DEX
		TXA
@skip3:	CMP SSSCLIPX	; is quikman at end of tunnel right?
		BCC @skip4
		LDA #$09
@skip4:
		STA SPRITEX		; put quikman at beginning of tunnel left
		;
		LDA SPRITEZ
		AND #$01
		ORA #%11000000	; force quikman sprite xfer/shift/copy/merge
		STA SPRITEZ
		LDX #>QANIM
		STX SPRITEIMGH
		LDX #<QANIM		; closed mouth
		LDA FRAME
		LSR
		AND #$03		; 00=close, 01=partial, 10=open, 11=partial
		TAY
		BEQ @anim
		LDA QMANDIR		; take 0=right, 1=down, 2=left, 3=up value
@x16:	ASL				; multiply by 16 to get partial
		ASL
		ASL
		ASL
		CPY #$02		; open?
		BNE @x2
		ORA #8
@x2:	CLC
		ADC #8			; add base offset
@x:		TAX
@anim:	STX SPRITEIMGL
		;
@sss:	LDX #%10001000
		LDA QMANDIR
		AND #$01
		BEQ @xy
		LDX #%10000100
@xy:	STX SPRITEDEF	; redefine sprite as moving X or Y direction only
		;
		LDX #$00		; PEEK: X indicates any cell offset for right or down
		LDA SPRITEX
		AND #$07
		CMP #$04
		BNE @skip5		; is quikman in the middle of a left/right cell?
		LDA QMANDIR		; yes, 0=right, 2=left
		EOR #$02
		BEQ @peek		; going left, no offset
		INX
		BNE @peek		; going right, use next
@skip5:	LDA SPRITEY
		AND #$07
		CMP #$04
		BNE @skip		; is quikman in the middle of an up/down cell?
		LDA QMANDIR		; yes, 1=down, 3=up
		CMP #$03		; going up, no offset
		BEQ @peek
		LDX PLAYCOLS	; going down, ahead for next row
		BNE @peek
@skip:	JMP NPCNEXT		; no, continue play
@peek:	STX R0			; save any offset
		LDX SPRITEX
		LDY SPRITEY
		LDA #$00		; when you are in the void,
		JSR SSSPEEKXY	; return a null
		CMP #$00
		BEQ @quik
		LDA CRSRCOL
		CLC
		ADC R0
		TAY
		LDA (SCRNLINE),Y	; retrieve tile from playfield
		CMP #SSSNULL
		BNE @food
@quik:	JMP PLAYLOOP	; nothing to eat here, so move a bit faster
		; check what was just eaten ...
@food:	TAX				; save that something in X
		CPX #SSSNULL-11	; something to eat?
		BCC @next		; nope
		CPX #SSSNULL+3	; walls?
		BCC @erase		; something edible
@next:	JMP NPCNEXT
@erase:	LDA #SSSNULL	; replace the cell quikman is on with an empty space
		STA (SCRNLINE),Y
		CPX #$21		; is it a dot?
		BNE POWERUP
		STX CHEWING		; quikman has to chew this dot, monsters keep movin'
		JSR WAHKA
		LDY #$00
		LDA #$10		; award 10-points
		JMP EATING
		;
POWERUP:
		CPX #$22		; is it a powerpill?
		BEQ @pp
		LDA #$11
		STA CHOMP		; YUMMY!
		TXA
		SEC
		SBC #SSSNULL-11	; strip off char code for score index
		PHA				;++
		TAX
		LDA FRUITSCORE,X
		LDY #$01		; fruit bonus x100
		JSR SCOREUPDATE
		LDA #$80
		STA FRUITTIMER
		LDA #$03
		STA COLORCODE
		LDX #FRUITCELLX-1
		LDY #FRUITCELLY
		JSR SSSPLOT
		PLA				;--
		STA R0
		ASL
		ADC R0			; x3
		TAX
		LDY #$03
@fs:	LDA FRUITVAL,X
		JSR SSSPRINT
		INX
		DEY
		BNE @fs
		BEQ NPCNEXT
		;
@pp:	STX PPILLFLAG
		LDY #$00
		LDA #$50		; award 50-points
EATING:
		JSR SCOREUPDATE
		LDX PLAYERUP
		DEC DOTS,X		; ate a dot, account for it
		BNE NPCNEXT
;===	achieved end of level	===
WONLEVEL:
		LDA #<QANIM		; quikman ends snoozing
		STA SPRITEIMGL
		LDX #$50
@smug:	LDA #$01
		JSR PAUSE
		DEX
		BNE @smug
		LDX #$04
@off:	LDA SPRITEDEF,X
		AND #$7F
		STA SPRITEDEF,X
		DEX
		BNE @off
		STX PPILLTIMER
		STX VIC+$0B		; mute any powerpill siren remaining
		STX FRAME
@loop:	LDA VIC+$0F
		AND #$07		; get border color
		LDX FRAME
		ORA BGCOLOR,X
		STA VIC+$0F		; cycle screen colors
		LDA #$07
		JSR PAUSE
		LDA FRAME
		AND #$03
		BNE @next
@next:	INC FRAME
		LDA FRAME
		CMP #$0F
		BNE @loop
		LDA #<QMAN
		CMP SPRITEIMGL
		BEQ @fini
		STA SPRITEIMGL
		LDA #>QMAN
		STA SPRITEIMGH
		LDX #$01
		BNE @smug
@fini:	LDA #$50
		JSR PAUSE
		LDX PLAYERUP
		JSR NEWMAZE
		JMP RESETCHR
;
NPCNEXT:
		JSR NPC
		JMP PLAYLOOP

;*********************************************************************
; non-player characters & events
;
NPC:
		LDA FRUITFLAG
		BNE @skip3		; is fruit (or fruit score) already on display?
		LDX PLAYERUP
		LDA DOTS,X
		CMP #$60		; are there 96 dots left?
		BEQ @skip1
		CMP #$30		; are there 48 dots left?
		BNE @skip3
@skip1:
		LDA LEVEL,X		; prepare thy bonus
		TAX
		LDY GENDER
		BEQ @maze
		LDA FRUIT2,X
		CPX #$07		; reach the last level?
		BCC @skip2
		LDA FRAME		; random fruit
		AND #$0F
		CMP #$0B		; > 11 ?
		BCC @msqm
		LSR				; limit to 1st eight
@msqm:	CLC
		ADC #$15
		BCC @skip2
@maze:	CPX #$0C		; reach the last level?
		BCC @table
		LDX #$0C		; only the key is left, and it leaves a bad metallic after-taste
@table:	LDA FRUIT,X
@skip2:	PHA				;++
		SEC
		SBC #$15
		TAX
		LDA FRUITCLR,X
		STA COLORCODE
		LDX #FRUITCELLX
		LDY #FRUITCELLY
		JSR SSSPLOT
		PLA				;--
		JSR SSSPOKE		; display fruit
		LDA #$FF		; 255-moves and counting
		STA FRUITTIMER	; reset fruit timer
		STA FRUITFLAG
@skip3:	LDA FRUITTIMER	; fruit is on display
		BEQ @skip4		; nothing to do
		DEC FRUITTIMER	; remove a tick
		BNE @skip6		; there is still time left
		LDX #FRUITCELLX-1
		LDY #FRUITCELLY
		LDA DEMO
		BEQ @demo
		INX
@demo:	JSR SSSPLOT
		LDA #SSSNULL	; time's up!
		JSR SSSPRINT	; no more fruit
		LDA DEMO		; playing?
		BNE @skip4
		JSR SSSPRINTS	; time's up! no more fruit score
		.byte $20,$20,$00
@skip4:	LDX PLAYERUP
		LDA DOTS,X
		CMP #$30		; has the 48th dot been eaten?
		BEQ @skip5
		CMP #$60		; has the 96th dot been eaten?
		BNE @skip6
@skip5:	LDA #$00
		STA FRUITFLAG	; more fruit potential on this level
;
@skip6:	LDA PPILLFLAG
		BEQ KISSING		; just swallowed a powerpill?
		LDA #$00		; account for that action
		STA PPILLFLAG
		LDA #$02		; start scoring @ 200-points
		STA FLEEINGSCORE
		LDA DEMO
		BNE @fake
		LDX PLAYERUP
		LDA LEVEL,X
@fake:	TAX
		AND #$07
		BEQ @break		; every 8-levels, keep timer up
		CPX #$10
		BCS @timer		; 16-levels of powerpill timing
		TXA
@break:	ASL				; x2
		ASL				; x2
		AND #$3F
		EOR #$3F		; invert A
		CPX #$05
		BCC @timer		; timer good to the 1st apple
		LSR				; 1/2
@timer:	STA PPILLTIMER	; set powerpill timer
		LDY #$04
@loop1:	LDX PENALTY-1,Y
		BNE @skip7		; is monster waiting in cage already?
		LDA SPRITEIMGL,Y
		CMP #$60		; is monster returning to cage?
		BEQ @skip7
		LDA #$06		; no, make monster blue
		STA SPRITECOL,Y
		LDA #$90		; make monster fleeing
		STA SPRITEIMGL,Y
		LDA $4E,Y
		EOR #$02		; and reverse its direction
		STA $4E,Y
@skip7:	DEY
		BNE @loop1
;
; check all monsters if any are in contact with quikman
KISSING:
		LDY #$04
KISSME:
		LDA DEMO		; playing?
		BNE NEXTKISS
		LDA SPRITEX
		CMP SPRITEX,Y
		BNE @skip3
		LDA SPRITEY
		SEC
		SBC SPRITEY,Y
		BCS @skip2
		EOR #$FF
@skip2:	CMP #$05
		BCS @skip3
		BCC ENGAGED		; is quikman engaged with a monster?
@skip3:	LDA SPRITEY
		CMP SPRITEY,Y
		BNE NEXTKISS
		LDA SPRITEX
		SEC
		SBC SPRITEX,Y
		BCS @skip4
		EOR #$FF
@skip4:	CMP #$05
		BCC ENGAGED		; is quikman engaged with a monster?
;
NEXTKISS:
		DEY				; next monster
		BNE KISSME
		JMP MONSTERS	; quikman is still freely running!
;
ENGAGED:
		LDA SPRITEIMGL,Y
		CMP #$60		; is monster returning to cage?
		BEQ NEXTKISS
		CMP #$90		; is monster fleeing?
		BNE DEAD		; no, quikman bites the dust
		TYA
		PHA				;++
		TXA
		PHA				;++
		; ahah!  caught a little sickly one!
		LDA SPRITEDEF,Y
		AND #$7F		; disable sprite
		STA SPRITEDEF,Y
		LDA FLEEINGSCORE
		CMP #$10
		BNE @dec
		LDA #$00
		STA PPILLTIMER
		STA VIC+$0B		; mute any powerpill timer remaining
		STA FLEEINGSCORE
		LDA #$16		; fix for 1600 in decimal mode
@dec:	LDY #$01		; x100
		JSR SCOREUPDATE
		ASL	FLEEINGSCORE ; next is worth x2 bonus
		LDA #$D0		; w..o..o..t..!
		STA VIC+$0A
		LDX PPILLTIMER
		LDA #>QMAN
		STA SPRITEIMGH
		LDA #<QMAN		; quikman smiles
		STA SPRITEIMGL
		LDA #$01
		JSR PAUSE
		LDA JIFFYL
		CLC
		ADC #$20
@loop:	LDY JIFFYL
		INY
@loop1:	CPY JIFFYL
		BNE @loop1		; wait up to a jiffy
		STX PPILLTIMER
		INC VIC+$0A
		CMP JIFFYL
		BNE @loop
		LDA #$00		; mute
		STA VIC+$0A
		LDA #>QANIM
		STA SPRITEIMGH
		LDA #<QANIM		; quikman snoozes
		STA SPRITEIMGL
		PLA				;--
		TAX
		PLA				;--
		TAY
		LDA #$60		; monster runs back to cage
		STA SPRITEIMGL,Y
		LDA SPRITEDEF,Y
		ORA #$80		; enable sprite
		STA SPRITEDEF,Y
		LDA QUIKMANCLR,Y
		STA SPRITECOL,Y
		JMP NEXTKISS	; is there another monster here?
;
DEAD:
		PLA				; remove quikman's call to NPC from stack
		PLA				; because he just died . . .
		LDX #$00
		STX PPILLTIMER
		STX VIC+$0B		; mute any powerpill siren remaining
		LDX #$10
@oops:	LDA #$06
		JSR PAUSE
		DEX
		BNE @oops
		; death sequence
		LDX #$04		; only feature quikman dying
@off1:	LDA SPRITEDEF,X
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		DEX
		BNE @off1
		LDA #<QANIM		; low-order byte of 1st quikman image
		STA SPRITEIMGL
		LDX #$04
@loop:	LDA QUIKMANCLR,X ; reset monsters starting colors
		STA SPRITECOL,X	; into their sprite color registers
		LDA #$70		; reset monsters as looking down
		STA SPRITEIMGL,X
		DEX
		BNE @loop
		LDA #$5B
		STA CHOMP
		;
		LDA #$14
		STA FRAME		; rotate quikman 5 times
@loop1:	LDA SPRITEIMGL
		CMP #<(QANIM+64) ; are we at the 4th quikman image?
		BCC @skip
		LDA #<(QANIM)	; reset to 1st quikman image
@skip:	CLC
		ADC #$10		; advance to next image
		STA SPRITEIMGL
		LDX #04			; NTSC delay
		LDA MACHINE
		CMP #$05		; NTSC
		BEQ @paws
		DEX				; PAL needs to go a bit faster
@paws:	TXA
		JSR PAUSE
		DEC FRAME
		BNE @loop1		; repeat next sequence
		;
		LDX #>POOF1		; POOF!
		STX SPRITEIMGH
		LDX #<POOF1
		STX SPRITEIMGL	; explode!
		LDA #$0A
		JSR PAUSE
		LDX #<POOF2
		STX SPRITEIMGL	; smoke!
		LDA #$08
		JSR PAUSE
		LDX #<POOF3
		STX SPRITEIMGL	; dust!
		LDA #$06
		JSR PAUSE
		LDA SPRITEDEF
		AND #$7F		; disable sprite
		STA SPRITEDEF
		LDA #$04
		JSR PAUSE
		;
		LDX PLAYERUP
		DEC LIVES,X
		BEQ FINALITY	; any lives remaining?
		LDA #$10
		JSR PAUSE
		LDA #$3B
		STA CHOMP
		LDA #$07
		STA COLORCODE
		LDY #$17
		LDA LIVES,X
		TAX
		JSR SSSPLOT
		LDA #$23
		JSR SSSPOKE
		LDA #$09
		JSR PAUSE
		LDA #$24
		JSR SSSPOKE
		LDA #$08
		JSR PAUSE
		LDA #$25
		JSR SSSPOKE
		LDA #$07
		JSR PAUSE
		LDA #SSSNULL	; erase life
		JSR SSSPOKE
		JSR INITVARS
		JSR DOLEVEL
		LDX PLAYERS
		BEQ @p1
		JSR SWAPMAZE
@p1:	JMP RESETCHR	; quikman still has life -- try again!
;
FINALITY:
		LDA #$FE
		STA MEGACART
		CMP MEGACART	; detect if this memory location is really writable
		BNE @cont		; no, might be an emulator or a real VIC with only 8k
		LDA HISCORE
		LDY HISCORE+1
		LDX HISCORE+2
		STA NVRAM
		STY NVRAM+1
		STX NVRAM+2
@cont:
		JSR GAMEOVER
		LDA #$A0
		JSR PAUSE
		LDX PLAYERS
		BEQ @p1
		JSR SWAPMAZE
		LDX PLAYERUP
		LDA LIVES,X
		BEQ @p1			; any lives remaining?
		JMP RESETCHR
@p1:	JMP RESTART		; this game is really over now

;*********************************************************************
GAMEOVER:
		LDX #$06
		LDY #$0D
		JSR SSSPLOT
		JSR SSSPRINTS	; print GAME OVER in red
		.byte	$F2,$87,$81,$8D,$85,$A0,$8F,$96,$85,$92,$00
		LDY #$00
		JSR SSSFLIP		; gratuitous
		RTS

;*********************************************************************
MONSTERS:
		LDA #$04
		STA $00			; start with monster #4
		LDA FRAME
		AND #$07
		BNE DOMONSTER
		DEC $00
		LDA FRAME
		AND #$0F
		BNE DOMONSTER
		DEC $00
;
DOMONSTER:
		LDA $00
		TAY
		LDA SPRITEZ,Y
		AND #$01
		ORA #%11000000	; force sprite xfer/shift/copy/merge
		STA SPRITEZ,Y
		TYA
		ASL				; x2
		STA $01
		LDX PENALTY-1,Y
		BNE @cage
		JMP ITMOVES		; is this monster free to roam?
@cage:	LDA FRAME
		AND #$03
		BEQ @anim
		DEX				; no, countdown to freedom
		STX PENALTY-1,Y
@anim:	LDA #$88		; reset monster as caged, but coming out
		CPX #$10
		BCC @eyes
		TXA
		AND #$08
		BNE @skip
		LDA #$68		; reset monster as caged looking right
		BNE @eyes
@skip:	LDA #$78		; reset monster as caged looking left
@eyes:	LDX $01
		STA SPRITEIMGL,Y
		LDA SPRITEX,Y
		AND #$F8
		STA SPRITEX,Y
;
NEXTMONSTER:
		LDX PLAYERUP
		DEC $00			; process next monster
		BNE DOMONSTER
		INC $00			; let's see if red needs a burst
		LDA SPRITEY+1
		CMP #$68		; no bonus at cage row
		BEQ @fini
		AND #$07
		ORA SPRITEX+1
		AND #$0F
		BEQ DOMONSTER
		LDA CHEWING
		BEQ @fini		; is quikman eating a dot?
		LDA #$00
		STA CHEWING
		LDA DOTS,X
		AND #$01
		BNE MONSTERS	; quikman can swallow every other dot faster
		;
@fini:	LDY DEMO		; playing?
		BNE SKILL
		LDY #$00		; no VSYNC
		LDA LEVEL,X
		CMP #$05
		BCS SKILL		; level 5+ goes at full speed
		TAX
		LDA MACHINE
		CMP #$05
		BEQ @ntsc
@pal:	INX				; PAL timing needs to go a bit faster
@ntsc:	LDA FRAME
		AND #$07
		TAY
		LDA MASK,Y
		AND SPEED,X
		TAY
		BEQ SKILL		; don't wait this frame
		LDY #$02		; wait for VSYNC
SKILL:	JSR SSSFLIP
		INC FRAME
		RTS				; no, we're done
;
ITMOVES:
		LDX $01			; get pairing index
		LDA SPRITEIMGL,Y
		CMP #$60
		BNE @go			; small ghost going home?
		LDA SPRITEX,Y
		CMP #$60
		BNE @caged
		LDA SPRITEY,Y
		CMP #$58
		BNE @caged		; is monster above cage ($60,$58 coord) doorway ?
		LDX $00
		INC SPRITEY,X	; move into doorway
		LDX #$01
		STX $4E,Y		; make direction DOWN to go into cage
@caged:	LDX $01			; get pairing index
		LDA SPRITEY,Y
		CMP #$68
		BNE @skip1		; is monster at cage row level?
		TYA
		ASL
		ASL
		ASL
		CLC
		ADC #$50
		CMP SPRITEX,Y	; is monster inside cage?
		BNE @skip1
@home:	LDA #$88		; restore monster caged image
		STA SPRITEIMGL,Y
		LDA CAGEDATA-1,Y
		EOR #$FF
		LSR
		ADC #$20		; compute waiting room time
		STA PENALTY-1,Y	; monster is waiting
		JMP NEXTMONSTER	; done moving
@go:	LDA FRAME
		ROR
		BCC @cont		; check for powerpill active?
		LDA GENDER
		BNE @flee		; not original maze
		LDA SPRITEY,Y
		CMP #$68
		BNE @flee		; is monster at cage row level?
		LDA SPRITEX,Y
		CMP #$30
		BCC @next		; in tunnel left?
		LDA SPRITEX,Y
		CMP #$91		; in tunnel right?
		BCS @next
@flee:	LDA SPRITEIMGL,Y
		CMP #$90		; this monster IS fleeing
		BNE @cont
@next:	JMP NEXTMONSTER	; skip its turn
@cont:	LDX $00
		LDA SPRITEX,X
		CMP #$60
		BNE @skip1
		LDA SPRITEY,X
		CMP #$68
		BNE @skip1		; is monster in cage ($60,$68 coord) doorway ?
		DEC SPRITEY,X	; move it a pixel UP to force it through the closed door
		LDX #$03
		STX $4E,Y		; make direction UP to get out of cage
		BNE @skip3
@skip1:	LDA SPRITEX,Y
		CMP #$11
		BCS @skip2		; is monster against the left-side of the tunnel?
		LDA #$00
		LDX $00
		STA $4E,X		; force a change of direction to the right
@skip2:	CMP #$AF		; is monster against the right-side of the tunnel?
		BCC @skip3
		LDA #$02
		LDX $00
		STA $4E,X		; force a change of direction to the left
@skip3:	LDY #$00
		LDX #$00
@loop1:	STX MONMOVE,Y	; preset move priority (0=right,1=down,2=left,3=up)
		INY
		INX
		CPX #$04
		BNE @loop1
		LDY $00			; start of monster's calculated move
		LDA SPRITEX,Y
		AND #$07
		BEQ @skip4		; is monster horizontally aligned with a screen cell?
		LDA SPRITEY,Y
		AND #$07
		BNE @skip5		; is monster vertically aligned with a screen cell?
@skip4:	JSR AI			; yes, check to see if a direction change is in its future
		CLC
		BCC @skip6
@skip5:	LDX $4E,Y		; not in a position to make a direction change,
		STX MONMOVE		; so just keep monster going in its current direction
@skip6:	LDY #$00
		STY $04
@loop2:	LDX MONMOVE,Y
		TXA
		LDX $00
		EOR $4E,X
		CMP #$02
		BEQ @skip7		; don't allow monsters to reverse direction on their own
		LDX MONMOVE,Y
		STX NEWDIR
		LDY $00
		LDX $4E,Y
		STX OLDDIR
		JSR MAZEMOVE	; validate
		BCC MAKEMOVE	; is this a good move?
@skip7:	INC $04
		LDY $04
		CPY #$04
		BNE @loop2
		LDY $00			; reverse direction
		LDA OLDDIR
		EOR #$02
		STA $4E,Y
		JMP NEXTMONSTER
;
MAKEMOVE:
		LDY $00			; commit to this move
		LDX NEWDIR
		STX $4E,Y		; save as monster's current direction
		LDA SPRITEDEF,Y
		AND #$F0
		TAY
		TXA
		AND #1
		BNE @ud
		TYA
		ORA #$08		; float X
		BNE @cont
@ud:	TYA
		ORA #$04		; float Y
@cont:	LDY $00
		sta SPRITEDEF,Y	; enable proper float X/Y bit
		LDA SPRITEIMGL,Y
		CMP #$60		; running back to cage
		BEQ @mommy		; to give birth again
		CMP #$90		; fleeing from quikman
		BEQ @anim
		TXA
		ASL				; multiply by 8 to get address
		ASL
		ASL
		CLC
		ADC #$68		; add base offset
@anim:	STA SPRITEIMGL,Y
@jmp:	JMP NEXTMONSTER
@mommy:
		LDY $00			; start of monster's calculated move
		LDA SPRITEX,Y
		ORA SPRITEY,Y
		AND #$02
		BEQ @jmp
		JMP ITMOVES

;*********************************************************************
; monster's artificial intelligence
;
AI:		; first, preload MONX/YKB with "best" moves this monster can make to give
		; quikman the kiss of death
		LDY $00
		LDX $01
		LDA MONXKB-2,X	; retrieve this monster's "X" knowledge where quikman was
		CMP SPRITEX,Y	; aligned?
		BNE @math1		; nope, compute intersect
		LDA JIFFYL
		AND #$01
		BEQ @skip1		; flip a coin
@math1:	SEC
		SBC SPRITEX,Y
		BCS @skip1
		LDY #$02
		STY MONMOVE		; LEFT is better
		LDY #$00
		STY MONMOVE+3	; RIGHT is worse
		BEQ @skip2
@skip1:	LDY #$00
		STY MONMOVE		; RIGHT is better
		LDY #$02
		STY MONMOVE+3	; LEFT is worse
@skip2:	LDA MONYKB-2,X	; retrieve this monster's "Y" knowledge where quikman was
		LDY $00
		CMP SPRITEY,Y	; aligned?
		BNE @math2		; nope, compute intersect
		LDA JIFFYL
		AND #$01
		BEQ @skip3		; flip a coin
@math2:	SEC
		SBC SPRITEY,Y
		BCS @skip3
		LDY #$03
		STY MONMOVE+1	; UP is 2nd best
		LDY #$01
		STY MONMOVE+2	; DOWN is 3rd best
		BNE AI2
@skip3:	LDY #$01		; DOWN is 2nd best
		STY MONMOVE+1
		LDY #$03		; UP is 3rd best
		STY MONMOVE+2
		;
AI2:	; next, prioritize monster move, based upon its current location in
		; respect to its knowledge where quikman was considered last.
		LDY $00
		LDX $01
		TXA
		ASL
		ASL
		ASL				; x8
		CMP JIFFYL
		BCS @skip3		; ignore priority during this time window
		LDA MONXKB-2,X
		SEC
		SBC SPRITEX,Y
		BCS @skip1
		EOR #$FF
@skip1:	STA R0
		LDA MONYKB-2,X
		SEC
		SBC SPRITEY,Y
		BCS @skip2
		EOR #$FF
@skip2:	CMP R0
		BCC @skip3		; can monster improve upon order of choices?
		LDX MONMOVE		; swap 1st & 2nd choices
		LDY MONMOVE+1
		STX MONMOVE+1
		STY MONMOVE
		LDY MONMOVE+2	; swap 3rd & 4th choices
		LDX MONMOVE+3
		STY MONMOVE+3
		STX MONMOVE+2
@skip3:	LDY $00
		LDA SPRITEIMGL,Y
		CMP #$90		; is this monster fleeing?
		BNE @fini		; no, chase!
		LDX MONMOVE		; swap 1st & 3rd choices
		LDY MONMOVE+2
		STX MONMOVE+2
		STY MONMOVE
@fini:	RTS

;*********************************************************************
; if move is valid, carry flag will be clear on return
;
MAZEMOVE:
		LDY $00			; get X,Y coord index
		LDA OLDDIR		; get the last direction moving
		AND #$01		; mask UP/DOWN
		BEQ @skip1		; is direction LEFT/RIGHT?
		LDA SPRITEY,Y	; no, then fetch the "Y" coordinate
		AND #$07
		BEQ MAZEANY		; at a crossroad?  check move in any 4-directions
		BNE @skip2
@skip1:	LDA SPRITEX,Y	; get one of sprite's coord
		AND #$07
		BEQ MAZEANY		; at a crossroad?  check move in any 4-directions
@skip2:	LDA NEWDIR
		CMP OLDDIR
		BEQ MYMOVE		; still want to move in the same direction?
		EOR OLDDIR
		CMP #$02
		BEQ MYMOVE		; is this a reverse direction request?
@ng:	SEC				; no new move made
		RTS
;
MAZEANY:
		LDY $00
		LDX NEWDIR
		CPX #$02
		BPL @skip2		; is X (2=left) or (3=up)?
		CPX #$01
		BEQ @d01		; is X (1=down)?
		LDA SSSCLIPX
		SEC
		SBC SPRITEX,Y
		CMP #$09
		BCC MYMOVE		; passthru tunnel right
@d01:	LDA SPRITEX,Y
		TAX
		LDA SPRITEY,Y
		TAY
		JSR SSSPEEKXY
		LDX NEWDIR
		LDA CRSRCOL
		CLC
		ADC PEEKAHEAD,X
		TAY				; look (0=right) or (1=down)
		LDA (SCRNLINE),Y
		JMP @skip4		; go validate
		;
@skip2:	LDA SPRITEX,Y
		CPX #$03
		BEQ @d23		; is X (3=up)?
		CMP #$10		; going left,
		BEQ MYMOVE		; and allow passthru tunnel left
@d23:	TAX
		LDA SPRITEY,Y
		SEC
		SBC #$08
		TAY
		JSR SSSPEEKXY
		LDX NEWDIR
		LDA CRSRCOL
		CLC
		ADC PEEKAHEAD,X
		TAY				; look (2=left) or (3=up)
		LDA (SCRNLINE),Y
@skip4:	CMP #$26		; is this direction into a maze wall?
		BCC MYMOVE		; good move?
		CMP #$40
		BCC @ng			; end of maze tiles
		CMP #$C0
		BCC MYMOVE		; passthru PETSCII
@ng:	SEC
		RTS

;*********************************************************************
; continue this sprite's move in whatever is loaded in NEWDIR
;
MYMOVE:
		LDA NEWDIR
		ASL				; 0=0, 1=2, 2=4, 3=6, 4=8
		TAX
		LDY $00
		LDA INERTIA,X
		CLC
		ADC SPRITEX,Y
		STA SPRITEX,Y
		LDA INERTIA+1,X
		CLC
		ADC SPRITEY,Y
		STA SPRITEY,Y
		CLC
		RTS

;*********************************************************************
; reset monsters to default starting location
;
INITVARS:
		LDX #$04
@loop1:	LDA STARTPOSX,X
		STA SPRITEX,X
		LDA STARTPOSY,X
		STA SPRITEY,X
		DEX
		BPL @loop1
@loop2:	LDA CAGEDATA,X
		STA PENALTY,X
		INX
		CPX #$10
		BNE @loop2
		;
		LDX PLAYERUP
		LDA DOTS,X
		CMP #$0A
		BCC @adj		; < 10 yields faster release
		CMP #$9C
		BCC @fini		; > 155 yields faster release
@adj:	LDA LEVEL,X
		BEQ @fini
		PHA				;++
		TAY
		LDX #$03
@loop3:	PLA				;--
		PHA				;++
		TAY
		LDA PENALTY,X
@loop4:	LSR
		DEY
		BNE @loop4
		STA PENALTY,X	; after each level, the monsters dispatch quicker
		DEX
		BPL @loop3
		PLA				;--
@fini:	RTS

;*********************************************************************
; compute which maze from level
; pass X with PLAYERUP
;
MAZEPTR:
		LDA GENDER
		BEQ @pac		; go original
@ms:	LDA LEVEL,X		; determine alternative
		AND #$07
		LSR
		TAY
		INY
		TYA
@pac:	STA MAZE
		RTS

;*********************************************************************
; start a new level/maze
;
NEWMAZE:
		LDX PLAYERUP
		LDA DEMO
		BNE @skip
		INC LEVEL,X
		LDA LEVEL,X
		BEQ @skip
		AND #$03
		BNE @skip
		JSR INTERLUDE
@skip:	; in-game Software Sprite Stack init
		JSR SSSINIT
@create:
		LDA #%00001000	; float X
		LDY #$08
		JSR SSSCREATE	; make five sprites, disabled
		CPX #$04
		BNE @create
		;
		LDX PLAYERUP
		JSR MAZEPTR		; return 0-4
		ASL
		TAX				; x2
		ASL
		TAY				; x4
		LDA MAZEDATA,X
		STA VECTORBG
		LDA MAZEDATA+1,X
		STA VECTORBG+1
		LDX PLAYERUP
		LDA MAZECONFIG,Y
		STA MAZEVIC,X
		LDA MAZECONFIG+1,Y
		STA MAZEWALL,X
		LDA MAZECONFIG+2,Y
		STA MAZEDOT,X
		STA COLORCODE
		LDA DEMO
		BNE @nocfg
		LDA MAZECONFIG+3,Y
		STA DOTS,X
@nocfg:
		LDX #$00
		LDY #$01
		JSR SSSPLOT
@loopy:	LDY #$00
@loop:	LDA (VECTORBG),Y
		BEQ @nomore
		STA NEWDIR
		CMP #$80
		BCC @tile		; tile?
		LDX #SSSNULL
		AND #$40
		BEQ @mt
		INX
@mt:	LDA NEWDIR
		STX NEWDIR
		AND #$0F		; repeats
		TAX
		BNE @draw
@tile:	LDX #$01
		CMP #$40
		BCC @draw
		ORA #$80
		STA NEWDIR
@draw:
		LDA NEWDIR
		JSR SSSPRINT
		DEX
		BNE @draw
		INY
		BNE @loop
		INC VECTORBG+1
		BNE @loopy
@nomore:
		LDX #$00
		JSR SCORESTATUS	; display P1 score
		LDX PLAYERS
		JSR SCORESTATUS	; display P2 score (maybe)
		;
		LDX #$04
@loop2:	LDA QUIKMANCLR,X ; reset monsters starting colors
		STA SPRITECOL,X ; into their sprite color registers
		LDA #$70		; reset monsters as looking down @ quikman
		STA SPRITEIMGL,X
		LDA #$1C
		STA SPRITEIMGH,X
		DEX
		BPL @loop2
		;
DOLEVEL:
		LDX #$02
		JSR SCORESTATUS	; display hi score
		LDX PLAYERUP
		LDA MAZEWALL,X	; paint the maze
		TAX
		JSR MAZEBORDER
		LDX PLAYERUP
		LDA LIVES,X
		CMP #$02
		BCC DOFRUIT		; no more quikman icons, keep hi score up
		JSR DOLIVES
		;
DOFRUIT:
		LDX #$0B
		LDY #$17
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,0
		LDX PLAYERUP
		LDA #$13
		SEC
		SBC LEVEL,X
		BCC @l13
		CMP #$0D
		BCS @rj
@l13:	LDA #$0D
@rj:	TAX				; right-justify fruit
		LDY #$17
		JSR SSSPLOT
		LDX PLAYERUP
		LDA LEVEL,X
		TAY
@loop5:	TYA
		TAX
		LDA GENDER
		BEQ @qm
		LDA #$14		; random character
		PHA				; ++
		LDA #$03		; cyan
		CPX #$07		; are we at the last level (random)?
		BCS @rnd
		PLA				; --
		LDA FRUIT2,X	; fruit character
		BNE @msqm
@qm:	CPX #$0C		; are we at the last level (key)?
		BCC @nokey
		LDX #$0C		; only keys remain
@nokey:	LDA FRUIT,X		; fruit character
@msqm:	PHA				; ++
		SEC
		SBC #$15
		TAX
		LDA FRUITCLR,X	; get its color
@rnd:	STA COLORCODE
		PLA				; --
		JSR SSSPRINT
		CPY #$00		; did we paint the cherry yet?
		BEQ @fini		; if so, we're done
		DEY
		LDA #$13
		CMP CRSRCOL
		BCS @loop5
@fini:	RTS
		;
DOLIVES:
		PHA				;++
		LDX #$00
		LDY #$17
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte $F7,SSSNULL,0
		PLA				;--
		TAY
		DEY
@loop:	LDA #$13		; quikman icon
		JSR SSSPRINT
		DEY
		BNE @loop
		JSR SSSPRINTS	; erase any I:000000
		.byte SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,SSSNULL,0
		RTS

;*********************************************************************
; restore an old level/maze, saving the current maze in its place
;
SWAPMAZE:
		LDA PLAYERUP
		EOR #$01		; swap players
		STA PLAYERUP
		TAX
		LDA LIVES,X		; any lives remaining?
		BNE @cont
		LDA DOTS,X		; any dots remaining?
		BEQ @cont		; no, didn't play a turn yet
		LDA PLAYERUP
		EOR #$01		; this player is done, try other player
		STA PLAYERUP
		RTS
		;
@cont:	LDX PLAYERUP
		LDA MAZEDOT,X
		STA COLORCODE
		LDX #$15		; save from 2nd line
		LDY #>VICFRAME1
		STX VECTORBG
		STY VECTORBG+1
		LDX #<MAZESAVE
		LDY #>MAZESAVE
		STX VECTORFG
		STY VECTORFG+1
		LDX #$00
		LDY #$01
		JSR SSSPLOT
		LDX #$02		; top + bottom half
		LDY #$00
@loop:	LDA (VECTORFG),Y
		JSR SSSPRINT
		LDA (VECTORBG),Y
		STA (VECTORFG),Y
		INY
		CPY #$E7
		BNE @loop
		LDA VECTORBG
		CLC
		ADC #$E7
		BCC @cc1
		INC VECTORBG+1
@cc1:	STA VECTORBG
		LDA VECTORFG
		CLC
		ADC #$E7
		BCC @cc2
		INC VECTORFG+1
@cc2:	STA VECTORFG
		LDY #$00
		DEX
		BNE @loop
		;
		LDX PLAYERUP
		BEQ @fini
		LDA DOTS,X
		BNE @fini		; good maze restored
		PLA				;--
		PLA				; pop JSR
		JMP RESETGAME	; player #2's first turn
@fini:	JSR DOLEVEL
		RTS

;*********************************************************************
; recolor maze with some new paint in X
;
MAZEBORDER:
		LDA DEMO
		CMP #$01		; menu mode?
		BEQ @paint
		TXA
		ORA #$08
		STA VIC+$0F		; in-game change border color to match walls
@paint:	STX JOYVAL
		LDY #$01
@loopy:	LDX #$00
@loopx:	JSR SSSPEEK
		LDY CRSRCOL
		CMP #$22
		BNE @cont
		LDA #$E1		; powerpills are always WHITE
		STA (COLORLINE),Y
		BNE @next
@cont:	BCC @next
		LDA #$E0
		ORA JOYVAL	; paint walls
		STA (COLORLINE),Y
@next:	LDY CRSRROW
		INC CRSRCOL
		LDX CRSRCOL
		CPX PLAYCOLS
		BNE @loopx
		INC CRSRROW
		LDY CRSRROW
		CPY #$17
		BNE @loopy
		;
@door:	LDA #$EC		; paint door border-pink-purple
		STA PLAYCOLOR+$DB
		STA PLAYCOLOR+$DC
		STA PLAYCOLOR+$DD
		LDY #$00
		LDX PLAYCOLS
@dirty:	LDA #$00
		STA DIRTYLINE,Y
		TXA
		STA DIRTYLINE2,Y
		INY
		CPY PLAYROWS
		BNE @dirty
		LDY DEMO
		BNE @fini
		INY
		JSR SSSFLIP
@fini:	RTS

;*********************************************************************
; This section is dedicated to background processing, accomplished
; via the keyboard IRQ service, called 60-times per second (jiffy).
;
BACKGROUND:
		CLD
		LDX PPILLTIMER	; drain powerpill
		BEQ FLASH		; is there still power left?
		LDY DEMO
		BNE @ppx
		LDA VIC+$0B
		CPX #$20
		BCS @pp
		SBC #$02
		CPX #$10
		BCS @pp
		SBC #$02
@pp:	SBC #$02
		CMP #$E0
		BCS @ppv2
		LDA #$FA
@ppv2:	STA VIC+$0B
@ppx:	LDA JIFFYL
		AND #$07
		BNE FLASH
		LDA PPILLTIMER
		CMP #$1F		; yes ... but are they
		BCS DRAIN		; getting confidence back?
		AND #$03		; yes, let's warn quikman
		BNE DRAIN
		LDY #$04
@pp1:	LDA SPRITEIMGL,Y
		CMP #<GHOST6	; is monster fleeing?
		BNE @pp2
		LDA SPRITECOL,Y
		EOR #$07		; flash white / blue
		STA SPRITECOL,Y
@pp2:	DEY
		BNE @pp1
DRAIN:	DEC PPILLTIMER
		BNE FLASH
		LDY #$04
@loop:	LDA MONSTERCLR-1,Y
		STA SPRITECOL,Y	; restore all monsters to their default colors
		LDA SPRITEIMGL,Y
		CMP #$60		; is monster already going home?
		BEQ @next
		LDA #<GHOST5	; restore monster chasing image
		STA SPRITEIMGL,Y
@next:	DEY
		BNE @loop
		STY VIC+$0B
FLASH:	LDA FLASHPILL	; powerpill flash
		CMP #$1F
		BCC @skip1
		LDA PLAYERUP
		ASL
		ASL
		ASL				; x8
		ADC #$07
		TAY
		LDX #$06		; reset counter
		STX FLASHPILL
@loop1:	LDA $1D10,X		; custom graphic char
		EOR $8288,X		; rom graphic char
		STA $1D10,X		; redraw 8x8 char cell
		LDA DEMO
		BNE @nextx
		LDA PUPHI,Y
		EOR PUPROM,Y
		STA PUPHI,Y
		LDA PUPHI+$10,Y
		EOR PUPROM+$10,Y
		STA PUPHI+$10,Y
		DEY
@nextx:	DEX
		BPL @loop1
		TXA				; render monster feet
		EOR GHOST6+7	; custom graphic char
		STA GHOST6+7	; redraw caged monster
		LDA #$FE
		EOR GHOST5+7
		STA GHOST5+7	; redraw fleeing monster
		STA GHOST4+7	; looking left
		STA GHOST3+7	; looking up
		LDA #$7F		; render monster feet
		EOR GHOST2+7	; custom graphic char
		STA GHOST2+7	; looking right
		STA GHOST1+7	; looking down
@skip1:	INC FLASHPILL
		LDA DEMO
		BEQ @ai			; playing?
		LDA JIFFYL		; manufacture a moving quikman 'spirit'
		BEQ @ai
		LDA JIFFYM
		AND #$01
		BNE @ai
		INC DEMOQMAN
		LDA DEMOQMAN
		AND #$03
		ASL				; x2 for pair
		TAY				; for the monsters to 'chase'
		LDA CAGEDATA+$08,Y
		STA SPRITEX
		LDA CAGEDATA+$09,Y
		STA SPRITEY
		;
@ai:	LDX PLAYERUP
		LDA DOTS,X
		PHA				;++
		LDX #$00
		LDY #$00
@loop6:	LDA PENALTY,X
		BNE @next		; not aware while caged
		LDA SPRITEIMGL+1,X
		CMP #$60
		BNE @go			; small ghost going home?
		LDA #$60
		STA MONXKB,Y
		LDA #$58
		STA MONYKB,Y
		BNE @next
@go:	PLA				;--
		PHA				;++
		CMP #$06		; make them all "smart" with < 5-dots
		BCC @skip5
		LDA CAGEDATA,X
		BEQ @skip5		; is monster "smart"?  Red one is ...
		CMP JIFFYL		; no, so check as often as it waits
		BNE @next		; is its wait time equal to the jiffy clock?
@skip5:	LDA SPRITEX		; update this monster's awareness to where quikman is
		CMP #$10
		BCS @ok1
		LDA #$60
@ok1:	CMP #$B0
		BCC @ok2
		LDA #$60
@ok2:	STA MONXKB,Y
		LDA SPRITEY
		STA MONYKB,Y
@next:	INY
		INY
		INX
		CPX #$04
		BNE @loop6
		PLA				;--
		;
@x:		LDA SPECIAL
		BEQ @eat
		DEC VIC+$0E
		AND #$0C
		BEQ @ding
		LDA VIC+$0C
		BNE @ding
		LDA #$AF		; pink & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$EB
@ding:	STA VIC+$0C
		DEC SPECIAL
		BNE @music
		STA CHOMP
		;
@eat:	LDX CHOMP
		BEQ @music
		CPX #$2B
		BCC @quik
		LDA JIFFYL
		ROR
		BCC @music
@quik:	DEC CHOMP
		LDA VIC+$0E
		AND #$F0
		ORA #$09		; medium
		STA VIC+$0E		; auxiliary color & volume
		LDA SNDBITS,X	; load tone data
		STA VIC+$0C
		BNE @music
		STA CHOMP
		;
@music:	; music player
		LDY NOTES
		BEQ @sss
		DEC DELAY
		BNE @sss
		LDA VECTOR3
		PHA				;++
		LDA VECTOR3+1
		PHA				;++
		LDA SHEET
		STA VECTOR3
		LDA SHEET+1
		STA VECTOR3+1
		LDA (VECTOR3),Y	; get a note
		TAX				; tenor only w/o bass
		BNE @note
		STA VIC+$0A		; mute bass
		BEQ @v2
@note:	CMP #$80
		BCS @v2
		ORA #$80		; add bass channel
@v1:	STA VIC+$0A		; bass voice
		INY
		LDA (VECTOR3),Y	; get another note
@v2:	STX VIC+$0B		; tenor voice
@v3:	STA VIC+$0C		; alto voice
@len:	INY
		LDA (VECTOR3),Y	; get duration
		BNE @next2
		STA NOTES		; fini
		BEQ @rv3
@next2:	INY
		STY NOTES
		CMP #$04
		BCC @us
		LDY MACHINE
		CPY #$05
		BEQ @us
		AND #$FD		; drop bit 2 (1/25th-sec)
@us:	STA DELAY
@rv3:	PLA				;--
		STA VECTOR3+1
		PLA				;--
		STA VECTOR3
		;
@sss:	JMP SSSIRQ		; check for video flip ...

;*********************************************************************
; Pass A for number of jiffies to wait, while preserving X
;
PAUSE:	TAY
		PHA				;++
		TXA
		PHA				;++
		JSR SSSREFRESH
		JSR SSSFLIP		; redraw sprites
		PLA				;--
		TAX
		PLA				;--
		RTS

;*********************************************************************
; Show a player's score
; Send X as index
;
SCOREJMP:	.word P1, P2, HI
SCORESTR:	.word P1STR, P2STR, HISTR
SCORESTATUS:
		STX XCOPY
		TXA
		ASL
		TAY				; x2
		ADC XCOPY
		TAX				; x3
		LDA SCOREJMP,Y
		STA $0300
		LDA SCOREJMP+1,Y
		STA $0301
		LDA SCORESTR,Y
		STA VECTOR1
		LDA SCORESTR+1,Y
		STA VECTOR1+1
		LDY #$00
@loop:	LDA SCORE1,X
		LSR
		LSR
		LSR
		LSR
		ORA #$40
		STA (VECTOR1),Y
		LDA SCORE1,X
		AND #$0F
		ORA #$40
		INY
		STA (VECTOR1),Y
		INX
		INY
		CPY #$06		; render 6-digits
		BNE @loop
		JMP ($0300)
		;
P1:		LDY #$00
		LDX #$01
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$F3,$4A,$4C,$F1			; [cyan]1UP:[white]
P1STR:	.byte	$40,$40,$40,$40,$40,$40	; 000000
		.byte	$00
		RTS
		;
P2:		LDY #$00
		LDX #$0C
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$F7,$4B,$4D,$F1			; [yellow]P2:[white]
P2STR:	.byte	$40,$40,$40,$40,$40,$40	; 000000
		.byte	$00
		RTS
		;
HI:		LDY #$00
		LDX #$0C
		LDA PLAYERS
		BEQ @skip
		LDY #$17
		LDX #$01
@skip:	JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$F6,$4E,$4F,$F1			; [blue]HI:[white]
HISTR:	.byte	$40,$40,$40,$40,$40,$40	; 000000
		.byte	$00
		RTS

;*********************************************************************
; Update a player's score
; Send Y as digit index (0=tens; 1=hundreds)
; Send A as value
;
SCOREUPDATE:
		PHA				;++
		LDA PLAYERUP
		ASL
		ADC PLAYERUP	; x3
		TAX
		STX R0
		PLA				;--
		SED				; switch to roman numerals... ;)
		CPY #$01		; are we adding to C or x values?
		BCC @x
@C:		CLC				; do hundreds
		ADC SCORE1+1,X
		STA SCORE1+1,X
		BCC @cc
		BCS @X
@x:		ADC SCORE1+2,X	; do tens
		STA SCORE1+2,X
		BCC @cc
		LDA SCORE1+1,X
		CLC
		ADC #$01
		STA SCORE1+1,X
@_:		BCC @cc
@X:		LDA SCORE1,X	; do tens of thousands
		CLC
		ADC #$01
		STA SCORE1,X
		CMP #$01		; first ten thousand gets an extra life
		BNE @cc
		LDA #$61
		STA SPECIAL		; ring my bell
		CLD
		LDX PLAYERUP
		INC LIVES,X
		LDA LIVES,X
		JSR DOLIVES		; update player's quikman count
@cc:	CLD
		; check for new hi score
		LDX R0
		LDA SCORE1,X
		CMP HISCORE
		BCC @print
		BNE @woot
		LDA SCORE1+1,X
		CMP HISCORE+1
		BCC @print
		BNE @woot
		LDA SCORE1+2,X
		CMP HISCORE+2
		BMI @print
		;
@woot:	LDA SCORE1,X
		STA HISCORE
		LDA SCORE1+1,X
		STA HISCORE+1
		LDA SCORE1+2,X
		STA HISCORE+2
		LDX PLAYERS
		BEQ @hi
		LDX PLAYERUP
		LDA LIVES,X
		CMP #$02
		BCS @print		; don't distract the player
@hi:	LDX #$02		; show new hi score
		JSR SCORESTATUS
@print:	LDX PLAYERUP	; update player's score
		JSR SCORESTATUS
		RTS

;*********************************************************************
; init dot-munching sound
;
WAHKA:	LDY GENDER
		LDA @bit,Y
@c:		STA CHOMP
		RTS
@bit:	.byte $20,$2A

;*********************************************************************
; data of the read-only kind
;
		.segment "RODATA"

		; 8x8, 16x8, 8x16, 16x16
sssALLOC:
		.byte   8,16,16,32	; fixed:	1,2,2,4
		.byte   16,24,32,48	; float Y:	2,3,4,6
		.byte   16,32,24,48	; float X:	2,4,3,6
		.byte   32,48,48,72	; both:		4,6,6,9
sssROWS:
		.byte   1,2,1,2	; fixed
		.byte   2,3,2,3	; float Y
		.byte   1,2,1,2	; float X
		.byte   2,3,2,3	; both
sssCOLS:
		.byte   1,1,2,2 ; fixed
        .byte   1,1,2,2 ; float Y
        .byte   2,2,3,3 ; float X
		.byte   2,2,3,3 ; both
		;
		; original theme
INTRO:	.byte	0
		.byte	59, 221, 7	; b1 & B4
		.byte	    238, 5	; B5
		.byte	93, 232, 7	; b2 & F#5
		.byte	    228, 5	; D#5
		.byte	59, 238, 7	; b1 & B5
		.byte	    232, 5	; F#5
		.byte	93, 228, 11	; b2 & D#5
		.byte	      0, 1	; mute
		;
		.byte	63, 223, 7	; c2 & C5
		.byte	    239, 5	; C6
		.byte	95, 234, 7	; c3 & G5
		.byte	    230, 5	; E5
		.byte	63, 239, 7	; c2 & C6
		.byte	    234, 5	; G5
		.byte	95, 230, 11	; c3 & E5
		.byte	      0, 1	; mute
		;
		.byte	59, 221, 7	; b1 & B4
		.byte	    238, 5	; B5
		.byte	93, 232, 7	; b2 & F#5
		.byte	    228, 5	; D#5
		.byte	59, 238, 7	; b1 & B5
		.byte	    232, 5	; F#5
		.byte	93, 228, 11	; b2 & D#5
		.byte	      0, 1	; mute
		;
		.byte	82, 228, 5	; f#2 & D#5
		.byte	    230, 3	; E5
		.byte	    231, 3	; F5
		.byte	      0, 1	; mute
		.byte	87, 231, 5	; g#2 & F5
		.byte	    232, 3	; F#5
		.byte	    234, 3	; G5
		.byte	      0, 1	; mute
		.byte	91, 234, 5	; a#2 & G5
		.byte	    235, 3	; G#5
		.byte	    236, 3	; A5
		.byte	      0, 1	; mute
		.byte	93, 236, 3	; b2 & A5
		.byte	    237, 3	; A#5
		.byte	    238, 5	; B5
		.byte	      0, 0	; mute & fini
		;
		; alternate theme
INTRO2:	.byte	0			; start of sheet music
		.byte	191-128, 230, 7	; c2 & E4
		.byte	      0, 1		; mute
		.byte	    230, 7		; E4
		.byte	      0, 1		; mute
		.byte	170-128, 223, 11; g1 & C3
		.byte	      0, 1		; mute
		.byte	    230, 11		; E4
		.byte	      0, 1		; mute
		;
		.byte	187-128, 226, 11; b1 & D4
		.byte	      0, 1		; mute
		.byte	    231, 11		; F4
		.byte	      0, 1		; mute
		.byte	191-128, 230, 7	; c2 & E4
		.byte	      0, 1		; mute
		.byte	    234, 7		; G4
		.byte	      0, 1		; mute
		;
		.byte	    231, 7		; F4
		.byte	      0, 1		; mute
		.byte	191-128, 230, 7	; c2 & E4
		.byte	      0, 1		; mute
		.byte	    226, 11		; D4
		.byte	      0, 1		; mute
		.byte	187-128, 231, 11; b2 & F4
		.byte	      0, 1		; mute
		;
		.byte	191-128, 230, 7	; c2 & E4
		.byte	      0, 1		; mute
		.byte	    234, 7		; G4
		.byte	      0, 1		; mute
		.byte	    234, 7		; G4
		.byte	      0, 1		; mute
		.byte	191-128, 230, 7	; c2 & E4
		.byte	      0, 1		; mute
		;
		.byte	    226, 5		; D4
		.byte	191-128, 230, 5	; c2 & E4
		.byte	    231, 5		; F4
		.byte	    234, 5		; G4
		.byte	    236, 5		; A4
		.byte	    238, 5		; B4
		.byte	      0, 1		; mute
		;
		.byte	204-128, 239, 11; e2 & C4
		.byte	      0, 1		; mute
		.byte	198-128, 238, 11; d2 & B4
		.byte	      0, 1		; mute
		.byte	191-128, 239, 14; c2 & C4
		.byte	      0, 0		; mute & fini
		;
INTERMISSION:
		.byte	0			; start of sheet music
		.byte	204-128, 223, 11; e2 & C3
		.byte	       0, 1 	; mute
		.byte	     223, 7 	; C3
		.byte	       0, 1 	; mute
		.byte	     223, 7 	; C3
		.byte	       0, 1 	; mute
		.byte	204-128, 223, 7 ; e2 & C3
		.byte	       0, 1 	; mute
		.byte	     217, 11	; A2
		.byte	       0, 1 	; mute
		.byte	     223, 7 	; C3
		.byte	       0, 1 	; mute
		.byte	     223, 7 	; C3
		.byte	       0, 1 	; mute
		.byte	170-128, 230, 11; g1 & E4
		.byte	     231, 7		; tail
		.byte	       0, 6		; mute
		;
		.byte	204-128, 223, 11; e2 & C3
		.byte	       0, 1 	; mute
		.byte	     223, 7 	; C3
		.byte	       0, 1 	; mute
		.byte	     223, 7 	; C3
		.byte	       0, 1 	; mute
		.byte	204-128, 223, 7 ; e2 & C3
		.byte	       0, 1 	; mute
		.byte	     217, 11	; A2
		.byte	       0, 1 	; mute
		.byte	     223, 7		; C3
		.byte	       0, 1		; mute
		.byte	     223, 7		; C3
		.byte	       0, 1 	; mute
		.byte	255-128, 212, 11; c0 & G2
		.byte	     211, 7		; tail
		.byte	       0, 6		; mute
		;
		.byte	204-128, 223, 11; e2 & C3
		.byte	       0, 1		; mute
		.byte	     223, 7		; C3
		.byte	       0, 1		; mute
		.byte	     223, 7		; C3
		.byte	       0, 1		; mute
		.byte	204-128, 223, 7 ; e2 & C3
		.byte	       0, 1		; mute
		.byte	     217, 11	; A2
		.byte	       0, 1		; mute
		.byte	     223, 7		; C3
		.byte	       0, 1		; mute
		.byte	     226, 7		; D3
		.byte	       0, 1		; mute
		.byte	191-128, 230, 7 ; c1 & E3
		.byte	       0, 1		; mute
		.byte	     231, 7		; F3
		.byte	       0, 1		; mute
		.byte	223-128, 234, 7 ; c2 & G4
		.byte	       0, 1		; mute
		.byte	     231, 7		; F3
		.byte	       0, 1		; mute
		.byte	     230, 7		; E3
		.byte	       0, 1		; mute
		.byte	     226, 7		; D3
		.byte	       0, 1		; mute
		.byte	170-128, 230, 11; E3
		.byte	       0, 1		; mute
		.byte	204-128, 223, 11; C3
SNDBITS:
		.byte	       0, 0		; mute & fini
		; yummy sound effect
		.byte	$A8,$B8, $A0,$90, $88,$84, $80,$82, $84,$86, $8A,$92, $98,$A4, $B0,$C8
		; wahka sound effects
		.byte	0,$80,$90,$A0,$B0,$C0,$D0,$E0,$F0,$E1,$D1,$C1,$B1,$A1,$91
		.byte	0,$FC,$F8,$F4,$F0,$EC,$E8,$F9,$F1,$E9
		; death sequences
		.byte	0
		.byte	$B8,$B0,$A8,$A0,$98,$90,$88,$80
		.byte	$D0,$C0,$B0,$A0,$98,$90,$88,$80
		.byte	$A0,$A4,$A8,$AC,$B0,$B4,$B8,$BC
		.byte	$B0,$B4,$B8,$BC,$C0,$C4,$C8,$CC
		.byte	$C0,$C4,$C8,$CC,$D0,$D4,$D8,$DC
		.byte	$D0,$D4,$D8,$DC,$E0,$E4,$E8,$EC
		;
FRUIT2:	; cherry, strawberry, peach, apple, pretzel, pear, banana, ?
		.byte	$18, $19, $1A, $1B, $15, $16, $17, $14
PEEKAHEAD:
		.byte	$01, $15, $14, $00

