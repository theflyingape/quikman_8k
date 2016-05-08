;*********************************************************************
; COMMODORE VIC 20 BOOT USING BASIC 2.0
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 07-Sep-2014
;
		.fileopt author,	"Robert Hurst"
        .fileopt comment,	"Quikman+"
        .fileopt compiler,	"VIC 20 ASSEMBLER"

		.include "VIC-SSS-MMX.h"
		.include "mazedata.h"

		; externally defined zero page symbols
		.global CHOMP
		.global FRAME
		.global GENDER
		.global MENU
		.global NOTES
		.global PLAYERS
		.global SPECIAL

;*********************************************************************
; Commodore BASIC 2.0 program
;
; LOAD "QUIKMAN+8K.PRG",8
; RUN
;
		.segment "BASIC"

		.word	RUN		; load address
RUN:	.word	@end	; next line link
		.word	2014	; line number
		.byte	$9E		; BASIC token: SYS
		.byte	<(MAIN / 1000 .mod 10) + $30
		.byte	<(MAIN / 100 .mod 10) + $30
		.byte	<(MAIN / 10 .mod 10) + $30
		.byte	<(MAIN / 1 .mod 10) + $30
		.byte	0		; end of line
@end:	.word	0		; end of program

;*********************************************************************
; Starting entry point for this program
;
		.segment "STARTUP"

MAIN:
		LDA MACHINE
		CMP #$05
		BNE PAL
		;
		; NTSC setup
NTSC:	LDX #<@NTSC		; load the timer low-byte latches
		LDY #>@NTSC
		LDA #$70		; top of last raster row
		BNE IRQSYNC
@NTSC = $4243			; (261 * 65 - 2)
		;
		; PAL setup
PAL:	LDX #<@PAL		; load the timer low-byte latches
		LDY #>@PAL
		LDA #$76		; raster line 228/229
@PAL = $5686			; (312 * 71 - 2)
		;
IRQSYNC:
		CMP VIC+$04
		BNE IRQSYNC
		STX $9126		; load T1 latch low
		STY $9125		; load T1 latch high, and transfer both to T1 counter
		; init VIC
		INC VIC			; adjust left border to accommodate one less column
		LDA VIC+$01
		SEC
		SBC #$04		; adjust top scan line to accomodate extra row
		STA VIC+$01
		LDA #$00+$15	; set for videoram @ $1400 with 21-columns
		STA VIC+$02		; video matrix address + columns
		LDA #$B0		; $B0 = 10110000 = 24 rows + 8x8 height
		STA VIC+$03		; rows / character height
		LDA #$DF		; set video @ $1400 and char table @ $1C00
		STA VIC+$05
		LDA #$8B		; brown & moderate
		STA VIC+$0E		; auxiliary color & volume
		LDA #$80
		STA SHIFTMODE	; locked
		;
@hi:	LDA #$FE
		STA MEGACART	; init MC memory register
		STA MEGACART	; init MC memory register a 2nd time
		CMP MEGACART	; detect if this memory location was really writable
		BNE SPLASH		; no, might be an emulator or a real VIC with only 8k
		LDX NVRAM+2		; load saved high score
		LDY NVRAM+1
		TXA
		AND #$0F
		BNE SPLASH		; no one's allowed -- reset hi-score
		LDA NVRAM
		CMP #$02
		BCC SPLASH		; less than 20,000 points?
		STA HISCORE
		STY HISCORE+1
		STX HISCORE+2
		;
SPLASH:	; my interrupt vector init
		SEI
		LDX #<BACKGROUND
		LDY #>BACKGROUND
		STX $0314
		STY $0315
		CLI

		LDA #$FE		; lt yellow screen / blue border
		STA VIC+$0F
		LDX #<SPLASHCOLOR
		LDY #>SPLASHCOLOR
		STX VECTORFG
		STY VECTORFG+1
		LDY #$94
		STX VECTORBG
		STY VECTORBG+1
		LDX #$02
		LDY #$00
		STY SPECIAL
@fill:	LDA (VECTORFG),Y
		STA (VECTORBG),Y
		INY
		BNE @fill
		INC VECTORFG+1
		INC VECTORBG+1
		DEX
		BNE @fill
		STX PLAYERS
		LDY #$94
		STY VECTORBG+1
		;
		LDX #$50
		STX DELAY		; init music player
		;
@loop:	LDA JIFFYL
@wait:	CMP JIFFYL
		BEQ @wait
		TAX
		AND #$03
		BNE @loop		; update every 4th jiffy
		TXA
		AND #$0F		; use remainder as x4 (2x2 character matrix)
		CMP #$0C
		BNE @qanim
		LDA #$04		; repeat partial
@qanim:	CLC
		ADC #$50		; add big quikman base character code
		TAX
		STX $1457
		INX
		STX $146C
		INX
		STX $1458
		INX
		STX $146D
		;
		LDX MARQUEE
		LDA BULBS,X
		TAY
		LDA #$02		; RED
		STA (VECTORBG),Y
		INX
		CPX #$30
		BNE @l0
		LDX #$00
@l0:	STX MARQUEE
		LDA BULBS,X
		TAY
		LDA #$04		; MAGENTA
		STA (VECTORBG),Y
		;
		LDX MARQUEE+1
		LDA BULBS,X
		TAY
		LDA #$06		; BLUE
		STA (VECTORBG),Y
		INX
		CPX #$30
		BNE @l1
		LDX #$00
@l1:	STX MARQUEE+1
		LDA BULBS,X
		TAY
		LDA #$03		; CYAN
		STA (VECTORBG),Y
		;
		LDX MARQUEE+2
		LDA BULBS,X
		TAY
		LDA #$05		; GREEN
		STA (VECTORBG),Y
		INX
		CPX #$30
		BNE @l2
		LDX #$00
@l2:	STX MARQUEE+2
		LDA BULBS,X
		TAY
		LDA #$07		; YELLOW
		STA (VECTORBG),Y
		;
@fruits:
		LDA NOTES
		BNE @key		; playing music?
		LDA MARQUEE
		AND #$0F
		BNE @key		; looped?
		LDA FDEMO
		CLC
		ADC #$18
		TAY				; fruit char code
		ASL				; x2
		EOR #$FF		; x-1
		TAX				; fruit col
		INC FRAME
		LDA FRAME
		ROR
		BCS @ds
		; restore fruit
		LDA #SSSNULL
		STA $14BB,X
		STA $14BD,X
		TYA
		STA $14BC,X
		LDY FDEMO
		LDA FRUITCLR+3,Y
		STA $94BC,X
		INC FDEMO
		LDY FDEMO
		CPY #$08
		BCC @key
		LDY #$00		; restart
		STY FDEMO
		INC NOTES
		BNE @key
@ds:	; display fruit score
		TYA
		SBC #$18
		STA VECTOR3
		ASL
		ADC VECTOR3		; x3
		TAY
		LDA #$00
		STA $94BC,X
		LDA FRUITVAL+9,Y
		STA $14BB,X
		LDA FRUITVAL+10,Y
		STA $14BC,X
		LDA FRUITVAL+11,Y
		STA $14BD,X
		LDA #$11
		STA CHOMP		; YUMMY!
		;
@key:	JSR GETIN
		CMP #$85		; got F1 ?
		BEQ @go
		LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; got FIRE ?
		BEQ @go
		JMP @loop
@go:
		LDX #SNDBITS-INTERMISSION
		STX NOTES
		JMP MENU
		;
MARQUEE:
		.byte	$00,$10,$20
BULBS:	.byte	1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
		.byte	41,62,83,104,125
		.byte	145,144,143,142,141,140,139,138,137,136,135,134,133,132,131,130,129,128,127
		.byte	105,84,63,42,21
FDEMO:	.byte	$00
QMAN:   .byte   $3C, $7E, $BD, $FF, $BD, $C3, $7E, $3C  ; evil otto
		.segment "SPLASH"
SPLASHDATA:
		.byte	$22,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$22
		.byte	$7E,$35,$38,$36,$2F,$20,$2F,$0C,$2F,$2F,$20,$20,$20,$20,$20,$20,$20,$20,$6C,$6E,$7E
		.byte	$7E,$37,$20,$37,$37,$20,$37,$2F,$39,$3C,$36,$20,$17,$20,$16,$20,$15,$20,$6D,$6F,$7E
		.byte	$7E,$33,$3B,$34,$33,$38,$34,$30,$30,$20,$30,$35,$3B,$36,$35,$38,$36,$35,$38,$36,$7E
		.byte	$7E,$20,$30,$50,$52,$7B,$7B,$7B,$20,$68,$6A,$37,$30,$37,$39,$38,$3A,$37,$20,$37,$7E
		.byte	$7E,$20,$20,$51,$53,$7C,$7C,$7C,$20,$69,$6B,$30,$20,$30,$30,$20,$30,$30,$20,$30,$7E
		.byte	$22,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$7D,$22
		.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
		.byte	$71,$77,$77,$77,$77,$79,$86,$85,$81,$94,$95,$92,$89,$8E,$87,$7A,$77,$77,$77,$77,$72
		.byte	$75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$0D,$20,$20,$20,$20,$81,$82,$92,$81,$88,$81,$8D,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$0D,$20,$20,$20,$20,$92,$85,$82,$85,$83,$83,$81,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$0D,$20,$20,$20,$20,$89,$93,$81,$82,$85,$8C,$8C,$81,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$0D,$20,$20,$20,$20,$90,$85,$81,$8E,$95,$7F,$20,$20,$20,$20,$20,$76
		.byte	$75,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$12,$20,$1F,$20,$1E,$20,$1D,$20,$1C,$20,$1B,$20,$1A,$20,$19,$20,$18,$20,$13,$76
		.byte	$75,$20,$20,$20,$20,$20,$20,$EC,$E2,$E2,$FB,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$20,$90,$92,$85,$93,$93,$60,$61,$63,$E1,$86,$8F,$92,$20,$8D,$85,$8E,$95,$67,$76
		.byte	$75,$20,$20,$20,$20,$20,$20,$FC,$62,$62,$FE,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76
		.byte	$75,$70,$42,$40,$41,$44,$20,$5C,$5D,$8F,$82,$85,$92,$7F,$5E,$5F,$95,$92,$93,$7F,$76
		.byte	$73,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$78,$74
INERTIA:		; maintain direction
		.byte	$01, $00, $00, $01, $FF, $00, $00, $FF
SPLASHCOLOR:
		.byte	$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01
		.byte	$07,$00,$00,$00,$00,$00,$00,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$09,$09,$07
		.byte	$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$00,$05,$00,$02,$00,$09,$09,$07
		.byte	$07,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07
		.byte	$07,$00,$00,$07,$07,$01,$01,$01,$00,$0A,$0A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07
		.byte	$07,$00,$00,$07,$07,$01,$01,$01,$00,$0A,$0A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07
		.byte	$01,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$0F,$0F,$0F,$0F,$0F,$0F,$04,$04,$04,$04,$04,$04,$04,$04,$04,$0F,$0F,$0F,$0F,$0F,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$02,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06,$06,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$05,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06,$06,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$03,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06,$06,$06,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$07,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$06,$00,$03,$00,$07,$00,$04,$00,$05,$00,$02,$00,$07,$00,$02,$00,$02,$00,$07,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$09,$09,$09,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$09,$09,$09,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$00,$00,$00,$00,$00,$00,$09,$09,$09,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F
		.byte	$0F,$04,$06,$06,$06,$06,$00,$04,$04,$06,$06,$06,$06,$06,$04,$04,$06,$06,$06,$06,$0F
		.byte	$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
		; regulator table (bit=1 means wait for next vsync)
SPEED:		.byte	%11101110	; NTSC: cherry,		PAL: n/a
			.byte	%11010110	; NTSC: strawberry,	PAL: cherry
			.byte	%01010101	; NTSC: peach #1,	PAL: strawberry
			.byte	%00101001	; NTSC: peach #2,	PAL: peach #1
			.byte	%00010001	; NTSC: apple #1,	PAL: peach #2
			.byte	%00010000	; NTSC: n/a,		PAL: apple #1
			.byte	%00000000	; full ahead!
			.byte	%00000000	; 

;*********************************************************************
; VIC Software Sprite Stack (SSS)
;
; The above BASIC loader will be overwritten by SSS upon its
; initialization (SSSINIT).  The linker will fill this reserved space
; for the dual video frame buffers, play field, and the sprite image buffers:
; 4096 - 6406 ($1000 - $1906)
;
; $1000 - $11FF		VICFRAME1 - first video buffer
; $1200 - $13FF		VICFRAME2 - second video buffer
; $1400 - $15FF		PLAYFIELD - write-pending screen buffer
; $1600 - $17FF		PLAYCOLOR - write-pending color / dirty buffer
;
		.segment "SSSBUF"
                            ; small enough to re-use 6502 system stack
SSSBUF		= $0100			; uses only 10 double-buffered chars: $6C-$7F
;
; SPRITE REGISTERS (assignments into the tape buffer)
;
SPRITEBUFH	= DATASETTE
SPRITEBUFL	= SPRITEBUFH + SPRITEMAX
SPRITEC1H	= SPRITEBUFL + SPRITEMAX
SPRITEC1L	= SPRITEC1H + SPRITEMAX
SPRITEC2H	= SPRITEC1L + SPRITEMAX
SPRITEC2L	= SPRITEC2H + SPRITEMAX
SPRITECOL	= SPRITEC2L + SPRITEMAX
SPRITEDEF	= SPRITECOL + SPRITEMAX
SPRITEH		= SPRITEDEF + SPRITEMAX
SPRITEIMGH	= SPRITEH + SPRITEMAX
SPRITEIMGL	= SPRITEIMGH + SPRITEMAX
SPRITEX		= SPRITEIMGL + SPRITEMAX
SPRITEY		= SPRITEX + SPRITEMAX
SPRITEZ		= SPRITEY + SPRITEMAX
sss			= SPRITEZ + SPRITEMAX	; screen row index 24*2


;*********************************************************************
; other initialized data can be appended here:
;
			.segment "MYDATA"

		;our HERO
QANIM:	;closed
        .byte   %00111100
        .byte   %01111110
        .byte   %11111111
        .byte   %11111111
        .byte   %11111111
        .byte   %11111111
        .byte   %01111110
        .byte   %00111100
		;right 1
        .byte   %00111100
        .byte   %01111110 
        .byte   %11111000
        .byte   %11100000
        .byte   %11100000
        .byte   %11111000
        .byte   %01111110
        .byte   %00111100
		;right 2
        .byte   %00111100
        .byte   %01111000
        .byte   %11110000
        .byte   %11100000
        .byte   %11100000
        .byte   %11110000
        .byte   %01111000
        .byte   %00111100
		;down 1
        .byte   %00111100
        .byte   %01111110
        .byte   %11111111
        .byte   %11100111
        .byte   %11100111
        .byte   %11000011
        .byte   %01000010
        .byte   %00000000
		;down 2
        .byte   %00111100
        .byte   %01111110
        .byte   %11111111
        .byte   %11100111
        .byte   %11000011
        .byte   %10000001
        .byte   %00000000
        .byte   %00000000
		;left 1
        .byte   %00111100
        .byte   %01111110
        .byte   %00011111
        .byte   %00000111
        .byte   %00000111
        .byte   %00011111
        .byte   %01111110
        .byte   %00111100
		;left 2
        .byte   %00111100
        .byte   %00011110
        .byte   %00001111
        .byte   %00000111
        .byte   %00000111
        .byte   %00001111
        .byte   %00011110
        .byte   %00111100
		;up 1
        .byte   %00000000
        .byte   %01000010
        .byte   %11000011
        .byte   %11100111
        .byte   %11100111
        .byte   %11111111
        .byte   %01111110
        .byte   %00111100
		;up 2
        .byte   %00000000
        .byte   %00000000
        .byte   %10000001
        .byte   %11000011
        .byte   %11100111
        .byte   %11111111
        .byte   %01111110
        .byte   %00111100

SCORE1:		.byte	0,0,0	; player1
SCORE2:		.byte	0,0,0	; player2
HISCORE:	.byte	2,0,0	; score to beat
LEVEL:		.byte	0,0		; player1&2
STARTING:	.byte	0		; starting level
LIVES:		.byte	0,0,0	; player1&2, demo
DOTS:		.byte	0,0		; player1&2 dots remaining
CAGEDATA:		; knowledge cycle time
		.byte	$00, $33, $76, $F9
				; right, up, left, right
		.byte	$00, $03, $02, $00
				; coordinate to consider going to upon release or demo
		.byte	$A8, $30, $18, $30, $A8, $98, $18, $98
STARTPOSX:
		.byte	$60, $60, $60, $70, $50
STARTPOSY:
		.byte	$98, $58, $68, $68, $68
;
QUIKMANCLR:		; yellow
		.byte	$07
MONSTERCLR:		; red, green, cyan, yellow
		.byte	$02, $05, $03, $07
FRUIT:			; cherry, strawberry, 2-peach, 2-apple, 2-pineapple, 2-tbird, 2-bell, key
		.byte	$18, $19, $1A, $1A, $1B, $1B, $1C, $1C, $1D, $1D, $1E, $1E, $1F
FRUITCLR:		; red, green yellow, red, red, yellow, red, green, magenta, yellow, cyan
		.byte	$02, $05, $07, $02, $02, $07, $02, $05, $04, $07, $03
FRUITSCORE:
		.byte	$10, $20, $50, $01, $03, $05, $07, $10, $20, $30, $50
FRUITVAL:		; on-screen scoring table
		.byte	$08, $07, $06	; 1000
		.byte	$09, $07, $06	; 2000
		.byte	$0B, $07, $06	; 5000
		.byte	$02, $01, $00	;  100
		.byte	$03, $01, $00	;  300
		.byte	$04, $01, $00	;  500
		.byte	$05, $01, $00	;  700
		.byte	$08, $07, $06	; 1000
		.byte	$09, $07, $06	; 2000
		.byte	$0A, $07, $06	; 3000
		.byte	$0B, $07, $06	; 5000
		;
MAZECONFIG:
		.byte	$0E,$06,$01,$AA	; black/blue, blue, white, 170
		.byte	$0D,$05,$01,$9D	; black/green, green, white, 157
		.byte	$0B,$03,$07,$A7	; black/red, red, yellow, 167
		.byte	$0A,$02,$07,$AB	; black/cyan, cyan, yellow, 171
		.byte	$0C,$04,$01,$A3	; black/magenta, magenta, white, 163
MAZEDATA:	.word	MAZEDATA0, MAZEDATA1, MAZEDATA2, MAZEDATA3, MAZEDATA4
MAZEVIC:	.byte	$00, $00, $00	; color of VIC screen/border
MAZEWALL:	.byte	$00, $00, $00	; color of maze walls
MAZEDOT:	.byte	$00, $00, $00	; color of maze dots
MAZESAVE:	.res	$15 * $16
		;
PUPROM:
@1:		.byte	%00000000
		.byte	%01100100
		.byte	%11100100
		.byte	%01100100
		.byte	%01100100
		.byte	%01100100
		.byte	%01100100
		.byte	%11110011
		;
@2:		.byte	%00000000
		.byte	%01100100
		.byte	%10110100
		.byte	%00110100
		.byte	%01100100
		.byte	%11000100
		.byte	%11000100
		.byte	%11110011
		;
@u:		.byte	%00000000
		.byte	%10111000
		.byte	%10100100
		.byte	%10100101
		.byte	%10111000
		.byte	%10100001
		.byte	%10100000
		.byte	%00100000
		;
@p:		.byte	%00000000
		.byte	%10111000
		.byte	%10100100
		.byte	%10100101
		.byte	%10111000
		.byte	%10100001
		.byte	%10100000
		.byte	%00100000
		;
		; red-orange-yellow-green-cyan-blue-magenta,black
BGCOLOR: .byte  40,168,136,152,120,248,88,216,56,184,104,232,72,200,8
		;
SHEET:		.word INTERMISSION
NOTES:		.byte 1
DELAY:		.byte 0

;*********************************************************************
; a deserved break between every 4th level
;
INTERLUDE:
		JSR SSSINIT
		JSR DOFRUIT
		;
		LDY #$10
		LDA #%10001011	; float X, double-sized
		JSR SSSCREATE	; reserve Quikman
		LDA #$07
		STA SPRITECOL
		LDY #13*8+4
		LDX #23*8
		STY SPRITEY
		STX SPRITEX
		LDY #$08
		LDA #%10001100	; float X/Y
		STY SPRITEH
		STA SPRITEDEF	; reset Quikman starting configuration
		JSR SSSCREATE	; reserve Abraham
		LDA #$02
		LDY #>GHOST3	; Abraham pursuing left
		LDX #<GHOST3
		STA SPRITECOL+1
		STY SPRITEIMGH+1
		STX SPRITEIMGL+1
		LDY #13*8+4
		LDX #$08
		STY SPRITEY+1
		STX SPRITEX+1
		;
		LDX #<INTERMISSION
		LDY #>INTERMISSION
		STX SHEET
		STY SHEET+1
		LDA MACHINE
		ASL
		STA DELAY
		LDX #$01
		STX NOTES
@pursue:
		LDY #>QANIM
		STY SPRITEIMGH
		LDX #<QANIM     ; closed mouth
		LDA FRAME
		LSR
		AND #$03        ; 00=close, 01=partial, 10=open, 11=partial
		TAY
		BEQ @anim
		LDA #$20
		CPY #$02        ; open?
		BNE @x2
		ORA #8
@x2:	CLC
		ADC #8          ; add base offset
@x:		TAX
@anim:	STX SPRITEIMGL
		JSR SSSREFRESH
		LDY #$01
		JSR SSSFLIP
		DEC SPRITEX+1
		INC FRAME
		LDA FRAME
		AND #$03
		BEQ @pursue
		DEC SPRITEX
		BNE @pursue
		;
		LDA #$06
		STA SPRITECOL+1
		LDY #>GHOST6	; Abraham being chased right
		LDX #<GHOST6
		STY SPRITEIMGH+1
		STX SPRITEIMGL+1
@wait:
		JSR SSSREFRESH
		LDY #$02
		JSR SSSFLIP
		DEC SPRITEX
		INC SPRITEX+1
		LDA SPRITEX+1
		CMP #6*8
		BCC @wait
		;
		LDA #%10001011	; float X only, sized 16x16
		STA SPRITEDEF
		LDY #$10
		STY SPRITEH
		LDY #>QUIKMAN
		LDX #<QUIKMAN
		STY SPRITEIMGH
		STX SPRITEIMGL
		LDY #13*8
		STY SPRITEY
		LDA MACHINE
		EOR #$0D
		STA DELAY
		ASL
		LDX #$01
		STX NOTES
@chase:
		JSR SSSREFRESH
		LDY #$01
		JSR SSSFLIP
		INC FRAME
		LDA FRAME
		AND #$07
		LSR
		BCS @cont
		CMP #$03
		BCC @qanim
		LDA #$01
@qanim:	ASL
		ASL
		ASL
		ASL
		ASL				; X32
		CLC
		ADC #<QUIKMAN
		STA SPRITEIMGL
@cont:	INC SPRITEX
		LDA FRAME
		AND #$03
		BEQ @chase
		INC SPRITEX+1
		BNE @chase
@fini:	RTS

;*********************************************************************
; Re-render player 1&2UP tiles
;
PUP:	LDX #$1F
@pup:	LDA PUPROM,X
		STA PUPHI,X
		DEX
		BPL @pup
		RTS

;*********************************************************************
; VIC Custom Graphic Characters
;
		.segment "MYCHAR"

		; on-screen scoring tiles
		; 0-11
FS00:	.byte	%00000000
		.byte	%10000000
		.byte	%01000000
		.byte	%01000000
		.byte	%01000000
		.byte	%01000000
		.byte	%10000000
		.byte	%00000000
		;
		.byte	%00000000
		.byte	%00110001
		.byte	%01001010
		.byte	%01001010
		.byte	%01001010
		.byte	%01001010
		.byte	%00110001
		.byte	%00000000
		;
FS100:	.byte	%00000000
		.byte	%00000010
		.byte	%00000110
		.byte	%00000010
		.byte	%00000010
		.byte	%00000010
		.byte	%00000111
		.byte	%00000000
		;
FS300:	.byte	%00000000
		.byte	%00001111
		.byte	%00000001
		.byte	%00000110
		.byte	%00000001
		.byte	%00001001
		.byte	%00000110
		.byte	%00000000
		;
FS500:	.byte	%00000000
		.byte	%00001111
		.byte	%00001000
		.byte	%00001110
		.byte	%00000001
		.byte	%00001001
		.byte	%00000110
		.byte	%00000000
		;
FS700:	.byte	%00000000
		.byte	%00001111
		.byte	%00000001
		.byte	%00000010
		.byte	%00000100
		.byte	%00000100
		.byte	%00000100
		.byte	%00000000
		;
FS000:	.byte	%00000000
		.byte	%00011000
		.byte	%10100100
		.byte	%10100100
		.byte	%10100100
		.byte	%10100100
		.byte	%00011000
		.byte	%00000000
		;
		.byte	%00000000
		.byte	%01100011
		.byte	%10010100
		.byte	%10010100
		.byte	%10010100
		.byte	%10010100
		.byte	%01100011
		.byte	%00000000
		;
FS1000:	.byte	%00000000
		.byte	%00000100
		.byte	%00001100
		.byte	%00000100
		.byte	%00000100
		.byte	%00000100
		.byte	%00001110
		.byte	%00000000
		;
FS2000:	.byte	%00000000
		.byte	%00001100
		.byte	%00010010
		.byte	%00000010
		.byte	%00001100
		.byte	%00010000
		.byte	%00011110
		.byte	%00000000
		;
FS3000:	.byte	%00000000
		.byte	%00011110
		.byte	%00000010
		.byte	%00001100
		.byte	%00000010
		.byte	%00010010
		.byte	%00001100
		.byte	%00000000
		;
FS5000:	.byte	%00000000
		.byte	%00011110
		.byte	%00010000
		.byte	%00011100
		.byte	%00000010
		.byte	%00010010
		.byte	%00001100
		.byte	%00000000
		;
		; 12-23
GHOST0:	.byte	%00000000	; $60 for your eyes only
		.byte	%01000010
		.byte	%10100101
		.byte	%10100101
		.byte	%01000010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
GHOST1:	.byte	$1C, $3E, $7F, $64, $7F, $7F, $7F, $55	; $68 right
GHOST2:	.byte	$1C, $3E, $7F, $7F, $49, $7F, $7F, $55	; $70 down
GHOST3:	.byte	$38, $7C, $FE, $26, $FE, $FE, $FE, $AA	; $78 left
GHOST4:	.byte	$38, $7C, $FE, $FE, $FE, $FE, $FE, $AA	; $80 up
GHOST5:	.byte	$38, $7C, $FE, $92, $FE, $82, $FE, $AA	; $88 caged
GHOST6:	.byte	$3C, $7E, $FF, $99, $FF, $81, $FF, $55	; $90 fleeing
		; life icon
		.byte	%00111100
		.byte	%01111110
		.byte	%00011111
		.byte	%00000111
		.byte	%00011111
		.byte	%01111110
		.byte	%00111100
		.byte	%00000000
		; random
		.byte	%01111100
		.byte	%11000110
		.byte	%01000110
		.byte	%00001100
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00011000
		; 21-31
		; edible tiles
		; pretzel
		.byte	%01101100
		.byte	%10010010
		.byte	%10010010
		.byte	%10010010
		.byte	%01010100
		.byte	%00101000
		.byte	%01010100
		.byte	%10000010
		; pear
		.byte	%00001000
		.byte	%00010000
		.byte	%00111000
		.byte	%00111000
		.byte	%01111100
		.byte	%11111110
		.byte	%11111110
		.byte	%01101100
		; banana
		.byte	%00001000
		.byte	%00011100
		.byte	%00001110
		.byte	%00001110
		.byte	%00011100
		.byte	%00111100
		.byte	%11111000
		.byte	%01110000
		.byte	$04, $08, $18, $24, $62, $F7, $F2, $60	; $C0 cherry
		.byte	$10, $7C, $FE, $AA, $D6, $AA, $54, $28	; $C8 strawberry
		.byte	$20, $10, $7C, $FE, $FE, $FE, $7C, $38	; $D0 peach
		.byte	$08, $10, $7C, $FE, $FE, $FE, $7C, $28	; $D8 apple
		.byte	$28, $10, $54, $AA, $FE, $54, $AA, $7C	; $E0 pineapple
		.byte	$92, $D6, $FE, $FE, $FE, $54, $10, $10	; $E8 tbird
		.byte	$10, $38, $7C, $7C, $7C, $7C, $FE, $10	; $F0 bell
		.byte	$18, $24, $18, $08, $08, $18, $08, $18	; $F8 key
		;
		; 32-63
		; maze tiles
		.byte	$00, $00, $00, $00, $00, $00, $00, $00	; $00 empty space
		.byte	$00, $00, $00, $18, $18, $00, $00, $00	; $08 dot
		.byte	$00, $3C, $7E, $7E, $7E, $7E, $3C, $00	; $10 powerpill (animated)
POOF1:	.byte	$00, $10, $10, $6C, $10, $10, $00, $00	; $18 explosion
POOF2:	.byte	$10, $44, $28, $C6, $28, $44, $10, $00	; $20 smoke
POOF3:	.byte	$92, $44, $00, $82, $00, $44, $92, $00	; $28 dust
		.byte	$00, $00, $55, $FF, $FF, $AA, $00, $00	; $30 door
		.byte	$00, $55, $01, $03, $03, $01, $55, $00	; $38 doorway east
		.byte	$00, $55, $40, $C0, $C0, $40, $55, $00	; $40 doorway west
		.byte	$00, $FF, $00, $00, $00, $00, $00, $00	; $48 maze wall h-top
		.byte	$00, $00, $00, $00, $00, $00, $FF, $00	; $50 maze wall h-bottom
		.byte	$40, $40, $40, $40, $40, $20, $1F, $00	; $58 maze wall s-w corner
		.byte	$02, $02, $02, $02, $02, $04, $F8, $00	; $60 maze wall s-e corner
		.byte	$00, $1F, $20, $40, $40, $40, $40, $40	; $68 maze wall n-w corner
		.byte	$00, $F8, $04, $02, $02, $02, $02, $02	; $70 maze wall n-e corner
		.byte	$00, $18, $24, $42, $42, $42, $42, $42	; $78 maze wall north
		.byte	$42, $42, $42, $42, $42, $24, $18, $00	; $80 maze wall south
		.byte	$00, $1F, $20, $40, $40, $20, $1F, $00	; $88 maze wall west
		.byte	$00, $F8, $04, $02, $02, $04, $F8, $00	; $90 maze wall east
		.byte	$42, $41, $40, $40, $40, $20, $1F, $00	; $98 maze wall s-w elbow
		.byte	$42, $82, $02, $02, $02, $04, $F8, $00	; $A0 maze wall s-e elbow
		.byte	$00, $1F, $20, $40, $40, $40, $41, $42	; $A8 maze wall n-w elbow
		.byte	$00, $F8, $04, $02, $02, $02, $82, $42	; $B0 maze wall n-e elbow
		.byte	$42, $42, $42, $42, $42, $42, $42, $42	; $B8 maze wall vertical
		.byte	$00, $FF, $00, $00, $00, $00, $FF, $00	; $C0 maze wall horizontal
		.byte	$42, $41, $40, $40, $40, $40, $41, $42	; $C8 maze wall west tee
		.byte	$42, $82, $02, $02, $02, $02, $82, $42	; $D0 maze wall east tee
		.byte	$00, $FF, $00, $00, $00, $00, $81, $42	; $D8 maze wall north tee
		.byte	$42, $81, $00, $00, $00, $00, $FF, $00	; $E0 maze wall south tee
		.byte	$42, $81, $00, $00, $00, $00, $81, $42	; $E8 maze wall cross
		.byte	$00, $FF, $00, $00, $00, $00, $80, $40	; $F0 maze wall north west tee
		.byte	$00, $FF, $00, $00, $00, $00, $01, $02	; $F8 maze wall north east tee
		;
GRAPHICS4:
		; 64-79
		; custom 0-9 digits
		; zero
		.byte	%0000000
		.byte	%0011100
		.byte	%0100110
		.byte	%1100011
		.byte	%1100011
		.byte	%1100011
		.byte	%0110010
		.byte	%0011100
		; one
		.byte	%0000000
		.byte	%0001100
		.byte	%0011100
		.byte	%0001100
		.byte	%0001100
		.byte	%0001100
		.byte	%0001100
		.byte	%0111111
		; two
		.byte	%0000000
		.byte	%0111110
		.byte	%1100011
		.byte	%0000011
		.byte	%0011110
		.byte	%0110000
		.byte	%1100000
		.byte	%1111111
		; three
		.byte	%0000000
		.byte	%0111111
		.byte	%0000110
		.byte	%0001100
		.byte	%0011110
		.byte	%0000011
		.byte	%1100011
		.byte	%0111110
		; four
		.byte	%0000000
		.byte	%0001110
		.byte	%0011110
		.byte	%0110110
		.byte	%1100110
		.byte	%1111111
		.byte	%0000110
		.byte	%0000110
		; five
		.byte	%0000000
		.byte	%1111110
		.byte	%1100000
		.byte	%1111110
		.byte	%0000011
		.byte	%0000011
		.byte	%1100011
		.byte	%0111110
		; six
		.byte	%0000000
		.byte	%0011110
		.byte	%0110000
		.byte	%1100000
		.byte	%1111110
		.byte	%1100011
		.byte	%1100011
		.byte	%0111110
		; seven
		.byte	%0000000
		.byte	%1111111
		.byte	%1000011
		.byte	%0000110
		.byte	%0001100
		.byte	%0011000
		.byte	%0011000
		.byte	%0011000
		; eight
		.byte	%0000000
		.byte	%0111100
		.byte	%1100010
		.byte	%1100010
		.byte	%0111100
		.byte	%1001111
		.byte	%1000011
		.byte	%0111110
		; nine
		.byte	%0000000
		.byte	%0111110
		.byte	%1100011
		.byte	%1100011
		.byte	%0111111
		.byte	%0000011
		.byte	%0000110
		.byte	%0111100
		;
PUPHI:
	; 1U
		.byte	%00000000
		.byte	%01100100
		.byte	%11100100
		.byte	%01100100
		.byte	%01100100
		.byte	%01100100
		.byte	%01100100
		.byte	%11110011
	; 2U
		.byte	%00000000
		.byte	%01100100
		.byte	%10110100
		.byte	%00110100
		.byte	%01100100
		.byte	%11000100
		.byte	%11000100
		.byte	%11110011
	; P:
		.byte	%00000000
		.byte	%10111000
		.byte	%10100100
		.byte	%10100101
		.byte	%10111000
		.byte	%10100001
		.byte	%10100000
		.byte	%00100000
	; P:
		.byte	%00000000
		.byte	%10111000
		.byte	%10100100
		.byte	%10100101
		.byte	%10111000
		.byte	%10100001
		.byte	%10100000
		.byte	%00100000
	; H
		.byte	%00000000		
		.byte	%11000110
		.byte	%11000110
		.byte	%11000110
		.byte	%11111110
		.byte	%11000110
		.byte	%11000110
		.byte	%11000110
	; I:
		.byte	%00000000		
		.byte	%01111000
		.byte	%00110000
		.byte	%00110010
		.byte	%00110000
		.byte	%00110010
		.byte	%00110000
		.byte	%01111000
		;
		; 80-95
QUIKMAN:
		.byte	%00000111
		.byte	%00011111
		.byte	%00111111
		.byte	%01111111
		.byte	%01111111
		.byte	%11111111
		.byte	%11111110
		.byte	%11111100
		;
		.byte	%11111100
		.byte	%11111110
		.byte	%11111111
		.byte	%01111111
		.byte	%01111111
		.byte	%00111111
		.byte	%00011111
		.byte	%00000111
		;
		.byte	%11100000
		.byte	%11110000
		.byte	%11100000
		.byte	%11000000
		.byte	%10000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10000000
		.byte	%11000000
		.byte	%11100000
		.byte	%11110000
		.byte	%11100000
QUIKMAN2:
		.byte	%00000111
		.byte	%00011111
		.byte	%00111111
		.byte	%01111111
		.byte	%01111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111100
		;
		.byte	%11111100
		.byte	%11111111
		.byte	%11111111
		.byte	%01111111
		.byte	%01111111
		.byte	%00111111
		.byte	%00011111
		.byte	%00000111
		;
		.byte	%11100000
		.byte	%11111000
		.byte	%11111100
		.byte	%11111100
		.byte	%11110000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		;
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11110000
		.byte	%11111100
		.byte	%11111100
		.byte	%11111000
		.byte	%11100000
QUIKMAN3:
		.byte	%00000111
		.byte	%00011111
		.byte	%00111111
		.byte	%01111111
		.byte	%01111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		;
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%01111111
		.byte	%01111111
		.byte	%00111111
		.byte	%00011111
		.byte	%00000111
		;
		.byte	%11100000
		.byte	%11111000
		.byte	%11111100
		.byte	%11111110
		.byte	%11111110
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		;
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111110
		.byte	%11111110
		.byte	%11111100
		.byte	%11111000
		.byte	%11100000
		;
R:		.byte	%00111111
		.byte	%01100111
		.byte	%10001110
		.byte	%00001110
		.byte	%00011111
		.byte	%00011100
		.byte	%00111000
		.byte	%01111100
		.byte	%11111110
		.byte	%00000111
		.byte	%00000111
		.byte	%00001110
		.byte	%11111100
		.byte	%11100000
		.byte	%01110000
		.byte	%00111100
		;
H:		.byte	%00111111
		.byte	%01100111
		.byte	%10001110
		.byte	%00001111
		.byte	%00011100
		.byte	%00011100
		.byte	%00111000
		.byte	%01111100
		.byte	%10011111
		.byte	%00001110
		.byte	%00011100
		.byte	%11111100
		.byte	%00111000
		.byte	%00111000
		.byte	%01110000
		.byte	%11111000
		;
		; 96-103
FKEYL:	.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
FKEY:	.byte	%11111011
		.byte	%11101111
		.byte	%11101111
		.byte	%10101011
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
FKEYB:	.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
F1:		.byte	%11111011
		.byte	%11101011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
F3:		.byte	%11101011
		.byte	%10111110
		.byte	%11111110
		.byte	%11101011
		.byte	%11111110
		.byte	%11111110
		.byte	%10111110
		.byte	%11101011
F5:		.byte	%10101010
		.byte	%10111111
		.byte	%10111111
		.byte	%10101011
		.byte	%11111110
		.byte	%11111110
		.byte	%10111110
		.byte	%11101011
F7:		.byte	%10101010
		.byte	%11111110
		.byte	%11111110
		.byte	%11111011
		.byte	%11111011
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		;
ITALIC:	.byte	%00011100
		.byte	%00011100
		.byte	%00111000
		.byte	%00111000
		.byte	%00110000
		.byte	%00000000
		.byte	%01100000
		.byte	%00000000
		;
		; these custom characters persist only for the splash startup
		; CHERRY
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		.byte	%00000011
		.byte	%00001100
		.byte	%00001100
		;
		.byte	%00101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%00101010
		;
		.byte	%00110000
		.byte	%00110000
		.byte	%11110000
		.byte	%11110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		;
		.byte	%00101000
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%00101000
		.byte	%00000000
		; BIG DUDE
		.byte	%00001111
		.byte	%00111111
		.byte	%00111111
		.byte	%11111111
		.byte	%11101011
		.byte	%11101011
		.byte	%11011011
		.byte	%11011011
		;
		.byte	%11111111
		.byte	%11111111
		.byte	%11110011
		.byte	%11001100
		.byte	%11111111
		.byte	%11111111
		.byte	%11111100
		.byte	%11001100
		;
		.byte	%11110000
		.byte	%11111100
		.byte	%11111100
		.byte	%11111111
		.byte	%11101011
		.byte	%11101011
		.byte	%11011011
		.byte	%11011011
		;
		.byte	%11111111
		.byte	%11111111
		.byte	%11001111
		.byte	%00110011
		.byte	%11111111
		.byte	%11111111
		.byte	%00111111
		.byte	%00110011
		; 112
		; copyright
		.byte	%00000000
		.byte	%00111100
		.byte	%01000010
		.byte	%10011001
		.byte	%10100001
		.byte	%10011001
		.byte	%01000010
		.byte	%00111100
		; multicolor frame
@tl:	.byte	%00000000
		.byte	%00000000
		.byte	%00000010
		.byte	%00001011
		.byte	%00101100
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
@tr:	.byte	%00000000
		.byte	%00000000
		.byte	%10000000
		.byte	%11100000
		.byte	%00111000
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
@bl:	.byte	%00110000
		.byte	%00110000
		.byte	%00111000
		.byte	%00001110
		.byte	%00000011
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@br:	.byte	%00001100
		.byte	%00001100
		.byte	%00101100
		.byte	%10110000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@vl:	.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
@vr:	.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
@ht:	.byte	%00000000
		.byte	%00000000
		.byte	%10101010
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@hb:	.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10101010
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@cl:	.byte	%10000000
		.byte	%11100000
		.byte	%10111000
		.byte	%11111100
		.byte	%10110000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
@cr:	.byte	%00000010
		.byte	%00001011
		.byte	%00101110
		.byte	%00111111
		.byte	%00001110
		.byte	%00000011
		.byte	%00000000
		.byte	%00000000
		; 122
@dot1:	.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00011000
@dot2:	.byte	%00011000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@m1:	.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%01100110
		.byte	%01100110
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@m2:	.byte	%00000000
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		.byte	%00000000
		.byte	%00011000
		.byte	%00011000
		.byte	%00000000
		;
T:		.byte	%1111100
		.byte	%0010000
		.byte	%0010000
		.byte	%0010000
		.byte	%0010000
		.byte	%0010000
		.byte	%0010000
		.byte	%0000000

	;	sprite character pool ($6C-$7F) rendered for the playfields

		.include "quikman.s"

