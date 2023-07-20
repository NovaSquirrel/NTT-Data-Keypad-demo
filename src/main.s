.include "snes.inc"
.include "global.inc"
.smart
.export main, nmi_handler

USE_AUDIO = 1

.segment "ZEROPAGE"
nmis: .res 1
oam_used: .res 2

keydown: .res 4
keynew:  .res 4
keylast: .res 4

CONTROLLER_TYPE = $F000
CONTROLLER_NTT  = $4000
KEY_0           = $0001
KEY_1           = $0002
KEY_2           = $0004
KEY_3           = $0008
KEY_4           = $0010
KEY_5           = $0020
KEY_6           = $0040
KEY_7           = $0080
KEY_8           = $0100
KEY_9           = $0200
KEY_STAR        = $0400
KEY_NUMBER      = $0800
KEY_POINT       = $1000
KEY_C           = $2000
KEY_ALWAYS_0    = $4000
KEY_END         = $8000

WhichKey:       .res 2 ; Most recently pressed key
WhichLetter:    .res 2 ; Which letter within the digit
TimeSincePress: .res 2 ; Frames since a digit was pressed
TileUpdateValue:     .res 2 ; Tile to put on the screen
TileUpdateAddress:   .res 2 ; Address to put TypingTile
TileUpdateValue2:    .res 2 ; Tile to put on the screen
TileUpdateAddress2:  .res 2 ; Address to put TypingTile
.export TypingPointer
TypingPointer:  .res 2 

.segment "BSS"

OAM:   .res 512
OAMHI: .res 512
; OAMHI contains bit 8 of X (the horizontal position) and the size
; bit for each sprite.  It's a bit wasteful of memory, as the
; 512-byte OAMHI needs to be packed by software into 32 bytes before
; being sent to the PPU, but it makes sprite drawing code much
; simpler.  The OBC1 coprocessor used in the game Metal Combat:
; Falcon's Revenge performs the same packing function in hardware,
; possibly as a copy protection method.

.segment "CODE"
;;
; Minimalist NMI handler that only acknowledges NMI and signals
; to the main thread that NMI has occurred.
.proc nmi_handler
  ; Because the INC and BIT instructions can't use 24-bit (f:)
  ; addresses, set the data bank to one that can access low RAM
  ; ($0000-$1FFF) and the PPU ($2100-$213F) with a 16-bit address.
  ; Only banks $00-$3F and $80-$BF can do this, not $40-$7D or
  ; $C0-$FF.  ($7E can access low RAM but not the PPU.)  But in a
  ; LoROM program no larger than 16 Mbit, the CODE segment is in a
  ; bank that can, so copy the data bank to the program bank.
  phb
  phk
  plb

  seta8
  inc a:nmis       ; Increase NMI count to notify main thread
  bit a:NMISTATUS  ; Acknowledge NMI

  ; And restore the previous data bank value.
  plb
  rti
.endproc

;;
; This program doesn't use IRQs either.
.proc irq_handler
  rti
.endproc

.segment "CODE1"
; init.s sends us here
.proc main
  ; Clear zeropage
  setaxy8
  ldx #255
: stz 0,x
  dex
  bpl :-

  setaxy16
  .if ::USE_AUDIO
    jsl spc_boot_apu
  .endif

  jsl load_bg_tiles  ; fill pattern table
  jsl draw_bg        ; fill nametable

  ; In LoROM no larger than 16 Mbit, all program banks can reach
  ; the system area (low RAM, PPU ports, and DMA ports).
  ; This isn't true of larger LoROM or of HiROM (without tricks).
  phk
  plb

  ; Program the PPU for the display mode
  seta8
  stz BGMODE     ; mode 0 (four 2-bit BGs) with 8x8 tiles
  stz BGCHRADDR  ; bg planes 0-1 CHR at $0000

  ; OBSEL needs the start of the sprite pattern table in $2000-word
  ; units.  In other words, bits 14 and 13 of the address go in bits
  ; 1 and 0 of OBSEL.
  lda #$4000 >> 13
  sta OBSEL      ; sprite CHR at $4000, sprites are 8x8 and 16x16
  lda #>$6000
  sta NTADDR+0   ; plane 0 nametable at $6000
  sta NTADDR+1   ; plane 1 nametable also at $6000
  ; set up plane 0's scroll
  stz BGSCROLLX+0
  stz BGSCROLLX+0
  lda #$FF
  sta BGSCROLLY+0  ; The PPU displays lines 1-224, so set scroll to
  sta BGSCROLLY+0  ; $FF so that the first displayed line is line 0

  stz PPURES
  lda #%00010001  ; enable sprites and plane 0
  sta BLENDMAIN
  lda #VBLANK_NMI|AUTOREAD  ; but disable htime/vtime IRQ
  sta PPUNMI

  ; Set up game variables, as if it were the start of a new level.
  setaxy16
  lda #$ffff
  sta WhichKey
  sta TileUpdateValue
  sta TileUpdateValue2
  lda #$6040
  sta TypingPointer
forever:

  ; Draw the player to a display list in main memory
  setaxy16
  stz oam_used

  ; ---

  ldx oam_used
  jsl ppu_clear_oam
  jsl ppu_pack_oamhi

  ; Backgrounds and OAM can be modified only during vertical blanking.
  ; Wait for vertical blanking and copy prepared data to OAM.
  jsl ppu_vsync
  jsl ppu_copy_oam
  seta8
  lda #$0F
  sta PPUBRIGHT  ; turn on rendering

  seta16
  lda TileUpdateValue
  cmp #$ffff
  beq :+
     lda TileUpdateAddress
     sta PPUADDR
     lda TileUpdateValue
     sta PPUDATA
     lda #$ffff
     sta TileUpdateValue
  :

  lda TileUpdateValue2
  cmp #$ffff
  beq :+
     lda TileUpdateAddress2
     sta PPUADDR
     lda TileUpdateValue2
     sta PPUDATA
     lda #$ffff
     sta TileUpdateValue2
  :

  lda #$6020
  sta PPUADDR
  seta8
  lda keydown
  jsr PutHex
  lda keydown+1
  jsr PutHex
  lda keydown+2
  jsr PutHex
  lda keydown+3
  jsr PutHex


  ; wait for control reading to finish
  lda #$01
padwait:
  bit VBLSTATUS
  bne padwait

  ; Update scroll position
  stz BGSCROLLX
  stz BGSCROLLX

  seta16
  lda keydown+2
  sta keylast+2
  seta8

  ; Read additional buttons
  stz keydown+2
  lda #$80
  sta keydown+3
: lda JOY0
  lsr
  seta16
  ror keydown+2
  seta8
  bcc :-

  ; Find newly pressed keys
  seta16
  lda keydown
  sta keylast

  lda JOY1CUR
  sta keydown

  lda keylast
  eor #$ffff
  and keydown
  sta keynew

  lda keylast+2
  eor #$ffff
  and keydown+2
  sta keynew+2
  seta8

  jsr RunKeyboard

  jmp forever
.endproc

.proc RunKeyboard
  phk
  plb

  setaxy16

  ; If it's been a bit, stop previewing and just use the current key
  inc TimeSincePress
  lda TimeSincePress
  cmp #60
  bcc :+
    lda WhichKey
    ina
    beq :+
      jsr TypeAKey
      lda #$ffff
      sta WhichKey
      rts
  :

  lda keynew;+2
  ora keynew+2
  and #KEY_C
  beq :++
    ; Don't allow a key to come out via timer
    lda #$ffff
    sta WhichKey

    stz TileUpdateValue
    stz TileUpdateValue2
    dec TypingPointer

    lda TypingPointer
    cmp #$6040
    bcs :+
      lda #$6040
      sta TypingPointer
    :

    lda TypingPointer
    sta TileUpdateAddress
    ina
    sta TileUpdateAddress2
    rts
  :

  lda keynew;+2 ; Conveniently every button in the second 16 bits is keyboard related
  ora keynew+2
  bne :+
    rts
  :

  ;;E C.#*9876543210
  ldx #0
: lsr
  bcs :+
  inx
  cpx #14
  bne :-
  rts
:

  ; X = the key pressed
  cpx WhichKey
  beq NotNewKey
    phx
    jsr TypeAKey
    plx
    stx WhichKey
    lda #.loword(-1)
    sta WhichLetter
  NotNewKey:

  stz TimeSincePress
  inc WhichLetter

  jsr PreviewAKey

  rts

TypeAKey:
  jsr GetCharacter
  ora #1 << 10
  sta TileUpdateValue
  lda TypingPointer
  inc TypingPointer
  sta TileUpdateAddress
  rts

PreviewAKey:
  jsr GetCharacter
  ora #0 << 10
  sta TileUpdateValue2
  lda TypingPointer
  sta TileUpdateAddress2
  rts

GetCharacter:
  ; Ignore if no key pressed
  lda WhichKey
  ina
  bne :+
    pla
    rts
  :

  ; Get the key table pointer
  lda #.loword(KeyboardLower)
  sta 0
  ; Pick a key from there, and get the list of characters for the key
  ldy WhichKey
  lda (0),y
  and #255
  clc
  adc 0
  sta 0

  ldy WhichLetter
  lda (0),y
  and #255
  bne :+
    ; Wrap around
    stz WhichLetter
    ldy #0

    ; Is it still zero?
    lda (0),y
    and #255
    bne :+
      pla
      rts
  :
  rts

; https://toot.cat/@rnd/110668370139659844

; 1     2ABC 3DEF
; 4GHI  5JKL 6MNO
; 7PRQS 8TUV 9WXYZ
;       0[ ]
; 1 usually does punctuation


KeyboardPointers:
  .addr KeyboardLower
  .addr KeyboardUpper

KeyboardLower:
:
  .byt @K0 - :-
  .byt @K1 - :-
  .byt @K2 - :-
  .byt @K3 - :-
  .byt @K4 - :-
  .byt @K5 - :-
  .byt @K6 - :-
  .byt @K7 - :-
  .byt @K8 - :-
  .byt @K9 - :-
  .byt @KS - :-
  .byt @KN - :-
  .byt @KP - :-
@K0:
  .byt " .,",59,0
@K1:
  .byt 0
@K2:
  .asciiz "abc2"
@K3:
  .asciiz "def3"
@K4:
  .asciiz "ghi4"
@K5:
  .asciiz "jkl5"
@K6:
  .asciiz "mno6"
@K7:
  .asciiz "pqrs7"
@K8:
  .asciiz "tuv8"
@K9:
  .asciiz "wxyz9"
@KS:
  .byt 0
@KN:
  .byt 0
@KP:
  .byt 0

KeyboardUpper:
:
  .byt @K0 - :-
  .byt @K1 - :-
  .byt @K2 - :-
  .byt @K3 - :-
  .byt @K4 - :-
  .byt @K5 - :-
  .byt @K6 - :-
  .byt @K7 - :-
  .byt @K8 - :-
  .byt @K9 - :-
  .byt @KS - :-
  .byt @KN - :-
  .byt @KP - :-
@K0:
  .asciiz " !?-0"
@K1:
  .byt 0
@K2:
  .asciiz "ABC2"
@K3:
  .asciiz "DEF3"
@K4:
  .asciiz "GHI4"
@K5:
  .asciiz "JKL5"
@K6:
  .asciiz "MNO6"
@K7:
  .asciiz "PQRS7"
@K8:
  .asciiz "TUV8"
@K9:
  .asciiz "WXYZ9"
@KS:
  .byt 0
@KN:
  .byt 0
@KP:
  .byt 0
.endproc

.proc PutHex
  php
  setaxy8
  pha
  lsr
  lsr
  lsr
  lsr
  and #15
  tax
  lda Table,x
  sta PPUDATA
  lda #2 << (10-8)
  sta PPUDATA+1

  pla
  and #15
  tax
  lda Table,x
  sta PPUDATA
  lda #2 << (10-8)
  sta PPUDATA+1

  plp
  rts

Table:
  .byt "0123456789ABCDEF"
.endproc
