
;*******************************************************************************
;*                                                                             *
;*                           S T A R   R A I D E R S                           *
;*                                                                             *
;*                  for the Atari 8-bit Home Computer System                   *
;*                                                                             *
;*       Reverse engineered and documented assembly language source code       *
;*                                                                             *
;*                                     by                                      *
;*                                                                             *
;*                                Lorenz Wiest                                 *
;*                                                                             *
;*                            (lo.wiest(at)web.de)                             *
;*                                                                             *
;*                                First Release                                *
;*                                 22-SEP-2015                                 *
;*                                                                             *
;*                                 Last Update                                 *
;*                                 10-dec-2016                                 *
;*                                                                             *
;*                STAR RAIDERS was created by Douglas Neubauer                 *
;*                  STAR RAIDERS was published by Atari Inc.                   *
;*                                                                             *
;*******************************************************************************

                .cpu "6502"

                .include "equates/system_atari8.equ"
                .include "equates/game.equ"


;--------------------------------------
;--------------------------------------
.if MEDIA==0    ; EXE only
;--------------------------------------
;--------------------------------------
                * = $6000
;--------------------------------------
.elsif MEDIA==1 ; ROM only
;--------------------------------------
;--------------------------------------
                * = $A000
;--------------------------------------
.endif
;--------------------------------------
;--------------------------------------

                .logical $A000

                .include "DATA/PART1.inc"
                .include "DATA/CHARSET.inc"
                .include "DATA/VIEWS.inc"


;*******************************************************************************
;*                                                                             *
;*                              G A M E   C O D E                              *
;*                                                                             *
;*******************************************************************************

                .include "src/initcold.asm"
                .include "src/gameloop.asm"

                .include "src/interrupt.asm"

                .include "src/linedraw.asm"
                .include "src/attackcomputer.asm"
                .include "src/hyperwarp.asm"
                .include "src/projection.asm"
                .include "src/maneuver.asm"
                .include "src/explode.asm"
                .include "src/dock.asm"
                .include "src/display.asm"
                .include "src/trigger.asm"
                .include "src/noise.asm"
                .include "src/combat.asm"
                .include "src/keyboard.asm"
                .include "src/gameover.asm"
                .include "src/galacticchart.asm"
                .include "src/titleline.asm"
                .include "src/sound.asm"
                .include "src/gameinit.asm"
                .include "src/galacticchartdraw.asm"
                .include "src/flush.asm"

                .include "src/calc.asm"
                .include "src/encircled.asm"
                .include "src/panel.asm"

                .include "DATA/PART2.inc"

                .endlogical

;--------------------------------------
;--------------------------------------
.if MEDIA==0    ; EXE only
;--------------------------------------
;--------------------------------------
                * = $0600
;--------------------------------------
RAMTOP      = $6A
CART_INIT   = $BFFE
CART_START  = $BFFA
;---
Bootstrap       lda RAMTOP              ; terminate if RAM size <= 36K
                cmp #$90
                bcc _XIT

                lda #$90
                sta RAMTOP
                jsr $F3F6               ; display handler

;   copy RAM $6000-7FFF -> $A000-BFFF
_setVal1        lda $6000               ; [SMC]
                inc _setVal1+1
                bne _setVal2

                inc _setVal1+2
_setVal2        sta $A000               ; [SMC]
                inc _setVal2+1
                bne _setVal1

                inc _setVal2+2

;   stop when we reach $C000
                lda #$C0
                eor _setVal2+2
                bne _setVal1

                jsr L0631

                jmp (CART_START)

_XIT            rts

L0631           jmp (CART_INIT)


;--------------------------------------
;--------------------------------------
                * = $02E0
;--------------------------------------
                .word Bootstrap
.endif
