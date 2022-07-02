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

                .include "equates_system_atari8.asm"
                .include "equates_game.asm"


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

                .include "game_data1.asm"
                .include "charset.asm"
                .include "views.asm"


;*******************************************************************************
;*                                                                             *
;*                              G A M E   C O D E                              *
;*                                                                             *
;*******************************************************************************

                .include "initcold.asm"
                .include "gameloop.asm"

                .include "interrupt.asm"

                .include "linedraw.asm"
                .include "attackcomputer.asm"
                .include "hyperwarp.asm"
                .include "projection.asm"
                .include "maneuver.asm"
                .include "explode.asm"
                .include "dock.asm"
                .include "display.asm"
                .include "trigger.asm"
                .include "noise.asm"
                .include "combat.asm"
                .include "keyboard.asm"
                .include "gameover.asm"
                .include "galacticchart.asm"
                .include "titleline.asm"
                .include "sound.asm"
                .include "gameinit.asm"
                .include "galacticchartdraw.asm"
                .include "flush.asm"

                .include "calc.asm"
                .include "encircled.asm"
                .include "panel.asm"

                .include "game_data2.asm"

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
