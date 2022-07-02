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

                .cpu "65816"

                .include "equates_system_c256.asm"
                .include "equates_system_atari8.asm"
                .include "equates_game.asm"

                .include "macros_65816.asm"
                .include "macros_frs_graphic.asm"
                .include "macros_frs_mouse.asm"

;--------------------------------------
;--------------------------------------
                * = CHARSET-40
;--------------------------------------
                .text "PGX"
                .byte $01
                .dword BOOT

BOOT            clc
                xce
                .m8i8
                .setdp $0000
                .setbank $00

                jml INITCOLD


;--------------------------------------
;--------------------------------------
                * = $A000
;--------------------------------------

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
