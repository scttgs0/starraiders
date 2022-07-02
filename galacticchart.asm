;*******************************************************************************
;*                                                                             *
;*                                 SELECTWARP                                  *
;*                                                                             *
;*             Select hyperwarp arrival location on Galactic Chart             *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine executes the following steps:
;
; (1)  Check if we are in Galactic Chart view and not in hyperwarp.
;
; (2)  Update the Galactic Chart in subroutine DrawGalacticChart ($B4B9) if the Subspace
;      Radio is not damaged.
;
; (3)  Move the arrival hyperwarp marker (PLAYER4) across the Galactic Chart
;      every other game loop iteration. The current location of our starship is
;      indicated by the departure hyperwarp marker (PLAYER3).
;
; Code execution continues into subroutine CALCWARP ($B1A7) to calculate the
; required hyperwarp energy to hyperwarp from the departure hyperwarp marker
; position to the arrival hyperwarp marker position.
;
; NOTE: To calculate the horizontal position of PLAYER3..4 an offset of 61 is
; added (from left to right: 48 Player/Missile (PM) pixels to the left edge of
; the screen + 16 PM pixels to the left border of the Galactic Chart - 3 PM
; pixels relative offset of the PLAYER shape's horizontal center to its left
; edge = 61 PM pixels).
;
; NOTE: To calculate the vertical position of PLAYER3..4 an offset of 63 is
; added (from top to bottom: 8 Player/Missile (PM) pixels to the start of the
; Display List + 56 PM pixels to the first row of sectors - 1 PM pixel relative
; offset of the PLAYER shape's vertical center to its top edge (?) = 63 PM
; pixels).

SELECTWARP      lda WARPSTATE           ; Return if hyperwarp engaged
                bne SKIP166             ;

                lda SHIPVIEW            ; Return if not in Galactic Chart view
                bmi SKIP167             ;
SKIP166         rts                     ; Return

SKIP167         bit GCSTATRAD           ; Skip if Subspace Radio is damaged or destroyed
                bmi SKIP168             ;

                jsr DrawGalacticChart   ; Redraw Galactic Chart

SKIP168         lda COUNT8              ; Move hyperwarp markers only every other game loop
                and #$01                ; (slowing down movement of hyperwarp markers)
                bne CALCWARP            ;

;*** Calc arrival hyperwarp marker column and row numbers, update PLAYER4 pos **
                clc                     ;
                lda vWarpArrvColumn     ; Load arrival hyperwarp marker column number
                adc JOYSTICKX           ; Add joystick x-delta
                and #$7F                ; Limit value to 0..127
                sta vWarpArrvColumn     ; Save new arrival hyperwarp marker column number
                clc                     ;
                adc #61                 ; Add offset of 61
                sta PL4COLUMN           ; Store as PLAYER4 column number

                clc                     ;
                lda vWarpArrvRow        ; Load arrival hyperwarp marker row number
                adc JOYSTICKY           ; Add joystick y-delta
                and #$7F                ; Limit value to 0..127
                sta vWarpArrvRow        ; Save new arrival hyperwarp marker row number
                clc                     ;
                adc #63                 ; Add offset of 63
                sta PL4ROWNEW           ; Store as PLAYER4 row number

;*** Calc departure hyperwarp marker column and row numbers, update PLAYER3 pos 
                lda vWarpDeprRow        ; Load departure hyperwarp marker row number
                clc                     ;
                adc #63                 ; Add offset of 63
                sta PL3ROWNEW           ; Store as PLAYER3 row number

                lda vWarpDeprColumn     ; Load departure hyperwarp marker column number
                clc                     ;
                adc #61                 ; Add offset of 61
                sta PL3COLUMN           ; Store as PLAYER3 column number

;*******************************************************************************
;*                                                                             *
;*                                  CALCWARP                                   *
;*                                                                             *
;*                   Calculate and display hyperwarp energy                    *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Calculates and displays the hyperwarp energy in the Galactic Chart view.
;
; This subroutine executes the following steps:
;
; (1)  Determine the arrival sector from the arrival hyperwarp marker position.
;
; (2)  If the Subspace Radio is not destroyed, update the target number digit of
;      the Galactic Chart Panel Display.
;
; (3)  Calculate the hyperwarp energy that is required to hyperwarp from the
;      departure hyperwarp marker to the arrival hyperwarp marker based on the
;      "block-distance":
;
;          DISTANCE := DELTAR / 2 + DELTAC
;
;          where
;
;          DELTAR := ABS(vWarpArrvRow - vWarpDeprRow)
;          DELTAC := ABS(vWarpArrvColumn - vWarpDeprColumn)
;
;      NOTE: Dividing DELTAR by 2 compensates for PLAYERs at single-line
;      resolution having Player/Missile pixels that are half as high as they are
;      wide.
;
;      The hyperwarp energy, divided by 10, is the sum of a value picked from
;      the hyperwarp energy table WARPENERGYTAB ($BADD) indexed by DISTANCE / 8)
;      plus a remainder computed from Bits B1..0 of DISTANCE. 
;
; (4)  Store the hyperwarp energy value in WARPENERGY ($91).
;
; (5)  Update the HYPERWARP ENERGY readout of the Galactic Chart Panel Display.

L_WARPARRVCOL   = $6A                   ; Saves arrival sector column number
L_DELTAC        = $6A                   ; Saves diff column value

;*** Calculate arrival sector **************************************************
CALCWARP        lda vWarpArrvColumn     ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                sta L_WARPARRVCOL       ; A := arrival sector column 0..15
                lda vWarpArrvRow        ;
                and #$70                ; A := arrival sector row (0..7) * 16
                ora L_WARPARRVCOL       ;
                sta ARRVSECTOR          ; Save arrival sector (format %0rrrcccc)

;*** Update target number digit of Galactic Chart Panel Display ****************
                tax                     ;
                lda gcMemMap,X          ; Get number of Zylon ships in arrival sector
                bpl SKIP169             ; Skip if no starbase in arrival sector
                lda #0                  ; Clear number of Zylon ships
SKIP169         ora #ccs_Col2|rom_0     ; Merge COLOR2 bits with number of Zylon ships
                bit GCSTATRAD           ; Skip if Subspace Radio destroyed
                bvs SKIP170             ;

                sta GCTRGCNT            ; Set target number digit of Galactic Chart Panel

;*** Calculate energy to hyperwarp between hyperwarp markers *******************
SKIP170         sec                     ; A := DELTAC := ABS(vWarpArrvColumn - vWarpDeprColumn)
                lda vWarpArrvColumn     ; (Column value difference)
                sbc vWarpDeprColumn     ;
                bcs SKIP171             ;
                eor #$FF                ;
                adc #1                  ;
SKIP171         sta L_DELTAC            ;

                sec                     ; A := DELTAR := ABS(vWarpArrvRow - vWarpDeprRow)
                lda vWarpArrvRow        ; (Row value difference)
                sbc vWarpDeprRow        ;
                bcs SKIP172             ;
                eor #$FF                ;
                adc #1                  ;

SKIP172         lsr A                   ; A := DISTANCE := DELTAR / 2 + DELTAC
                clc                     ;
                adc L_DELTAC            ;

                tay                     ; Save DISTANCE
                lsr A                   ; Calc index into hyperwarp energy table
                lsr A                   ;
                lsr A                   ;
                tax                     ;

                tya                     ; Load DISTANCE value
                and #$03                ; Get DISTANCE bits B1..0
                clc                     ;
                adc WARPENERGYTAB,X     ; Add hyperwarp energy from table
                sta WARPENERGY          ; Save hyperwarp energy

;*** Update HYPERWARP ENERGY readout of Galactic Chart Panel Display ***********
                tay                     ; Prep with hyperwarp energy value

                lda #rom_0              ; Set HYPERWARP ENERGY readout digit1..3 to '0'
                sta GCWARPD1            ;
                sta GCWARPD1+1          ;
                sta GCWARPD1+2          ;

LOOP053         ldx #2                  ; Loop over HYPERWARP ENERGY readout digit3..1
LOOP054         inc GCWARPD1,X          ; Increment digit value
                lda GCWARPD1,X          ;
                cmp #rom_9+1            ;
                bcc SKIP173             ; Skip if energy digit <= '9'

                lda #rom_0              ; Replace energy digit with '0'
                sta GCWARPD1,X          ;
                dex                     ;
                bpl LOOP054             ; Next energy digit

SKIP173         dey                     ; Decrement HYPERWARP ENERGY readout value
                bne LOOP053             ;
                rts                     ; Return
