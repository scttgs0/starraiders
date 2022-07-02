;*******************************************************************************
;*                                                                             *
;*                                  UPDPANEL                                   *
;*                                                                             *
;*                        Update Control Panel Display                         *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine executes the following steps: 
;
; (1)  Accelerate or decelerate our starship, update the VELOCITY readout
;
;      If the new velocity value is different from the current one either
;      increment or decrement the current velocity value toward the new one.
;
;      If the Engines are damaged or destroyed (and hyperwarp is not engaged)
;      then store a random value (less or equal than the current velocity) as
;      the current velocity.
;
;      Display the updated velocity by the VELOCITY readout of the Control Panel
;      Display.
;
; (2)  Update THETA, PHI, and RANGE readouts.
;
;      If the Attack Computer is working then display the x, y, and z
;      coordinates of the currently tracked space object as THETA, PHI, and
;      RANGE readout values of the Control Panel Display.
;
; (3)  Calculate overall energy consumption.
;
;      Add the overall energy consumption per game loop iteration to the energy
;      counter. This value is given in energy subunits (256 energy subunits = 1
;      energy unit displayed by the 4-digit ENERGY readout of the Console Panel
;      Display). It is the total of the following items:
;
;      (1)  8 energy subunits if the Shields are up
;
;      (2)  2 energy subunits if the Attack Computer is on
;
;      (3)  1 energy subunit of the life support system
;
;      (4)  Our starship's Engines energy drain rate (depending on its velocity)
;
;      If there is a carryover of the energy counter then decrement the ENERGY
;      readout of the Control Panel Display by one energy unit after code
;      execution has continued into subroutine DECENERGY ($B86F). 

;*** Accelerate or decelerate our starship *************************************
UPDPANEL        ldx VELOCITYLO          ; Skip if new velocity = current velocity
                cpx NEWVELOCITY         ;
                beq SKIP241             ;

                bcc SKIP240             ; In/decrement current velocity toward new velocity
                dec VELOCITYLO          ;
                bcs SKIP242             ;
SKIP240         inc VELOCITYLO          ;

SKIP241         lda WARPSTATE           ; Skip if hyperwarp engaged
                bne SKIP242             ;

                bit GCSTATENG           ; Skip if Engines are OK
                bpl SKIP242             ;

                lda NEWVELOCITY         ; Store RND(0..current velocity) to current velocity
                and RANDOM              ;
                sta VELOCITYLO          ;

SKIP242         ldy #VELOCD1-PANELTXT-1 ; Update digits of VELOCITY readout
                jsr SHOWDIGITS          ;

;*** Display coordinates of tracked space object of Control Panel Display ******
                bit GCSTATCOM           ; Skip if Attack Computer damaged or destroyed
                bmi SKIP243             ;

                lda #$31                ; Update THETA readout (x-coordinate)
                ldy #THETAC1-PANELTXT   ;
                jsr SHOWCOORD           ;

                lda #$62                ; Update PHI readout (y-coordinate)
                ldy #PHIC1-PANELTXT     ;
                jsr SHOWCOORD           ;

                lda #$00                ; Update RANGE readout (z-coordinate)
                ldy #RANGEC1-PANELTXT   ;
                jsr SHOWCOORD           ;

                lda RANGEC1+2           ; Hack to clear RANGE digit 3 when in hyperwarp:
                sta RANGEC1+3           ; Copy RANGE digit 2 to digit 3
                cmp #CCS_9+1            ; Skip if digit character > '9' (= 'infinity' char)
                bcs SKIP243             ;

                ldx TRACKDIGIT          ; Get z-coordinate (low byte) of tracked space object
                lda ZPOSLO,X            ;
                lsr A                   ; ...divide it by 16...
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                tax                     ;
                lda MAPTOBCD99,X        ; ...map value of $00..$0F to BCD value 0..9
                sta RANGEC1+3           ; ...and store it to RANGE digit 3

;*** Calculate overall energy consumption **************************************
SKIP243         clc                     ;
                lda ENERGYCNT           ; Load energy counter
                adc DRAINSHIELDS        ; Add energy drain rate of Shields
                adc DRAINENGINES        ; Add energy drain rate of our starship's Engines
                adc DRAINATTCOMP        ; Add energy drain rate of Attack Computer
                adc #$01                ; Add 1 energy subunit of life support system
                cmp ENERGYCNT           ;
                sta ENERGYCNT           ;
                bcs SKIP246             ; Return if no energy counter carryover

                ldx #3                  ; Will decrement third energy digit

;*******************************************************************************
;*                                                                             *
;*                                  DECENERGY                                  *
;*                                                                             *
;*                               Decrease energy                               *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; When not in demo mode, subtract energy from the 4-digit ENERGY readout of the
; Control Panel Display. If crossing a 100-energy-unit boundary during
; subtraction the score is decremented by one unit. If the energy is zero the
; game is over.
;
; INPUT
;
;   X = ENERGY readout digit to be decremented. Used values are:
;     1 -> Subtract 100 units from ENERGY readout
;     2 -> Subtract  10 units from ENERGY readout
;     3 -> Subtract   1 unit  from ENERGY readout

;*** Display ENERGY readout ****************************************************
DECENERGY       bit ISDEMOMODE          ; Return if in demo mode
                bvs SKIP246             ;

                dec ENERGYD1,X          ; Decrement energy digit character
                lda ENERGYD1,X          ;
                cmp #CCS_COL2|CCS_0     ;
                bcs SKIP246             ; Return if digit character >= '0'
                lda #CCS_COL2|CCS_9     ;
                sta ENERGYD1,X          ; Store digit character '9'

;*** Decrement score when crossing a 100-energy-unit boundary while subtracting 
                cpx #2                  ; Skip if no crossing of 100-energy-unit boundary
                bne SKIP245             ;

                lda SCORE               ; SCORE := SCORE - 1
                bne SKIP244             ;
                dec SCORE+1             ;
SKIP244         dec SCORE               ;

SKIP245         dex                     ;
                bpl DECENERGY           ; Next digit

;*** Energy is zero, game over *************************************************
                ldx #CCS_SPC            ; Clear 4-digit ENERGY readout
                txa                     ;
                ldy #3                  ;
LOOP079         sta ENERGYD1,Y          ;
                dey                     ;
                bpl LOOP079             ;

                jsr SETVIEW             ; Set Front view

                ldy #$31                ; Set title phrase "MISSION ABORTED ZERO ENERGY"
                ldx #$04                ; Set mission bonus offset
                jsr GAMEOVER            ; Game over

SKIP246         rts                     ; Return

;*******************************************************************************
;*                                                                             *
;*                                  SHOWCOORD                                  *
;*                                                                             *
;*  Display a position vector component (coordinate) in Control Panel Display  *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Displays a position vector component (coordinate) by one of the THETA, PHI, or
; RANGE readouts of the Control Panel Display.
;
; Write the sign to the Control Panel Display, then map the high byte of the
; respective coordinate (x -> THETA, y -> PHI, z -> RANGE) to a BCD-value in
; 00..99. Code execution continues into subroutine SHOWDIGITS ($B8CD) where the
; digits are actually stored in the Control Panel Display.
;
; NOTE: If the digits of either the THETA or PHI readout are to be displayed and
; the x or y position vector component (high byte) is $FF then tweak the value
; to $FE. This avoids accessing table MAPTOBCD99 ($0EE9) with an index of $FF
; that would return the special value $EA. This value represents the CCS_INF
; ($0E) and CCS_SPC ($0A) characters (see comments in subroutine INITIALIZE
; ($B3BA)) that are displayed by the RANGE readout only.
;
; INPUT
;
;   A = Position vector component (coordinate) offset. Used values are: 
;     $00 -> z-coordinate
;     $31 -> x-coordinate
;     $62 -> y-coordinate
;
;   Y = Offset into the Control Panel Display memory map. Used values are:
;     $17 -> First character (sign) of THETA readout (x-coordinate of tracked
;            space object)
;     $1D -> First character (sign) of PHI readout   (y-coordinate of tracked
;            space object)
;     $23 -> First character (sign) of RANGE readout (z-coordinate of tracked
;            space object)

L_SIGNCHAR      = $6A                   ; Saves sign character

SHOWCOORD       clc                     ; Add index of tracked space object...
                adc TRACKDIGIT          ; ...to position vector component offset
                tax                     ; Save position vector component index

;*** Display sign in Control Panel Display *************************************
                lda #CCS_PLUS           ; Save '+' (CCS_PLUS) to sign character
                sta L_SIGNCHAR          ;

                lda ZPOSSIGN,X          ; Prep sign of coordinate
                lsr A                   ;
                lda ZPOSHI,X            ; Prep coordinate (high byte)
                bcs SKIP247             ; Skip if sign is positive

                eor #$FF                ; Invert coordinate (high byte)
                dec L_SIGNCHAR          ; Change saved sign character to '-' (CCS_MINUS)

SKIP247         tax                     ; Save coordinate (high byte)
                lda L_SIGNCHAR          ; Store sign character in Control Panel Display
                sta PANELTXT,Y          ;

;*** Get RANGE digits **********************************************************
                tya                     ; Skip if RANGE is to be displayed
                and #$10                ;
                beq SHOWDIGITS          ;

                cpx #$FF                ; If coordinate (high byte) = $FF decrement value
                bne SHOWDIGITS          ; This avoids output of CCS_INFINITY in...
                dex                     ; ...THETA and PHI readouts

;*******************************************************************************
;*                                                                             *
;*                                 SHOWDIGITS                                  *
;*                                                                             *
;*          Display a value by a readout of the Control Panel Display          *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Converts a binary value in $00..$FF to a BCD-value in 0..99 and displays it as
; a 2-digit number in the Control Panel Display.
;
; INPUT
;
;   X = Value to be displayed as a 2-digit BCD-value. Used values are: $00..$FF.
;
;   Y = Offset into the Control Panel Display memory map relative to the first
;       character of the Control Panel Display (the 'V' of the VELOCITY
;       readout). Used values are: 
;     $01 -> Character before first digit of VELOCITY readout
;     $17 -> First character (sign) of THETA readout (x-coordinate of tracked
;            space object)
;     $1D -> First character (sign) of PHI readout   (y-coordinate of tracked
;            space object)
;     $23 -> First character (sign) of RANGE readout (z-coordinate of tracked
;            space object)

SHOWDIGITS      lda MAPTOBCD99,X        ; Map binary value to BCD-value
                tax                     ;
                and #$0F                ;
                sta PANELTXT+2,Y        ; Store 'ones' digit in Control Panel Display
                txa                     ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                sta PANELTXT+1,Y        ; Store 'tens' digit in Control Panel Display
                rts                     ; Return
