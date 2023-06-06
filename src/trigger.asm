;*******************************************************************************
;*                                                                             *
;*                                   TRIGGER                                   *
;*                                                                             *
;*                           Handle joystick trigger                           *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine handles the joystick trigger and launches one of our
; starship's photon torpedo. If a target is in full lock-on then a second photon
; torpedo is prepared for automatic launch in the next game loop iteration.
;
; DETAILS
;
; If the trigger is pressed then reset the idle counter and, if not in
; hyperwarp, launch a photon torpedo with the following steps:
;
; (1)  If the trigger was pressed in this game loop iteration, a photon torpedo
;      will be launched if a previously launched photon torpedo is already under
;      way for at least 255 - 232 = 23 game loop iterations. This avoids firing
;      photon torpedoes too rapidly. 
;
; (2)  Start tracking a space object. If it is in full lock-on, set up the
;      lock-on timer, activate photon torpedo tracking, and tweak the last saved
;      trigger state such that our other photon torpedo (if available) is
;      launched automatically in the next game loop iteration.
;
; (3)  If the Photon Torpedoes are destroyed, do nothing.
;
; (4)  If the Photon Torpedoes are damaged, launch a photon torpedo from the
;      same barrel than the previous one. 
;
; (5)  If the Photon Torpedoes are not damaged, launch a photon torpedo from the
;      other barrel. 
;
; (6)  Set the lifetime of our starship's photon torpedo to infinite, set the
;      PLAYER shape to PHOTON TORPEDO. 
;
; (7)  Initialize the position vector of our starship's photon torpedo to:
;
;          x-coordinate := +256 (+$0100) <KM> (Right barrel)
;                          -256 (-$FF00) <KM> (Left barrel)
;          y-coordinate := -256 (-$FF00) <KM>
;          z-coordinate :=   +1 (+$0001) <KM>
;
; (8)  Initialize the velocity vector of our starship's photon torpedo to:
;
;          x-velocity   :=   +0 <KM/H>
;          y-velocity   :=   +0 <KM/H>
;          z-velocity   := +102 <KM/H> (All views but Aft view)
;                          -102 <KM/H> (Aft view)
;
; (9)  Subtract 10 energy units for launching our starship's photon torpedo.
;
; (10) Play the noise sound pattern PHOTON TORPEDO LAUNCHED by continuing code
;      execution into subroutine NOISE ($AEA8).

TRIGGER         lda OLDTRIG0            ; Prep last trigger state

                ldy TRIG0               ; Copy current trigger state
                sty OLDTRIG0            ;
                bne SKIP124             ; Return if trigger currently not pressed

                sty IDLECNTHI           ; Reset idle counter

                ldx WARPSTATE           ; Return if hyperwarp engaged
                bne SKIP124             ;

                ldx BARRELNR            ; Prep barrel number (0 -> left, 1 -> right)

                cmp #1                  ; If trigger is newly pressed -> handle tracking...
                beq SKIP125             ; ...and launch our starship's photon torpedo...
                bcs SKIP127             ; ...else launch our starship's photon torpedo only
SKIP124         rts                     ; Return

;*** Set up our starship's photon torpedo tracking *****************************
SKIP125         lda PL3LIFE,X           ; Return if torpedo's lifetime >= 232 game loops
                cmp #232                ;
                bcs SKIP124             ;

                ldy TRACKDIGIT          ; Store index of tracked space object
                sty PLTRACKED           ;

                lda #12                 ; Prep lock-on lifetime := 12 game loops
                ldy ISINLOCKON          ; If target is in full lock-on...
                sty ISTRACKING          ; ...activate photon torpedo tracking

                beq SKIP126             ; Skip if target not in full lock-on
                lda #0                  ; Prep lock-on lifetime := 0 game loops
SKIP126         sta LOCKONLIFE          ; Store lock-on lifetime (either 0 or 12 game loops)

;*** Launch our starship's photon torpedo **************************************
SKIP127         sty OLDTRIG0            ; Update last trigger state
                bit GCSTATPHO           ; Return if Photon Torpedoes are destroyed
                bvs SKIP124             ;

                bmi SKIP128             ; If Photon Torpedoes damaged launch from same barrel
                txa                     ; ...else switch barrel from which to launch torpedo
                eor #$01                ;
                sta BARRELNR            ;

SKIP128         txa                     ; SUMMARY: Our starship's photon torpedo's...
                sta PL3XPOSSIGN,X       ; x-coordinate := +256 (+$0100) <KM> (right barrel)
                lda BARRELXTAB,X        ; x-coordinate := -256 (-$FF00) <KM> (left barrel)
                sta PL3XPOSHI,X         ; y-coordinate := -256 (-$FF00) <KM>
                lda #255                ; z-coordinate :=   +1 (+$0001) <KM>
                sta PL3LIFE,X           ; ...lifetime := 255 game loops
                sta PL3YPOSHI,X         ;
                lda #0                  ;
                sta PL3SHAPTYPE,X       ; PLAYER3 or PLAYER4 is PHOTON TORPEDO (shape type 0)
                sta PL3ZPOSHI,X         ;
                sta PL3XPOSLO,X         ;
                sta PL3YPOSSIGN,X       ;
                sta PL3YPOSLO,X         ;
                lda #1                  ;
                sta PL3ZPOSSIGN,X       ;
                sta PL3ZPOSLO,X         ;

                lda SHIPVIEW            ; SUMMARY: Our starship's photon torpedo's...
                lsr A                   ; x-velocity :=   +0 <KM/H>
                ror A                   ; y-velocity :=   +0 <KM/H>
                ora #102                ; z-velocity := +102 <KM/H> (Other views)
                sta PL3ZVEL,X           ; z-velocity := -102 <KM/H> (Aft view)
                lda #0                  ;
                sta PL3XVEL,X           ;
                sta PL3YVEL,X           ;

                ldx #2                  ; ENERGY := ENERGY - 10 for launching photon torpedo
                jsr DECENERGY           ;

                ldx #$00                ; Play noise sound pattern PHOTON TORPEDO LAUNCHED
