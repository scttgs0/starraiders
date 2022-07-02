;*******************************************************************************
;*                                                                             *
;*                                   DOCKING                                   *
;*                                                                             *
;*      Handle docking at starbase, launch and return of transfer vessel       *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Handles docking at a starbase, launching and returning the transfer vessel,
; and repairing our starship's subsystems. 
;
; This subroutine changes, if in Front view, the PLAYER-PLAYFIELD priority such
; that PLAYERs like the starbase appear behind the cross hairs, which are part
; of the PLAYFIELD.
;
; BUG (at $ACEE): In Front view, the specific order of PLAYERs (PL0..4) and
; PLAYFIELD colors (PF0..4) is, from front to back:
;
;     PL4 > PF0, PF1, PF2 > PL0 > PL1 > PL2 > PL3 > PF4 (BGR)
;
; This makes the starbase appear behind the cross hairs, but also behind the
; stars, as both cross hairs and stars are part of the PLAYFIELD - a rarely
; noticed glitch.
;
; Note also that, as an exception of the rule, PLAYER4 (transfer vessel) is
; displayed before the PLAYFIELD. Thus, the transfer vessel appears in front of
; the cross hairs!
;
; In Aft view, the arrangement is reversed: PLAYERs are arranged in front of the
; PLAYFIELD. The specific order of PLAYERs (PL0..4) and PLAYFIELD colors
; (PF0..4) is, from front to back:
;
;     PL0 > PL1 > PL2 > PL3 > PL4 > PF0, PF1, PF2 > PF4 (BGR)
;
; In this case, both the starbase and the transfer vessel appear in front of the
; cross hairs! Suggested fix: None, technically not possible.
;
;
; The starbase is tracked and the PLAYER0..2 shape types are set to STARBASE
; RIGHT, STARBASE LEFT, and STARBASE CENTER, respectively, combining them into a
; 3-part starbase shape. 
;
; If this sector is still marked as a starbase sector but no more so on the
; Galactic Chart (if in the meantime either Zylon units have surrounded this
; sector and destroyed the starbase or you have destroyed the starbase with a
; photon torpedo) then the noise sound pattern SHIELD EXPLOSION is played in
; subroutine NOISE ($AEA8) and code execution returns.
;
; Otherwise a minimum distance to the starbase of +32 (+$0020) <KM> is enforced
; and the conditions for a successful docking are checked:
;
; DOCKING CONDITIONS
;
; A docking is successful if all of the following conditions are met:
;
; (1)  The PLAYER2 (STARBASE CENTER) column number is in 120..135. 
;
;      BUG (at $AD39): At first glance, the PLAYER column interval of 120..135
;      corresponds to an almost symmetric interval of -8..+7 PM pixels relative
;      to the horizontal center of the PLAYFIELD, at PLAYER column number 128
;      (48 PM pixels offset to left PLAYFIELD border + 80 PM pixels to the
;      PLAYFIELD center). This is correct only if the PLAYER column number were
;      to designate the horizontal center of the PLAYER. However it designates
;      its left edge! Thus the used pixel column number range 120..135 creates
;      an asymmetric horizontal docking position: A docking is successful if the
;      horizontal position of the starbase shape's center is roughly -5..+10 PM
;      pixels relative to the horizontal center of the PLAYFIELD. Suggested fix:
;      Replace sbc #120 with sbc #117. This leads to an interval of -8..+7
;      pixels relative to the horizontal center of the PLAYFIELD and better
;      symmetry in the horizontal docking position.
;
; (2)  The PLAYER2 (STARBASE CENTER) row number is in 104..119.
;
;      BUG (at $AD43): The PLAYER row interval of 104..119 corresponds to an
;      asymmetric interval of -20..-5 PM pixels relative to the vertical center
;      of the PLAYFIELD, at pixel row number 80 or PLAYER row number 124. It
;      lets you dock at a starbase that "sits" on top of the horizontal cross
;      hairs but not at one that "hangs" from them. Suggested fix: Replace sbc
;      #104 with sbc #108. This leads to an interval of -8..+7 pixels relative
;      to the vertical center of the PLAYFIELD (assuming a PLAYER2 shape of 16
;      pixel height, which is typical during docking) and better symmetry in the
;      vertical docking position. 
;
; (3)  The starbase is in correct distance in front of our starship: The
;      starbase's z-coordinate must be < +512 (+$02**) <KM>.
;
; (4)  Our starship is horizontally level with the starbase: The starbase's
;      y-coordinate must be < +256 (+$01**) <KM>.
;
; (5)  Our starship is at a complete halt.
;
; DOCKING SUCCESSFUL
;
; If the conditions for a successful docking are met, the subsequent docking and
; transfer operation can be divided in the following states, starting with state
; NOT DOCKED:
;
; (1)  NOT DOCKED 
;
;      The docking state is set to ORBIT ESTABLISHED and the title line is
;      updated with "ORBIT ESTABLISHED".
;
; (2)  ORBIT ESTABLISHED 
;
;      After waiting until the title line "ORBIT ESTABLISHED" has disappeared,
;      the transfer vessel is initialized and launched: The PLAYER4 shape type
;      is set to TRANSFER VESSEL. Its position vector is set to a position above
;      and in front of our starship, but behind the starbase:
;
;          x-coordinate :=     +0..+255 (+$00**) <KM>
;          y-coordinate :=   +256..+511 (+$01**) <KM> 
;          z-coordinate := +4096..+4351 (+$10**) <KM>
;
;      Its velocity vector is set to
;
;          x-velocity   := +1 <KM/H> 
;          y-velocity   := -1 <KM/H> 
;          z-velocity   := -7 <KM/H>
;
;      This will move the transfer vessel from behind the starbase into a
;      direction toward and a little to the lower right of our starship. The
;      lifetime of the transfer vessel (and its return journey) is set to 129
;      game loop iterations. Finally, the docking state is set to RETURN
;      TRANSFER VESSEL.
;
; (3)  RETURN TRANSFER VESSEL 
;
;      After checking if the transfer vessel has passed behind our starship, the
;      beeper sound pattern ACKNOWLEDGE is played in subroutine BEEP ($B3A6),
;      the title line is updated with "TRANSFER COMPLETE", our starship's
;      subsystems are repaired, and our starship's ENERGY readout is restored to
;      9999 energy units. by inverting the z-velocity the velocity vector of the
;      transfer vessel is changed to
;
;          x-velocity   := +1 <KM/H> 
;          y-velocity   := -1 <KM/H> 
;          z-velocity   := +7 <KM/H>
;
;      thus launching the transfer vessel on its return journey to the starbase.
;      The docking state is set to TRANSFER COMPLETE. Finally, the screen is
;      updated in subroutine UPDSCREEN ($B07B).
;
; (4)  TRANSFER COMPLETE 
;
;      This docking state marks the end of a successful docking and transfer
;      operation.
;
; DOCKING ABORTED
;
; If the docking conditions above are not met and the docking state is already
; ORBIT ESTABLISHED or RETURN TRANSFER VESSEL then the message "DOCKING ABORTED"
; is displayed and the docking state is set to NOT DOCKED.

DOCKING         lda ISSTARBASESECT      ; Return if not in starbase sector
                beq SKIP111             ;

                lda SHIPVIEW            ; Skip if not in Front view
                bne SKIP112             ;
                lda #$14                ; GTIA: Enable PLAYER4, prio: PFs > PLs > BGR (!)
                sta PRIOR               ; (Cross hairs in front of PLAYERs)

SKIP112         lda #2                  ; Track starbase (PLAYER2)
                sta TRACKDIGIT          ;

;** Initialize starbase shape **************************************************
                lda #shapeSTARBASEC     ; PLAYER2 is STARBASE CENTER (shape type 3)
                sta PL2SHAPTYPE         ;
                lda #shapeSTARBASEL     ; PLAYER1 is STARBASE LEFT (shape type 2)
                sta PL1SHAPTYPE         ;
                lda #shapeSTARBASER     ; PLAYER0 is STARBASE RIGHT (shape type 4)
                sta PL0SHAPTYPE         ;

                lda #255                ; Prep starbase lifetime := 255 game loops (infinite)

                ldx vCurrentSector      ; Skip if starbase in current sector
                ldy gcMemMap,X          ;
                bmi SKIP113             ;

                lda #0                  ; Prep starbase lifetime := 0 game loops (fast death)

SKIP113         sta PL0LIFE             ; PLAYER0 lifetime := either 0 or 255 game loops
                sta PL1LIFE             ; PLAYER1 lifetime := either 0 or 255 game loops
                sta PL2LIFE             ; PLAYER2 lifetime := either 0 or 255 game loops
                sta ISSTARBASESECT      ; Store starbase-in-sector flag
                bmi SKIP114             ; Skip if starbase in current sector

                ldy #2                  ; Init explosion at PLAYER2 (STARBASE CENTER)
                jsr INITEXPL            ;

                ldx #$0A                ; Play noise sound pattern SHIELD EXPLOSION, return
                jmp NOISE               ;

;*** Keep minimum distance to starbase *****************************************
SKIP114         lda PL2ZPOSHI           ; Skip if starbase z-coordinate > +255 (+$00**) <KM>
                bne SKIP115             ;

                lda PL2ZPOSLO           ; Approach starbase not closer than +32 (+$0020) <KM>
                cmp #32                 ;
                bcs SKIP115             ;
                inc PL2ZPOSLO           ; ...else push starbase back

;*** Check if in docking range *************************************************
SKIP115         lda PL2COLUMN           ; Abort docking if PLAYER column number of...
                sec                     ; ...PLAYER2 (STARBASE CENTER) not in 120..135.
                sbc #120                ; (!)
                cmp #16                 ;
                bcs SKIP116             ;

                lda PL2ROWNEW           ; Abort docking if PLAYER row number of...
                sec                     ; ...PLAYER2 (STARBASE CENTER) not in 104..119.
                sbc #104                ; (!)
                cmp #16                 ;
                bcs SKIP116             ;

                lda PL2ZPOSHI           ; Abort docking if...
                cmp #2                  ; ... z-coordinate of starbase >= +512 (+$02**) <KM>
                bcs SKIP116             ;

                lda PL2ZPOSSIGN         ; Abort docking...
                and PL2YPOSSIGN         ; ...if starbase not in front and upper screen half
                eor #$01                ;
                ora VELOCITYLO          ; ...if our starship's velocity not zero
                ora PL2YPOSHI           ; ...if starbase not roughly vertically centered
                ora NEWVELOCITY         ; ...if our starship's new velocity not zero
                beq SKIP119             ; Else skip and handle docking

;*** Docking aborted ***********************************************************
SKIP116         lda DOCKSTATE           ; Skip if DOCKSTATE is NOT DOCKED, TRANSFER COMPLETE
                cmp #2                  ;
                bcc SKIP117             ;

                ldy #$1F                ; Set title phrase "DOCKING ABORTED"
                jsr SETTITLE            ;

SKIP117         lda #0                  ; DOCKSTATE := NOT DOCKED
                sta DOCKSTATE           ;
SKIP118         rts                     ; Return

;*** Docking successful, check docking state ***********************************
SKIP119         bit DOCKSTATE           ; Check DOCKSTATE
                bvs SKIP120             ; If DOCKSTATE = ORBIT ESTABLISHED hide title line
                bmi SKIP122             ; If DOCKSTATE = RETURN TRANSFER VESSEL return it
                lda DOCKSTATE           ;
                bne SKIP118             ; Return if DOCKSTATE not NOT DOCKED
                dec DOCKSTATE           ; DOCKSTATE := ORBIT ESTABLISHED

                ldy #$1C                ; Set title phrase "ORBIT ESTABLISHED" and return
                jmp SETTITLE            ;

;*** Orbit established *********************************************************
SKIP120         ldx #0                  ; Enqueue new, empty title phrase
                stx NEWTITLEPHR         ;

                ldy TITLEPHR            ; Return if "ORBIT ESTABLISHED" still displayed
                bne SKIP118             ;

;*** Launch transfer vessel ****************************************************
                lda #shapeTRANSVSSL     ; PLAYER4 is TRANSFER VESSEL (shape 5)
                sta PL4SHAPTYPE         ;

                lda #1                  ; Place transfer vessel behind starbase:
                sta PL4ZPOSSIGN         ; x-coordinate :=    +0..+255  (+$00**) <KM>
                sta PL4XPOSSIGN         ; y-coordinate :=  +256..+511  (+$01**) <KM>
                sta PL4YPOSSIGN         ; z-coordinate := +4096..+4351 (+$10**) <KM>
                sta PL4YPOSHI           ;
                sta PL4XVEL             ; Move transfer vessel toward our starship:
                lda #$10                ; x-velocity := +1 <KM/H>
                sta PL4ZPOSHI           ; y-velocity := -1 <KM/H>
                lda #$00                ; z-velocity := -7 <KM/H>
                sta PL4XPOSHI           ;
                lda #NEG|7              ;
                sta PL4ZVEL             ;
                lda #NEG|1              ; DOCKSTATE := RETURN TRANSFER VESSEL
                sta DOCKSTATE           ;
                sta PL4YVEL             ;
                sta PL4LIFE             ; Transfer vessel lifetime := 129 game loops
SKIP121         rts                     ; Return

;*** Return transfer vessel ****************************************************
SKIP122         lda PL4ZPOSSIGN         ; Return if transfer vessel in front of our starship
                bne SKIP121             ;

                ldx #$0C                ; Play beeper sound pattern ACKNOWLEGDE
                jsr BEEP                ;

                ldy #$21                ; Set title phrase "TRANSFER COMPLETE"
                jsr SETTITLE            ;

                ldx #5                  ; Repair all 6 subsystems
LOOP041         lda PANELTXTTAB+73,X    ;
                sta GCSTATPHO,X         ;
                dex                     ;
                bpl LOOP041             ;

                lda #ccs_Col2|ccs_9     ; Set starship's ENERGY readout to "9999" in COLOR2
                ldx #3                  ;
LOOP042         sta ENERGYD1,X          ;
                dex                     ;
                bpl LOOP042             ;

                lda #7                  ; Move transfer vessel back toward starbase:
                sta PL4ZVEL             ; x-velocity := -1 <KM/H>
                lda #NEG|1              ; y-velocity := +1 <KM/H>
                sta PL4XVEL             ; z-velocity := +7 <KM/H>
                lda #1                  ;
                sta PL4YVEL             ;

                sta DOCKSTATE           ; DOCKSTATE := TRANSFER COMPLETE
                jmp UPDSCREEN           ; Update screen and return
