;*******************************************************************************
;*                                                                             *
;*                                  KEYBOARD                                   *
;*                                                                             *
;*                            Handle Keyboard Input                            *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; If a keyboard code has been collected during a keyboard IRQ in the Immediate
; Interrupt Request handler IRQHNDLR ($A751), the idle counter is reset and the
; PLAYER-PLAYFIELD priority arranges the PLAYERs in front of the PLAYFIELD.
;
; Then, the keyboard code is compared with keyboard codes of table KEYTAB
; ($BABE). If no match is found the "WHAT'S WRONG" message is displayed in the
; title line and code execution returns.
;
; If one of the speed keys '0'..'9' has been pressed, a pending hyperwarp is
; aborted in subroutine ABORTWARP ($A980) and code execution returns. Otherwise
; the Engines drain rate is adjusted as well as the new velocity of our
; starship. If the Engines are damaged, a maximum speed is possible equivalent
; to speed key '5'.
;
; If one of our starship's view keys 'F' (Front), 'A' (Aft), 'G' (Galactic
; Chart), or 'L' (Long-Range Scan) have been pressed, the Display List is
; modified accordingly in subroutine ModDLST ($ADF1) and a new star field of 12
; stars is created with the help of subroutine InitPosVec ($B764). Code
; execution returns via subroutine UPDSCREEN ($B07B).
;
; If one of the 'T' (Tracking Computer), 'S' (Shields) or 'C' (Attack Computer)
; keys have been pressed, the corresponding status bits are toggled and the
; title line is updated with the corresponding title phrase. The beeper sound
; pattern ACKNOWLEDGE is played in subroutine BEEP ($B3A6). The tracking letter
; of the Control Panel Display is updated and the PLAYFIELD is cleared in
; subroutine ClrPlayfield ($AE0D). If the Attack Computer is on, the Front or
; Aft view cross hairs are drawn, depending on the current view of our starship,
; via subroutine DRAWLINES ($A76F).
;
; If the 'H' (Hyperwarp) key has been pressed then the hyperwarp is engaged. Our
; starship's velocity is set to the maximum value, the Engines drain rate is
; increased to the equivalent of speed key '7'. Star trails are prepared. The
; position vector of the Hyperwarp Target Marker (PLAYER3) is set to the
; following values:
;
;     x-coordinate :=   +0 (+$0000) <KM>
;     y-coordinate := +256 (+$0100) <KM>
;     z-coordinate :=    + (+$****) <KM> (sign only)
;
; The velocity vector is set to the following values:
;
;     x-velocity   :=  (not initialized)
;     y-velocity   :=  (not initialized)
;     z-velocity   :=          +0 <KM/H>
;
; The temporary arrival hyperwarp marker column and row numbers are saved. If we
; are not in a NOVICE mission, the maximum veer-off velocity of the Hyperwarp
; Target Marker during hyperwarp is picked from table VEERMASKTAB ($BED7). This
; value depends on the selected hyperwarp energy (and thus on the distance to
; hyperwarp). Finally, the title line displays the "HYPERWARP ENGAGED" message.
;
; If the 'M' (Manual target selector) key has been pressed, the tracked target
; space object is swapped and the corresponding digit of the Control Panel
; Display is toggled between 0 and 1.
;
; If the 'P' (Pause) key has been pressed, an endless loop waits until the
; joystick is pushed.
;
; BUG (at $B103): The endless loop branches back one instruction too far.
; Suggested fix: Branch to instruction lda PORTA at $B0FE.
;
; If the 'INV' (Abort mission) key has been pressed, the mission is aborted by
; setting the mission bonus offset, then displaying the "MISSION ABORTED"
; message in the title line. Code execution continues into subroutine GAMEOVER
; ($B10A).
;
; NOTE: This subroutine has two additional entry points:
;
; (1)  SETVIEW ($B045), which is used to enforce the Front view. It is entered
;      from the game loop GAMELOOP ($A1F3) and subroutines INITSTART ($A15E) and
;      DECENERGY ($B86F).
;
; (2)  UPDSCREEN ($B07B), which draws the cross hairs and the Attack Computer
;      Display, and then sets the tracking letter of the Control Panel Display.
;      It is entered from subroutine Docking ($ACE6).

L_KEYCODE       = $6A                   ; Saves pressed keyboard code

KEYBOARD        lda KEYCODE             ; Return if no keyboard code collected
                beq SKIP150             ;

                ldx #20                 ; Prep keyboard code table loop index
                sta L_KEYCODE           ; Save keyboard code

                lda #0                  ; Reset idle counter
                sta IDLECNTHI           ;
                sta KEYCODE             ; Clear keyboard code

                lda #$11                ; GTIA: Enable PLAYER4, prio: PLs > PFs > BGR
                sta PRIOR               ; (PLAYERs in front of stars - and cross hairs)

;*** Search keyboard code in lookup table **************************************

LOOP051         lda KEYTAB,X            ; Loop over all valid keyboard codes
                cmp L_KEYCODE           ;
                beq SKIP147             ; Branch if matching entry found
                dex                     ;
                bpl LOOP051             ; Next keyboard code

                ldy #$10                ; No match found...
                jmp SETTITLE            ; ...set title phrase "WHATS WRONG?" and return

;*** Handle '0'..'9' keyboard keys (speed) *************************************
SKIP147         cpx #10                 ; Skip section if keyboard code does not match
                bcs SKIP151             ;

                lda WARPSTATE           ; Skip if hyperwarp disengaged...
                beq SKIP148             ;
                jmp ABORTWARP           ; ...else abort hyperwarp

SKIP148         bit GCSTATENG           ; Skip if Engines are OK or destroyed
                bvc SKIP149             ;
                cpx #6                  ; Allow max velocity equivalent to speed key '5'
                bcc SKIP149             ;
                ldx #5                  ;

SKIP149         lda DRAINRATETAB,X      ; Set Engines energy drain rate
                sta DRAINENGINES        ;
                lda VELOCITYTAB,X       ; Set new velocity
                sta NEWVELOCITY         ;
SKIP150         rts

;*** Handle 'F', 'A', 'L', 'G' keyboard keys (our starship's views) ************
SKIP151         cpx #14                 ; Skip section if keyboard code does not match
                bcs SKIP152             ;

;*** Entry to force Front view after game init and failed missions *************
SETVIEW         lda VIEWMODETAB-10,X    ; Store our starship's view type
                sta SHIPVIEW            ;

                ldy DLSTFRAGOFFTAB-10,X ; Get DL fragment offset (Front, Aft, LRS, GC)
                ldx #$02                ; Switch to corresponding view
                lda #$08                ;
                jsr ModDLST             ;

                ldx #NUMSPCOBJ_NORM-1   ; Create new star field of 12 stars
LOOP052         jsr InitPosVec          ;
                dex                     ;
                cpx #NUMSPCOBJ_PL       ;
                bcs LOOP052             ;

                bcc UPDSCREEN           ; Return via updating screen (below)

;*** Handle 'T', 'S', 'C' keyboard keys (Tracking, Shields, Attack Computer) ***
SKIP152         cpx #17                 ; Skip section if keyboard code does not match
                bcs SKIP156             ;

                ldy MSGOFFTAB-14,X      ; Prep title phrase offset "... OFF"
                lda ISTRACKCOMPON-14,X  ; Toggle status bits (also energy consumption values)
                eor MSGBITTAB-14,X      ;
                sta ISTRACKCOMPON-14,X  ;
                beq SKIP153             ;
                ldy MSGONTAB-14,X       ; Prep title phrase offset "... ON"
SKIP153         jsr SETTITLE            ; Set title phrase to "... ON" or "... OFF" version

                ldx #$0C                ; Play beeper sound pattern ACKNOWLEDGE
                jsr BEEP                ;

;*** Update PLAYFIELD (Cross hairs, Attack Computer, set tracking letter) ******
UPDSCREEN       ldx #ccs_T              ; Get custom char 'T' (entry point TRANSFER COMPLETE)
                ldy ISTRACKCOMPON       ;
                beq SKIP154             ; Skip if Tracking Computer is on

                inx                     ; Get custom char 'C'

SKIP154         stx TRACKC1             ; Store tracking character in Control Panel Display
                jsr ClrPlayfield        ; Clear PLAYFIELD
                lda DRAINATTCOMP        ; Return if Attack Computer off
                beq SKIP150             ;

                ldx SHIPVIEW            ; If Aft view   -> Draw Aft cross hairs and return
                beq SKIP155             ; If Front view -> Draw Front cross hairs and ...
                cpx #$01                ;                  ...Attack Computer and return
                bne SKIP150             ;
                ldx #$2A                ;
SKIP155         jmp DRAWLINES           ;

;*** Handle 'H' keyboard key (Hyperwarp) ***************************************
SKIP156         cpx #17                 ; Skip if keyboard code does not match
                bne SKIP158             ;

;*** Engage Hyperwarp **********************************************************
                lda WARPSTATE           ; Return if hyperwarp engaged
                bne SKIP159             ;

                lda #$7F                ; Engage hyperwarp
                sta WARPSTATE           ;
                lda #255                ; Set new velocity
                sta NEWVELOCITY         ;
                lda #30                 ; Set Engines energy drain rate (= speed key '7')
                sta DRAINENGINES        ;

                lda #NUMSPCOBJ_ALL-1    ; Set space obj index of first star of star trail
                sta TRAILIND            ;
                lda #0                  ; Clear star trail delay
                sta TRAILDELAY          ;

                sta PL3XPOSHI           ; Init position vector and velocity vector of...
                sta PL3XPOSLO           ; ... Hyperwarp Target Marker (PLAYER3):
                sta PL3YPOSLO           ; x-coordinate :=   +0 (+$0000) <KM>
                sta PL3ZVEL             ; y-coordinate := +256 (+$0100) <KM>
                lda #1                  ; z-coordinate :=    + (+$****) <KM> (sign only)
                sta PL3ZPOSSIGN         ; z-velocity := +0 <KM/H>
                sta PL3XPOSSIGN         ;
                sta PL3YPOSSIGN         ;
                sta PL3YPOSHI           ;

                lda vWarpArrvColumn     ; Store temp arrival hyperwarp marker column number
                sta WARPTEMPCOLUMN      ;
                lda vWarpArrvRow        ; Store temp arrival hyperwarp marker row number
                sta WARPTEMPROW         ;

                lda MISSIONLEVEL        ; Skip if NOVICE mission
                beq SKIP157             ;

                lda WARPENERGY          ; Bits B0..1 of hyperwarp energy index a table...
                rol A                   ; ...containing the maximum value of how much the...
                rol A                   ; ...Hyperwarp Target Marker will veer off during...
                rol A                   ; ...hyperwarp
                and #$03                ;
                tay                     ;
                lda VEERMASKTAB,Y       ;

SKIP157         sta VEERMASK            ; Store veer-off velocity limitation mask

                ldy #$11                ; Set title phrase "HYPERWARP ENGAGED" and return
                jmp SETTITLE            ;

;*** Handle 'M' keyboard key (Manual Target Selector) key **********************
SKIP158         cpx #19                 ; Skip if keyboard code does not match
                bcs SKIP160             ;

                lda TRACKDIGIT          ; Toggle digit of tracked space object of...
                eor #$01                ; ... Control Panel Display
                and #$01                ;
                sta TRACKDIGIT          ;
SKIP159         rts

;*** Handle 'P' keyboard key (Pause) *******************************************
SKIP160         bne SKIP161             ; Skip if keyboard code does not match

                lda PORTA               ; Push joystick to resume action
                cmp #$FF                ;
                beq SKIP160             ; (!)
                rts

;*** Handle 'INV' keyboard key (Abort Mission) *********************************
SKIP161         ldy #$76                ; Preload title phrase "MISSION ABORTED..."
                ldx #$04                ; Set mission bonus offset
