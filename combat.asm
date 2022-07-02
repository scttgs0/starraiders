;*******************************************************************************
;*                                                                             *
;*                                  HOMINGVEL                                  *
;*                                                                             *
;*      Calculate homing velocity of our starship's photon torpedo 0 or 1      *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Calculates the x (or y) velocity vector component of our starship's photon
; torpedo 0 or 1 when it is tracking (homing in on) a target space object.
;
; Our starship's photon torpedo's x (or y) velocity vector component depends on
; the PLAYER column (or row) number difference between the target PLAYER and our
; starship's photon torpedo PLAYER in Player/Missile (PM) pixels. This
; difference is used as an index to pick the new x (or y) velocity vector
; component of our starship's photon torpedo from table HOMVELTAB ($BFC9):
;
; +---------------+--------------+
; | Difference in | New Velocity |
; |   PM Pixels   |  Component   |
; +---------------+--------------+
; |    >= +7      |   -64 <KM/H> |
; |       +6      |   -56 <KM/H> |
; |       +5      |   -48 <KM/H> |    
; |       +4      |   -40 <KM/H> |
; |       +3      |   -24 <KM/H> |
; |       +2      |   -16 <KM/H> |    
; |       +1      |    -8 <KM/H> |
; |        0      |     0 <KM/H> |
; |       -1      |    +8 <KM/H> |    
; |       -2      |   +16 <KM/H> |
; |       -3      |   +24 <KM/H> |
; |       -4      |   +40 <KM/H> |    
; |       -5      |   +48 <KM/H> |
; |       -6      |   +56 <KM/H> |    
; |    <= -7      |   +64 <KM/H> |
; +---------------+--------------+      
;
; INPUT
;
;   A     = PLAYER column (or row) number difference between the target PLAYER
;           and our starship's photon torpedo PLAYER in Player/Missile pixels
;
;   CARRY = Sign of the PLAYER column (or row) number difference. Used values
;           are:
;     0 -> Negative difference (target PLAYER column (or row) number < our
;          starship's photon torpedo PLAYER column (or row) number
;     1 -> Positive difference (target PLAYER column (or row) number >= our
;          starship's photon torpedo PLAYER column (or row) number
;
; OUTPUT
;
;   A = New velocity vector component of our starship's photon torpedo in <KM/H>

L_VELSIGN       = $6A                   ; Saves velocity sign

HOMINGVEL       ldy #NEG                ; Preload negative velocity sign
                bcs SKIP131             ; Skip if difference is positive

                eor #$FF                ; Invert to get absolute value of difference
                ldy #0                  ; Preload positive velocity sign

SKIP131         sty L_VELSIGN           ; Save velocity sign
                cmp #8                  ;
                bcc SKIP132             ;
                lda #7                  ; Limit difference to 0..7
SKIP132         tay                     ;
                lda L_VELSIGN           ; Reload velocity sign
                ora HOMVELTAB,Y         ; Combine with homing velocity from table
                rts                     ; Return

;*******************************************************************************
;*                                                                             *
;*                                   DAMAGE                                    *
;*                                                                             *
;*             Damage or destroy one of our starship's subsystems              *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Damages or destroys one of our starship's subsystems. There are 6 subsystems:
;
; (1)  Photon Torpedoes
; (2)  Engines
; (3)  Shields
; (4)  Attack Computer
; (5)  Long-Range Scan
; (6)  Subspace Radio
;
; Their status is stored and displayed in the Galactic Chart Panel Display by
; the colored letters PESCLR. The color of each letter represents the
; subsystem's status:
;
; +---------------+------------------+
; | Letter Color  | Subsystem Status |
; +---------------+------------------+
; | {LIGHT GREEN} | OK               |
; | {CORN YELLOW} | Damaged          |
; | {PINK}        | Destroyed        |
; +---------------+------------------+
;
; This subroutine first makes sure that we are not in demo mode. Then it picks a
; random value in 0..255 and the damage probability value. The latter value
; depends on the mission level and is picked from table DAMAGEPROBTAB ($BF10):
;
; +-----------+-------------------+---------------+
; |  Mission  |       Damage      |    Damage     |
; |   Level   | Probability Value |  Probability  |
; +-----------+-------------------+---------------+
; | NOVICE    |          0        |  0% (  0:256) | 
; | PILOT     |         80        | 31% ( 80:256) |
; | WARRIOR   |        180        | 70% (180:256) |
; | COMMANDER |        254        | 99% (254:256) |
; +-----------+-------------------+---------------+
;
; If the random number is lower than the damage probability value, a randomly
; picked subsystem is about to get damaged (or destroyed). There is a built-in
; upfront probability of 25% (2:8) that no subsystem gets harmed.
;
; If the picked subsystem is already destroyed then another subsystem is picked.
;
; Then the title phrase offset is picked from table DAMAGEPHRTAB ($BF14) to
; display the damaged subsystem in the title line. Next, color bits are picked
; that indicate a damaged system.
;
; If the Zylon photon torpedo's lifetime >= 30 game loop iterations the
; subsystem will not only be damaged but destroyed.
;
; NOTE: The Zylon photon torpedo lifetime decreases from 62 to 0 game loop
; iterations. With a remaining lifetime >= 30 game loop iterations it is
; considered strong enough to destroy one of our starship's subsystems. There
; are two exceptions to this rule: If the Attack Computer was picked to be
; destroyed it will be damaged only - not destroyed - if the Long-Range Scan has
; been already destroyed, and vice versa.
;
; Then the title phrase offset from table DESTROYPHRTAB ($BF1A) is picked to
; display the destroyed subsystem in the title line. Next, color bits are picked
; that indicate a destroyed system.
;
; The color of the subsystem's status letter is adjusted in the Galactic Chart
; Panel Display. Next, the title phrase describing the subsystem's status is
; enqueued for display in the title line. If the Attack Computer has been
; destroyed it is switched off and the PLAYFIELD is cleared. The title line is
; updated with the "DAMAGE CONTROL" message. Finally, the beeper sound pattern
; DAMAGE REPORT is played in subroutine BEEP ($B3A6).

DAMAGE          bit ISDEMOMODE          ; Return if in demo mode
                bmi SKIP137             ;

;*** Damage some subsystem *****************************************************
                ldx MISSIONLEVEL        ; Prep mission level
LOOP047         lda RANDOM              ; Return if random number >= damage probability
                cmp DAMAGEPROBTAB,X     ; ...(the latter depends on mission level)
                bcs SKIP137             ;

                and #$07                ; Randomly pick 1 of 6 subsystems
                cmp #6                  ; Return if no subsystem picked
                bcs SKIP137             ;

                tax                     ;
                lda GCSTATPHO,X         ; Get picked subsystem status letter
                asl A                   ; Check bit B6 (= destroyed) of letter code
                bmi LOOP047             ; Try again if subsystem already destroyed

                lda PL2LIFE             ; Load Zylon photon torpedo lifetime...
                cmp #30                 ; ...and compare it to 30 game loops

                lda #CCS_COL2           ; Preload COLOR2 text color bits (= damaged status)
                ldy DAMAGEPHRTAB,X      ; Preload title phrase offset of damaged subsystem

                bcc SKIP135             ; Skip if Zylon torpedo lifetime < 30 game loops

                cpx #3                  ; Skip if selected subsystem not Attack Computer
                bne SKIP133             ;
                bit GCSTATLRS           ; Skip if Long-Range Scan already destroyed
                bvs SKIP135             ;
SKIP133         cpx #4                  ; Skip if selected subsystem is not Long-Range Scan
                bne SKIP134             ;
                bit GCSTATCOM           ; Skip if Attack Computer already destroyed
                bvs SKIP135             ;

SKIP134         lda #CCS_COL3           ; Preload COLOR3 text color bits (= destroyed status)
                ldy DESTROYPHRTAB,X     ; Preload title phrase offset of destroyed subsystem

SKIP135         ora GCSTATPHO,X         ; Combine status letter with new color
                sta GCSTATPHO,X         ;
                sty NEWTITLEPHR         ; Enqueue damage status title phrase
                bit GCSTATCOM           ; Skip if Attack Computer OK or damaged
                bvc SKIP136             ;

                lda #0                  ; Switch Attack Computer off
                sta DRAINATTCOMP        ;
                jsr CLRPLAYFIELD        ; Clear PLAYFIELD

SKIP136         ldy #$52                ; Set title phrase "DAMAGE CONTROL..."
                jsr SETTITLE            ;

                ldx #$12                ; Play beeper sound pattern DAMAGE REPORT
                jsr BEEP                ;

SKIP137         rts                     ; Return

;*******************************************************************************
;*                                                                             *
;*                                  COLLISION                                  *
;*                                                                             *
;*            Detect a collision of our starship's photon torpedoes            *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Both of our starship's photon torpedoes are checked if they have collided with
; a space object represented by PLAYER0..2, such as a Zylon ship, a Zylon photon
; torpedo, a starbase, or a meteor. 
;
; For quick lookup, the following table lists the PLAYERs and what space objects
; they represent:
;
; +--------+--------------------------------------------------+
; | PLAYER |                   Represents                     |
; +--------+--------------------------------------------------+
; |   0    | Zylon ship 0, Starbase Left                      |
; |   1    | Zylon ship 1, Starbase Right                     |
; |   2    | Zylon photon torpedo, Starbase Center, Meteor    | 
; |   3    | Our starship's photon torpedo 0                  |
; |   4    | Our starship's photon torpedo 1, Transfer Vessel |
; +--------+--------------------------------------------------+
;
; NOTE: Only space objects represented by PLAYER0..2 are checked for collisions.
; The transfer vessel of the starbase, represented by PLAYER4, is not checked
; and therefore cannot be destroyed by one of our starship's photon torpedoes.
;
; This subroutine first checks if our starship's photon torpedoes are
; represented by alive PLAYERs with PHOTON TORPEDO shape.
;
; In order to detect a collision with a space object, our starship's photon
; torpedo must compare its x, y, and z coordinates with the ones of the space
; object.
;
; Instead of comparing the x and y coordinates, however, this subroutines uses a
; much more efficient method by inspecting the Player/Missile collision
; registers, as the x and y axis of the 3D coordinate system establish the plane
; in which the TV screen lies. Each of our starship's photon torpedoes has its
; own Player/Missile collision register: PL3HIT ($82) for our starship's photon
; torpedo 0 and PL4HIT ($83) for our starship's photon torpedo 1. By inspecting
; these registers the hit space object is determined: 
;
; +---------------------------------------------------+-------------------------+
; |          Bits B2..0 of Collision Register         |        Hit PLAYER       |
; |              (0 -> Not Hit, 1 -> Hit)             |                         |
; +-----------------+----------------+----------------+                         |
; |     PLAYER2     |     PLAYER1    |    PLAYER0     |                         |
; | (Zylon torpedo) | (Zylon ship 1) | (Zylon ship 0) |                         |
; +-----------------+----------------+----------------+-------------------------+
; |        0        |        0       |        0       | None                    |
; |        0        |        0       |        1       | PLAYER0 (Zylon ship 0)  |
; |        0        |        1       |        0       | PLAYER1 (Zylon ship 1)  |
; |        0        |        1       |        1       | PLAYER1 (Zylon ship 1)  |
; |        1        |        0       |        0       | PLAYER2 (Zylon torpedo) |
; |        1        |        0       |        1       | PLAYER2 (Zylon torpedo) |
; |        1        |        1       |        0       | PLAYER1 (Zylon ship 1)  |
; |        1        |        1       |        1       | PLAYER1 (Zylon ship 1)  |
; +-----------------+----------------+----------------+-------------------------+
;
; If the lifetime of the hit space object has already expired, then the hit is
; ignored.
;
; A collision along the z-axis happens if the z-coordinate of our starship's
; photon torpedo is close enough to the z-coordinate of the space object. This
; is determined as follows:
;
; The absolute value of the z-coordinate of the space object is converted into a
; range index in 0..7. This index picks a minimum and a maximum z-coordinate
; from tables HITMINZTAB ($BF7D) and HITMAXZTAB ($BF75). If the absolute value
; of the z-coordinate of our starship's photon torpedo is inside this interval,
; then our starship's photon torpedo has hit the space object. The following
; table lists the relevant values: 
;
; +-----------------------+-------+--------------------------+--------------------------+
; | ABS(z-Coordinate)     | Range | Min ABS(z-Coordinate)    | Max ABS(z-Coordinate)    |
; | of Space Object       | Index | of Photon Torpedo to Hit | of Photon Torpedo to Hit |
; +-----------------------+-------+--------------------------+--------------------------+
; | <=   511 ($01**) <KM> |   0   |           0 ($00**) <KM> |      < 3328 ($0C**) <KM> |
; | <=  1023 ($03**) <KM> |   1   |           0 ($00**) <KM> |      < 3328 ($0C**) <KM> |
; | <=  1535 ($05**) <KM> |   2   |           0 ($00**) <KM> |      < 3328 ($0C**) <KM> |
; | <=  2047 ($07**) <KM> |   3   |         512 ($02**) <KM> |      < 3328 ($0C**) <KM> |
; | <=  2559 ($09**) <KM> |   4   |        1024 ($04**) <KM> |      < 3840 ($0E**) <KM> |
; | <=  3071 ($0B**) <KM> |   5   |        1536 ($06**) <KM> |      < 3840 ($0E**) <KM> |
; | <=  3583 ($0D**) <KM> |   6   |        2048 ($08**) <KM> |      < 3840 ($0E**) <KM> |
; | <= 65535 ($FF**) <KM> |   7   |        3072 ($0C**) <KM> |      < 8448 ($20**) <KM> |
; +-----------------------+-------+--------------------------+--------------------------+
;
; If a collision has been detected, the "age" (= initial lifetime - remaining
; lifetime) of our starship's photon torpedo is calculated. This age is used to
; delay playing the ZYLON EXPLOSION noise sound pattern. It is also used to
; determine the strength of our starship's photon torpedo. Only photon torpedoes
; of an age < 15 game loop iterations can destroy a Zylon basestar.
;
; Some clean-up work is done before the actual explosion: The lock-on timer, our
; starship's photon torpedo lifetime, and the hit space object's PLAYER lifetime
; is set to 0. 
;
; If a meteor or a Zylon photon torpedo have been hit, then the score is not
; changed, skipping right to the explosion part. Otherwise, our starship's
; photon torpedo tracking flag is cleared and the Galactic Chart Map is updated.
; If a starbase was destroyed, then 3 points are subtracted from the score. If a
; Zylon ship was destroyed, then 6 points are added to the score and the Zylon
; KILL COUNTER readout of the Control Panel Display is incremented. Next, the
; explosion is initialized in subroutine INITEXPL ($AC6B).
;
; NOTE: This subroutine lacks proper explosion initialization if the starbase
; was hit. The actual explosion initialization is done in subroutine DOCKING
; ($ACE6) when the code finds out that the starbase sector is no more marked as
; such in the Galactic Chart.
;
; Finally, the Galactic Chart Map is searched for a remaining Zylon unit. If
; none is found then the mission is complete and code execution continues into
; subroutine GAMEOVER2 ($B121), ending the game. 

L_PLHIT         = $6B                   ; Saves PLAYER (and space object) index of hit PLAYER
L_VIEWDIR       = $6C                   ; Saves view direction. Used values are:
                                        ;   $00 -> Front view
                                        ;   $FF -> Aft view

COLLISION       ldx #2                  ; Loop over our starship's two photon torpedoes
LOOP048         dex                     ;
                bpl SKIP138             ; Branch into loop body below
                rts                     ; Return

;*** Photon torpedo sanity checks **********************************************
SKIP138         lda PL3SHAPTYPE,X       ; Next photon torpedo if PLAYER not a PHOTON TORPEDO
                bne LOOP048             ;

                lda PL3LIFE,X           ; Next photon torpedo if PLAYER not alive
                beq LOOP048             ;

;*** Check if our starship's photon torpedo has hit in x-y plane ***************
                lda PL3HIT,X            ; Check Player/Missile collision register
                and #$07                ; Next torpedo if no torpedo-to-PLAYER collision
                beq LOOP048             ;

                lsr A                   ; Find out which of PLAYER0..2 was hit in PLAYFIELD
                cmp #3                  ;
                bne SKIP139             ;
                lsr A                   ;
SKIP139         tay                     ; Save resulting index of hit PLAYER

                lda PL0LIFE,Y           ; Next torpedo if PLAYER0..2 (= targets) not alive
                beq LOOP048             ;

;*** Has our starship's photon torpedo hit within valid z-coordinate interval? *
                lda SHIPVIEW            ; Skip if in Front view
                beq SKIP140             ;
                lda #$FF                ; Calculate range index...
SKIP140         sta L_VIEWDIR           ; Saves view direction
                eor ZPOSHI,Y            ; Calc ABS(z-coordinate (high byte)) of hit object
                cmp #16                 ; Limit range index to 0..7
                bcc SKIP141             ;
                lda #15                 ;
SKIP141         lsr A                   ;
                sty L_PLHIT             ; Save index of hit PLAYER

                tay                     ;
                lda L_VIEWDIR           ; Reload view direction
                eor PL3ZPOSHI,X         ; Calc ABS(z-coordinate (high byte)) of torpedo

                cmp HITMAXZTAB,Y        ; Next torpedo if torpedo >= max hit z-coordinate
                bcs LOOP048             ;

                cmp HITMINZTAB,Y        ; Next torpedo if torpedo < min hit z-coordinate
                bcc LOOP048             ;

;*** Our starship's photon torpedo has hit within valid z-coordinate interval! *
                ldy L_PLHIT             ; Reload index of hit PLAYER
                sec                     ; Calc "age" of photon torpedo in game loops to...
                lda #255                ; delay playing ZYLON EXPLOSION noise sound pattern
                sbc PL3LIFE,X           ;
                sta NOISEZYLONTIM       ;

                cmp #15                 ; Skip if photon torpedo "age" < 15
                bcc SKIP142             ;
                lda PL0SHAPTYPE,Y       ; CARRY := PLAYER is ZYLON BASESTAR (shape type 8)
                cmp #SHAP_ZBASESTAR     ; (and torpedo "age" good to destroy ZYLON BASESTAR)

;*** Clean up our starship's photon torpedo and hit PLAYER *********************
SKIP142         lda #0                  ; Lock-on lifetime := 0 game loops
                sta LOCKONLIFE          ;
                sta PL3LIFE,X           ; Photon torpedo's lifetime := 0 game loops
                bcs SKIP144             ; If CARRY set do not score, just do explosion

                sta PL0LIFE,Y           ; Hit PLAYER lifetime := 0 game loops

                lda PL0SHAPTYPE,Y       ; If hit PLAYER is...
                beq SKIP144             ; ...a PHOTON TORPEDO (shape type 0)...
                cmp #SHAP_METEOR        ; ...or a METEOR (shape type 6)...
                beq SKIP144             ; ...do not score, just do explosion

                lda #0                  ; Clear photon torpedo tracking flag
                sta ISTRACKING          ;

;*** Zylon ship (or starbase) destroyed! ***************************************
                ldx CURRSECTOR          ; Decrement Zylon count on Galactic Chart
                dec GCMEMMAP,X          ;
                bpl SKIP143             ; Skip if destroyed space object was Zylon ship

;*** Starbase destroyed! *******************************************************
                lda #0                  ; Remove destroyed starbase from Galactic Chart
                sta GCMEMMAP,X          ;
                sec                     ; SCORE := SCORE - 3 for destroying starbase
                lda SCORE               ;
                sbc #3                  ;
                sta SCORE               ;
                lda SCORE+1             ;
                sbc #0                  ;
                sta SCORE+1             ;
                rts                     ; Return

;*** Zylon ship destroyed! *****************************************************
SKIP143         clc                     ; SCORE := SCORE + 6 for destroying Zylon ship
                lda SCORE               ;
                adc #6                  ;
                sta SCORE               ;
                lda SCORE+1             ;
                adc #0                  ;
                sta SCORE+1             ;

                ldx #1                  ; Increment Zylon KILL COUNTER readout...
LOOP049         inc KILLCNTD1,X         ; ...of Control Panel Display
                lda KILLCNTD1,X         ;
                cmp #(CCS_COL1|CCS_9)+1 ;
                bcc SKIP144             ;
                lda #(CCS_COL1|CCS_0)   ;
                sta KILLCNTD1,X         ;
                dex                     ;
                bpl LOOP049             ;

SKIP144         jsr INITEXPL            ; Init explosion at hit PLAYER

;*** Any Zylon ships left? *****************************************************
                ldx #127                ; Scan all sectors of Galactic Chart
LOOP050         lda GCMEMMAP,X          ;
                bmi SKIP145             ;
                bne SKIP146             ; Return if Zylon sector found
SKIP145         dex                     ;
                bpl LOOP050             ;

;*** Game over (Mission Complete) **********************************************
                ldy #$3F                ; Set title phrase "MISSION COMPLETE"
                ldx #0                  ; Set mission bonus offset
                jsr GAMEOVER2           ; Game over
SKIP146         rts                     ; Return
