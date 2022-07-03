;*******************************************************************************
;*                                                                             *
;*                                FLUSHGAMELOOP                                *
;*                                                                             *
;*         Handle remaining tasks at the end of a game loop iteration          *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine handles at the end of a game loop iteration the following
; tasks:
;
; (1)  Increment counters COUNT256 ($76) and COUNT8 ($72).
;
; (2)  If our starship's energy has dropped below 1000 units then flash a {PINK}
;      alert that changes to {DARK GREY BLUE} and back to {PINK} every 128 game
;      loop iterations.
;
; (3)  Set the Shields color and the Control Panel background color every 8 game
;      loop iterations:
;
;      o   If the Shields are up and OK then set the Shields color to {DARK
;          GREEN} and the Control Panel background color to {DARK BLUE}.
;
;      o   If the Shields are up and damaged there is a probability of 78%
;          (200:256) that the Shield color is not changed.
;
;      o   If the Shields are down, damaged, or destroyed then set the Shields
;          color to {BLACK}.
;
;      o   If the Shields are destroyed then set the Control Panel background
;          color to {ORANGE}.
;
; (4)  Decrement the lifetime of our starship's and Zylon photon torpedoes.
;
; (5)  Decrement the lifetime of an explosion. If the explosion lifetime is less
;      than 112 game loop iterations, clear HITBADNESS ($8A) (thus the explosion
;      cannot destroy our starship). If the explosion lifetime is less than 24
;      (?) game loops decrement the number of explosion fragments. This makes
;      explosion fragments disappear gradually toward the end of an explosion.
;
; (6)  Increment every 40 game loop iterations the stardate clock of the
;      Galactic Chart Panel Display.
;
; (7)  Move Zylon units in the Galactic Chart.
;
;      Every 50 game loop iterations (or 100 game loop iterations when a
;      starbase is surrounded by Zylon units) decrement the score.
;
; Code execution continues if the game is not in demo mode with the following
; steps:
;
; (1)  Search the Galactic Chart for starbases surrounded by Zylon units.
;      Destroy any such starbase: Replace it with a 2-Zylon sector and subtract
;      18 points from the score. If the Subspace Radio was not destroyed, then
;      flash the title phrase "STARBASE DESTROYED" and play the beeper sound
;      pattern MESSAGE FROM STARBASE in subroutine BEEP ($B3A6).
;
; (2)  Every 8 game loop iterations the Zylon units decide, which starbase to
;      hunt: First, 128 randomly picked sectors are searched for a starbase. If
;      no starbase was found in this way, the sectors of the Galactic Chart are
;      scanned systematically left-to-right, top-to-bottom. If a starbase was
;      found then its sector, sector column, and sector row are saved, otherwise
;      code execution returns.
;
; (3)  Now the Zylon units converge toward the sector of the hunted starbase:
;      All sectors of the Galactic Chart are scanned. For any sector with a
;      Zylon unit that was not moved yet (its sector does not have the temporary
;      "already-moved" bit B5 set) its movement probability value is picked from
;      table MOVEPROBTAB ($BFBB):
;
;      +---------------+-------------+----------------+
;      |  Sector Type  |  Movement   |   Movement     | 
;      |               | Probability |  Probability   |
;      |               |    Value    |                |
;      +---------------+-------------+----------------+
;      | Empty sector  |       0     |   0% (  0:256) | 
;      | 1 Zylon ship  |     255     | 100% (255:256) |
;      | 2 Zylon ships |     255     | 100% (255:256) | 
;      | 3 Zylon ships |     192     |  75% (192:256) | 
;      | 4 Zylon ships |      32     |  13% ( 32:256) |
;      +---------------+-------------+----------------+
;
;      If this value is less or equal than a random number in 0..255 then the
;      Zylon unit is moved to another sector. A Zylon unit that currently
;      occupies the sector of our starship is not moved.
;
;      BUG (at $B620): The instruction to check the marker bit B5 of the sector
;      is cpy #$0A. This works, as sectors containing Zylon units that need to
;      be moved have values of 2..4, see table SECTORTYPETAB ($BBA6). Suggested
;      fix: Replace cpy #$0A with cpy #$20, which may make the code clearer.
;
; (4)  For every Zylon unit that is about to be moved, 9 distances ("block
;      distances") between the Zylon unit and the starbase are calculated by
;      tentatively moving the Zylon unit into each of its 8 adjacent sectors -
;      and by moving it not at all. The sector offsets are taken from table
;      COMPASSOFFTAB ($BFC0) which stores direction offsets in the following
;      order: NORTH, NORTHWEST, WEST, SOUTHWEST, SOUTH, SOUTHEAST, EAST,
;      NORTHEAST, CENTER. All 9 distances are stored in 9 consecutive bytes at
;      NEWZYLONDIST ($96).
;
;      NOTE: The last calculated distance is the current distance between Zylon
;      unit and starbase.
;
;      The Zylon unit moves to the first of the 8 adjacent sectors that matches
;      the following conditions: 
;
;      (1)  It is closer to the starbase than the Zylon unit's current sector.
;
;      (2)  It is located inside the Galactic Chart.
;
;      (3)  It is empty.
;
;      (4)  It is not the sector containing our starship.
;
;      If a suitable new sector was found then the Zylon unit is moved to this
;      sector, which is marked with the "already-moved" marker bit B5 in the
;      Galactic Chart memory map. This marker bit prevents a Zylon unit that has
;      been already moved from being moved again. The old Zylon unit sector is
;      cleared. 
;
;      If no suitable new sector was found then the above distance calculations
;      are repeated once again by adding 1 to the current distance between the
;      Zylon unit and the starbase. This may provoke a Zylon unit to move that
;      would not have moved in the previous set of distance calculations.
;
;      After having moved all Zylon units the sectors are stripped of the
;      "already-moved" marker bit B5.
;
; (5)  If a starbase has been surrounded then the Zylon unit movement timer is
;      reset to 99, buying our starship some time to destroy one of the
;      surrounding Zylon units. If the Subspace Radio is not destroyed, then the
;      message "STARBASE SURROUNDED" is flashed in the title line and the beeper
;      sound pattern MESSAGE FROM STARBASE is played in subroutine BEEP ($B3A6).

L_ISDESTROYED   = $6A                   ; Flags the destruction of a starbase.
                                        ; Used values are:
                                        ;   $00 -> Starbase not destroyed
                                        ;   $02 -> Starbase has been destroyed
L_NEWSECTOR     = $6A                   ; Sector to which the Zylon unit is tentatively moved
L_ABSDIFFCOLUMN = $6B                   ; Absolute difference between new Zylon and starbase
                                        ;   column on Galactic Chart in PM pixels
L_LOOPCNT2      = $6B                   ; Loop counter. Used values are: 0..1.
L_DIRECTIONIND  = $6A                   ; Compass rose direction index.
                                        ; Used values are: 0..7.

;*** Increment counters and flash low-energy alert *****************************
FLUSHGAMELOOP   inc COUNT256            ; Increment COUNT256 counter

                ldx #$90                ; Prep DLI background color {DARK GREY BLUE}
                lda COUNT256            ;
                bpl SKIP198             ; Skip if counter < 128.

                ldy ENERGYD1            ; When energy drops below 1000 units...
                cpy #ccs_Col2|ccs_0     ;
                bne SKIP198             ;
                ldx #$44                ; ...prep new DLI background color {PINK}

SKIP198         and #$03                ; Increment COUNT8
                sta COUNT8              ;
                bne SKIP202             ; Skip setting colors but every 8 game loops

;*** Set Shields and Control Panel background color ****************************
                ldy DRAINSHIELDS        ; Skip if Shields are off
                beq SKIP201             ;

                ldy #$A0                ; Prep Shields color {DARK GREEN}
                bit GCSTATSHL           ; Skip if Shields are OK
                bpl SKIP200             ;
                bvs SKIP199             ; Skip if Shields are destroyed
                lda RANDOM              ; If Shields are damaged, Shields colors are...
                cmp #200                ; ...unchanged with probability of 78% (200:256)
                bcc SKIP201             ;

SKIP199         ldy #$00                ; Prep Shields color {BLACK}
SKIP200         tya                     ;
                bne SKIP201             ;

                ldx #$26                ; Prep Control Panel background color {ORANGE}

SKIP201         sty SHIELDSCOLOR        ; Store Shields color
                stx BGRCOLORDLI         ; Store Control Panel background color

;*** Decrement lifetime of our starship's and Zylon photon torpedoes ***********
SKIP202         ldx #2                  ; Loop over PLAYER2..4

LOOP068         lda PL2SHAPTYPE,X       ; Next PLAYER if not PHOTON TORPEDO (shape type 0)
                bne SKIP203             ;

                lda PL2LIFE,X           ; Next PLAYER if this PLAYER not alive
                beq SKIP203             ;

                dec PL2LIFE,X           ; Decrement photon torpedo PLAYER lifetime

SKIP203         dex                     ;
                bpl LOOP068             ; Next PLAYER

;*** Decrement lifetime of explosion *******************************************
                lda EXPLLIFE            ; Skip if explosion lifetime expired
                beq SKIP206             ;

                dec EXPLLIFE            ; Decrement explosion lifetime
                bne SKIP204             ; Skip if explosion lifetime still counting

                ldx #NUMSPCOBJ_NORM     ; Explosion finished,...
                stx MAXSPCOBJIND        ; ...restore normal number of space objects

SKIP204         cmp #112                ; Skip if explosion lifetime >= 112 game loops
                bcs SKIP205             ;

                ldx #0                  ; HITBADNESS := NO HIT
                stx HITBADNESS          ;

SKIP205         cmp #24                 ; Skip if explosion lifetime >= 24 game loops (?)
                bcs SKIP206             ;

                dec MAXSPCOBJIND        ; Decrement number of explosion fragment space objs

;*** Increment stardate clock **************************************************
SKIP206         dec CLOCKTIM            ; Decrement stardate clock timer
                bpl SKIP209             ; Return if timer is still counting

                lda #40                 ; Reset stardate clock timer to 40 game loops
                sta CLOCKTIM            ;

                ldx #4                  ; Increment stardate clock of Galactic Chart Panel
LOOP069         inc GCSTARDAT,X         ;
                lda GCSTARDAT,X         ;
                cmp #(ccs_Col3|rom_9)+1 ;
                bcc SKIP208             ;
                lda #(ccs_Col3|rom_0)   ;
                sta GCSTARDAT,X         ;
                cpx #3                  ;
                bne SKIP207             ;
                dex                     ;
SKIP207         dex                     ;
                bpl LOOP069             ;

;*** Decrement Zylon unit movement timer ***************************************
SKIP208         dec ZYLONUNITTIM        ; Decrement Zylon unit movement timer
                bmi SKIP210             ; If timer < 0 move Zylon units

SKIP209         rts

;*** Restore Zylon unit movement timer and decrement score *********************
SKIP210         lda #49                 ; Reset Zylon unit movement timer to 49
                sta ZYLONUNITTIM        ;

                lda SCORE               ; SCORE := SCORE - 1
                bne SKIP211             ;
                dec SCORE+1             ;
SKIP211         dec SCORE               ;

                ldx ISDEMOMODE          ; Return if in demo mode
                bne SKIP209             ;

;*** Is starbase surrounded? ***************************************************
                stx L_ISDESTROYED       ; Init L_ISDESTROYED with 0 (starbase not destroyed)
LOOP070         lda gcMemMap,X          ; Loop over all sectors, load sector type
                bpl SKIP212             ; Skip if not a starbase sector

                jsr IsSurrounded        ; Skip if starbase sector not completely surrounded
                beq SKIP212             ;

;*** Starbase is surrounded, destroy starbase **********************************
                lda #2                  ; Replace starbase sector with 2-Zylon sector
                sta gcMemMap,X          ;
                sta L_ISDESTROYED       ; Flag destruction of starbase

                sec                     ; SCORE := SCORE - 18
                lda SCORE               ;
                sbc #18                 ;
                sta SCORE               ;
                lda SCORE+1             ;
                sbc #0                  ;
                sta SCORE+1             ;

SKIP212         inx                     ;
                bpl LOOP070             ; Next sector

;*** Report starbase destruction ***********************************************
                lda L_ISDESTROYED       ; Skip if no starbase has been destroyed
                beq SKIP213             ;

                bit GCSTATRAD           ; Skip notification if Subspace Radio destroyed
                bvs SKIP213             ;

                ldy #$15                ; Set title phrase "STARBASE DESTROYED"
                jsr SETTITLE            ;

                ldx #$18                ; Play beeper sound pattern MESSAGE FROM STARBASE
                jsr BEEP                ;

;*** Pick new starbase to be hunted by Zylon units *****************************
SKIP213         dec HUNTTIM             ; Decrement hunting timer
                bmi SKIP214             ; If timer < 0 decide which starbase to hunt

                ldx HUNTSECTOR          ; Skip if Zylon units already picked starbase to hunt
                lda gcMemMap,X          ;
                bmi SKIP215             ;

SKIP214         lda #7                  ; Reset hunting timer
                sta HUNTTIM             ;

                ldy #127                ; Loop over 127(+1) randomly picked sectors
LOOP071         lda RANDOM              ;
                and #$7F                ;
                tax                     ;
                lda gcMemMap,X          ; Skip if starbase sector found
                bmi SKIP215             ;
                dey                     ;
                bpl LOOP071             ; Next sector

                ldx #127                ; Loop over all sectors of the Galactic Chart
LOOP072         lda gcMemMap,X          ;
                bmi SKIP215             ; Skip if starbase sector found
                dex                     ;
                bpl LOOP072             ; Next sector

                rts                     ; (no starbase sector found)

;*** Store coordinates of starbase to be hunted ********************************
SKIP215         stx HUNTSECTOR          ; Store hunted starbase sector column and row
                txa                     ;
                and #$0F                ;
                sta HUNTSECTCOLUMN      ;
                txa                     ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                sta HUNTSECTROW         ;

;*** Move all Zylon units toward hunted starbase *******************************
                ldx #$FF                ;
LOOP073         inx                     ; Loop over all sectors to move Zylon units
                bpl SKIP218             ; Jump into loop body below

;*** Strip marker bits from moved Zylon units **********************************
                ldx #0                  ;
LOOP074         lda gcMemMap,X          ; Loop over all sectors
                and #$DF                ;
                sta gcMemMap,X          ; Strip marker bit B5 from moved Zylon units
                inx                     ;
                bpl LOOP074             ; Next sector

;*** Handle surrounded starbase ************************************************
                bit GCSTATRAD           ; Return if Subspace Radio is destroyed
                bvs SKIP217             ;

                ldx #0                  ; Loop over all sectors
LOOP075         lda gcMemMap,X          ;
                bpl SKIP216             ; Skip if not a starbase sector
                jsr IsSurrounded        ; Skip if starbase not surrounded
                beq SKIP216             ;

                lda #99                 ; Yes, starbase surrounded...
                sta ZYLONUNITTIM        ; ...set Zylon unit movement timer to 99

                ldy #$13                ; Set title phrase "STARBASE SURROUNDED"
                jsr SETTITLE            ;

                ldx #$18                ; Play beeper sound pattern MESSAGE FROM STARBASE...
                jmp BEEP                ; ...and return

SKIP216         inx                     ;
                bpl LOOP075             ; Next sector

SKIP217         rts

;*** Move single Zylon unit ****************************************************
SKIP218         ldy gcMemMap,X          ; X contains current sector
                cpy #$0A                ; Next sector if it has marker bit B5 set (!)
                bcs LOOP073             ;

                lda RANDOM              ; Get random number
                cmp MOVEPROBTAB,Y       ; Get movement probability
                bcs LOOP073             ; Next sector if movement probability < random number

                cpx vCurrentSector      ; Next sector if this is our starship's sector
                beq LOOP073             ;

;*** Compute distance to starbase by moving Zylon unit into 9 directions *******
                ldy #8                  ; Loop over 8(+1) possible directions
LOOP076         clc                     ;
                txa                     ;
                adc COMPASSOFFTAB,Y     ; Add direction offset to current sector
                sta L_NEWSECTOR         ; Store new sector

                and #$0F                ; Calc distance ("block distance") between...
                sec                     ; ...starbase sector and tentative new Zylon sector
                sbc HUNTSECTCOLUMN      ;
                bcs SKIP219             ;
                eor #$FF                ;
                adc #1                  ;
SKIP219         sta L_ABSDIFFCOLUMN     ;
                lda L_NEWSECTOR         ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                sec                     ;
                sbc HUNTSECTROW         ;
                bcs SKIP220             ;
                eor #$FF                ;
                adc #1                  ;
SKIP220         clc                     ;
                adc L_ABSDIFFCOLUMN     ;

                sta NEWZYLONDIST,Y      ; Store distance in distance array
                dey                     ;
                bpl LOOP076             ; Next direction

;*** Pick the shortest distance to starbase ************************************
                lda #1                  ; Loop over compass rose directions twice to...
                sta L_LOOPCNT2          ; ...provoke movement regardless of truncation errors

LOOP077         ldy #7                  ;
LOOP078         lda NEWZYLONDIST,Y      ; Loop over all 7(+1) compass rose directions
                cmp OLDZYLONDIST        ;
                bcs SKIP222             ; Next direction if new distance > current distance

                clc                     ; Calc new Galactic Chart sector for Zylon unit
                txa                     ;
                adc COMPASSOFFTAB,Y     ;
                bmi SKIP222             ; Next direction if new sector outside Galactic Chart

                sty L_DIRECTIONIND      ; Save compass rose direction index
                tay                     ;
                lda gcMemMap,Y          ;
                bne SKIP221             ; Next direction if new sector not empty

                lda gcMemMap,X          ; Preload Zylon sector type to be moved
                cpy vCurrentSector      ;
                beq SKIP221             ; Next direction if sector is our starship's sector

                ora #$20                ; New sector for Zylon unit found!
                sta gcMemMap,Y          ; Temporarily mark that sector with marker bit B5
                lda #0                  ;
                sta gcMemMap,X          ; Clear old Zylon unit sector
                beq SKIP223             ; Next sector (unconditional branch)

SKIP221         ldy L_DIRECTIONIND      ; Restore compass rose direction index
SKIP222         dey                     ; Next compass rose direction
                bpl LOOP078             ;

                inc OLDZYLONDIST        ; Increment center distance
                dec L_LOOPCNT2          ;
                bpl LOOP077             ; Loop over all compass rose directions one more time

SKIP223         jmp LOOP073             ; Next sector
