
;*******************************************************************************
;*                                                                             *
;*                                  MANEUVER                                   *
;*                                                                             *
;*     Maneuver our starship's and Zylon photon torpedoes and Zylon ships      *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine maneuvers both of our starship's photon torpedoes, the single
; Zylon photon torpedo, and the one or two Zylon ships that are simultaneously
; displayed on the screen. It also creates meteors and new Zylon ships. This
; subroutine is executed only if our starship is not in a starbase sector and
; hyperwarp is not engaged.
;
; BACKGROUND
;
; When a Zylon ship is initialized, a "flight pattern" is assigned to it. There
; are 3 flight patterns (0, 1, and 4) which are picked from table ZYLONFLPATTAB
; ($BF91).
;
; The flight pattern determines the maximum velocity with which a Zylon ship can
; move along each axis of the 3D coordinate system, that is, the maximum value
; of a velocity vector component. Velocity vector components for Zylon ships are
; picked from the Zylon velocity table ZYLONVELTAB ($BF99):
;
; +-----------------+-----+-----+-----+-----+-----+-----+-----+-----+
; | Velocity Index  |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |
; +-----------------+-----+-----+-----+-----+-----+-----+-----+-----+
; | Velocity <KM/H> | +62 | +30 | +16 |  +8 |  +4 |  +2 |  +1 |  0  |
; +-----------------+-----+-----+-----+-----+-----+-----+-----+-----+
; +-----------------+-----+-----+-----+-----+-----+-----+-----+-----+
; | Velocity Index  |  8  |  9  |  10 |  11 |  12 |  13 |  14 |  15 |
; +-----------------+-----+-----+-----+-----+-----+-----+-----+-----+
; | Velocity <KM/H> |  0  |  -1 |  -2 |  -4 |  -8 | -16 | -30 | -62 |
; +-----------------+-----+-----+-----+-----+-----+-----+-----+-----+
;
; The index into the Zylon velocity table ZYLONVELTAB ($BF99) corresponding to
; the maximum velocity is called the "maximum velocity index". The following
; table shows the flight patterns, their maximum velocity indices, and their
; corresponding velocities:
;
; +----------------+------------------+------------------+
; | Flight Pattern | Maximum Velocity | Maximum Velocity |
; |                |      Index       |                  |
; +----------------+------------------+------------------+
; |       0        |         0        |    +62 <KM/H>    |
; |       0        |        15        |    -62 <KM/H>    |
; |       1        |         1        |    +30 <KM/H>    |
; |       1        |        14        |    -30 <KM/H>    |
; |       4        |         4        |     +4 <KM/H>    |
; |       4        |        11        |     -4 <KM/H>    |
; +----------------+------------------+------------------+
;
; Because flight pattern 0 produces the fastest-moving Zylon ships, which
; maneuver aggressively, it is called the "attack flight pattern".
;
; Each Zylon ship has a set of 3 maximum velocity indices, one for each of its
; velocity vector components.
;
; Each Zylon ship has also one more set of 3 velocity indices, called "Zylon
; velocity indices", one for each of its velocity vector components. They are
; used to pick the current values of the velocity vector components from the
; Zylon velocity table ZYLONVELTAB ($BF99).
;
; In order to maneuver Zylon ships this subroutine uses the concept of
; "milestone velocity indices". By using delay timers, called "Zylon timers",
; this subroutine gradually increases or decreases the Zylon velocity indices
; with every game loop iteration to eventually match the corresponding milestone
; velocity indices. By incrementing a Zylon velocity index a Zylon ship
; accelerates toward the negative direction of a coordinate axis. By
; decrementing a Zylon velocity index a Zylon ship accelerates toward the
; positive direction of a coordinate axis. If one milestone velocity index is
; matched or a "milestone timer" has counted down to 0, a new milestone velocity
; index is calculated and the matching of the current Zylon velocity indices
; with the new milestone velocity indices repeats.
;
; DETAILS
;
; For quick lookup, the following table lists the PLAYERs and what space objects
; they represent in this subroutine:
;
; +--------+---------------------------------+
; | PLAYER |           Represents            |
; +--------+---------------------------------+
; |    0   | Zylon Ship 0                    |
; |    1   | Zylon Ship 1                    |
; |    2   | Zylon Photon Torpedo, Meteor    |
; |    3   | Our starship's Photon Torpedo 0 |
; |    4   | Our starship's Photon Torpedo 1 |
; +--------+---------------------------------+
;
; This subroutine executes the following steps:
;
; (1)  Update the x and y velocity vector components of both of our starship's
;      photon torpedoes 0 and 1.
;
;      The x and y velocity vector components of both of our starship's photon
;      torpedoes 0 and 1 are only updated if they are tracking (homing in on) a
;      target.
;
;      To update the y-velocity vector components of both of our starship's
;      photon torpedoes 0 and 1 the PLAYER row number difference between the
;      PLAYER of tracked target space object and the current location of the
;      PLAYER of our starship's photon torpedo 0 is passed to subroutine
;      HOMINGVEL ($AECA). It returns the new y-velocity vector component value
;      for both of our starship's photon torpedoes in <KM/H>. If the target is
;      located below our starship's photon torpedo 0 a value of 0 <KM/H> is
;      used.
;
;      NOTE: The new y-velocity vector components depend only on the PLAYER row
;      number of our starship's photon torpedo 0.
;
;      To update the x-velocity vector components of both of our starship's
;      photon torpedoes, the above calculation is repeated for the PLAYER column
;      numbers of each of our starship's photon torpedoes 0 and 1.
;
; (2)  Make the Zylon ships follow the rotation of our starship.
;
;      If you rotate our starship away from Zylon ships they adjust their course
;      such that they reappear in our starship's view.
;
;      This is achieved by 4 Zylon timers, one for each of both Zylon ships'
;      current x and y Zylon velocity indices. The Zylon timers are decremented
;      every game loop iteration. If any of them reach a value of 0, the
;      corresponding Zylon velocity index is incremented or decremented
;      depending on the current joystick position.
;
;      For example, if the Zylon timer for the x-velocity of Zylon ship 0
;      reaches 0 and at the same time the joystick is pushed left then the
;      x-Zylon velocity index of this Zylon ship is incremented. This
;      accelerates the Zylon ship toward negative x-direction ("left"): The
;      Zylon ship follows our starship's rotation. This works in Aft view, too,
;      where the direction of change of the Zylon velocity index is reversed.
;      After setting the new Zylon velocity index, it is used to pick a new
;      Zylon timer value for this Zylon velocity index: 
;
;      +--------------------------------+----+----+----+----+----+----+----+----+
;      | Velocity Index                 |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |
;      +--------------------------------+----+----+----+----+----+----+----+----+
;      | Zylon Timer Value (Game Loops) |  0 |  2 |  4 |  6 |  8 | 10 | 12 | 14 |
;      +--------------------------------+----+----+----+----+----+----+----+----+
;      +--------------------------------+----+----+----+----+----+----+----+----+
;      | Velocity Index                 |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 |
;      +--------------------------------+----+----+----+----+----+----+----+----+
;      | Zylon Timer Value (Game Loops) | 14 | 12 | 10 |  8 |  6 |  4 |  2 |  0 |
;      +--------------------------------+----+----+----+----+----+----+----+----+
;
; (3)  Update the x and y velocity vector components of the single Zylon photon
;      torpedo.
;
;      If a Zylon photon torpedo is moving toward our starship then update its x
;      and y velocity vector components. They are picked from table
;      ZYLONHOMVELTAB ($BF85) and depend on the mission level. The signs of the
;      velocity vector components are always set such that the Zylon photon
;      torpedo is guided toward our starship. 
;
; (4)  Create a meteor?
;
;      If PLAYER2, the PLAYER to represent a meteor, is either initial or not
;      alive, then attempt in 7 out of 8 game loop iterations to create a new
;      meteor. 
;
;      With a probability of 2% (4:256) a new meteor is created: Its shape type
;      is set to METEOR, its position vector components to random coordinates in
;      subroutine INITPOSVEC ($B764), its lifetime to 60 game loop iterations,
;      and its velocity vector components (velocities) to x-velocity: 0 <KM/H>,
;      y-velocity: 0 <KM/H>, z-velocity: -8 <KM/H>. Then code execution returns.
;
; (5)  Toggle Zylon ship control.
;
;      Every other game loop iteration, the game takes control of and maneuvers
;      the other Zylon ship.
;
; (6)  Create new Zylon ship?
;
;      If the game-controlled Zylon ship is not alive, check if both Zylon ships
;      are not alive and this is an empty sector. If so, then attempt to create
;      a meteor. Otherwise create a new Zylon ship with infinite lifetime.
;      Randomly pick its shape type from table ZYLONSHAPTAB ($BF89) (ZYLON
;      BASESTAR, ZYLON CRUISER, or ZYLON FIGHTER) and its flight pattern from
;      table ZYLONFLPATTAB ($BF91) (attack flight pattern 0 is always picked in
;      a NOVICE mission). Then set the milestone timer to 1 game loop iteration
;      and the position vector of the Zylon ship to a position of at least
;      +28928 (+$71**) <KM> in front of our starship. The y-coordinate depends
;      on the value of VICINITYMASK ($C7). The x-coordinate is the sum of the
;      y-coordinate plus at least 4864..5119 ($13**) <KM>. Randomly choose the
;      signs of the x and y coordinates.
;
; (7)  Set the current flight pattern to attack flight pattern?
;
;      The current flight pattern of the Zylon ship will change to attack flight
;      pattern if it is close enough (z-coordinate < +8192 (+$20**) <KM>) and
;      one of the following conditions is met:
;
;      o   The Zylon ship is located behind our starship.
;
;      o   The shape of the Zylon ship is not initial and does not currently
;          appear as a blip in the Long-Range Scan view.
;
; (8)  Update the back-attack flag and the milestone velocity indices.
;
;      The milestone timer is decremented for the game-controlled Zylon ship. If
;      this timer reaches a value of 0 the following steps are executed:
;
;      o   The milestone timer is reset to a value of 120 game loop iterations.
;
;      o   The back-attack flag is updated. It determines if the game-controlled
;          Zylon ship not only attacks from the front of our starship but also
;          from the back. A back-attack takes place with a probability of 19%
;          (48:256) in WARRIOR or COMMANDER missions.
;
;      o   Course corrections are prepared for the game-controlled Zylon ship by
;          computing the new milestone vector indices, resulting in new velocity
;          vector components for this Zylon ship. The new milestone velocity
;          indices for each velocity vector component are randomly chosen
;          depending of the flight pattern. Recall that the Zylon velocity index
;          is changed gradually to match the milestone velocity index. It
;          corresponds to a maximum velocity vector component when using this
;          index to pick a velocity vector component from Zylon velocity table
;          ZYLONVELTAB ($BF99):
;
;          +----------------+----------------+------------------+
;          | Flight Pattern | New Milestone  | Maximum Velocity |
;          |                | Velocity Index | Vector Component |
;          +----------------+----------------+------------------+
;          |       0        |        0       |    +62 <KM/H>    |
;          |       0        |       15       |    -62 <KM/H>    |
;          |       1        |        1       |    +30 <KM/H>    |
;          |       1        |       14       |    -30 <KM/H>    |
;          |       4        |        4       |     +4 <KM/H>    |
;          |       4        |       11       |     -4 <KM/H>    |
;          +----------------+----------------+------------------+
;
; (9)  Update milestone velocity indices in attack flight pattern.
;
;      If a Zylon ship executes the attack flight pattern, its milestone
;      velocity indices are changed depending on the current location of the
;      Zylon ship as follows:
;
;      +--------------+-------------+----------------+------------+----------------+
;      | x-Coordinate |  Where on   |   Milestone    |  Velocity  | Zylon Ship     |
;      |              |   Screen    | Velocity Index |            | Accelerates... |
;      +--------------+-------------+----------------+------------+----------------+
;      | x <  0 <KM>  | left half   |       0        | +62 <KM/H> | to the right   |
;      | x >= 0 <KM>  | right half  |      15        | -62 <KM/H> | to the left    |
;      +--------------+-------------+----------------+------------+----------------+
;      +--------------+-------------+----------------+------------+----------------+
;      | y-Coordinate |  Where on   |   Milestone    |  Velocity  | Zylon Ship     |
;      |              |   Screen    | Velocity Index |            | Accelerates... |
;      +--------------+-------------+----------------+------------+----------------+
;      | y <  0 <KM>  | bottom half |       0        | +62 <KM/H> | up             |
;      | y >= 0 <KM>  | top half    |      15        | -62 <KM/H> | down           |
;      +--------------+-------------+----------------+------------+----------------+
;
;      Thus, with respect to its x and y coordinates, the Zylon ship oscillates
;      around the center of the Front or Aft view.
;
;      This is the behavior of the Zylon ship along the z-axis:
;
;      If the Zylon ship attacks from the front:
;
;      +--------------------------+----------------+------------+----------------+
;      |       z-Coordinate       |   Milestone    |  Velocity  | Zylon Ship     |
;      |                          | Velocity Index |            | Accelerates... |
;      +--------------------------+----------------+------------+----------------+
;      | z <  +2560 (+$0A00) <KM> |       0        | +62 <KM/H> | outbound       |
;      | z >= +2560 (+$0A00) <KM> |      15        | -62 <KM/H> | inbound        |
;      +--------------------------+----------------+------------+----------------+
;
;      In other words, the Zylon ship accelerates into positive z-direction
;      (outbound) up to a distance of +2560 (+$0A00) <KM>, then reverses its
;      course and returns back to our starship (inbound).
;
;      If the Zylon ship attacks from the back:
;
;      +--------------------------+----------------+------------+----------------+
;      |       z-Coordinate       |   Milestone    |  Velocity  | Zylon Ship     |
;      |                          | Velocity Index |            | Accelerates... |
;      +--------------------------+----------------+------------+----------------+
;      | z <  -2816 (-$F500) <KM> |       0        | +62 <KM/H> | inbound        |
;      | z >= -2816 (-$F500) <KM> |      15        | -62 <KM/H> | outbound       |
;      +--------------------------+----------------+------------+----------------+
;
;      In other words, the Zylon ship accelerates into negative z-direction
;      (outbound) up to a distance of -2816 (-$(0B00)) <KM>, then reverses its
;      course and returns back to our starship (inbound).
;
; (10) Change Zylon velocity index toward milestone velocity index.
;
;      Compare all 3 Zylon velocity indices of the game-controlled Zylon ship
;      with their corresponding milestone velocity indices. Increment or
;      decrement the former to better match the latter. Use the new Zylon
;      velocity indices to pick the current velocity values from Zylon velocity
;      table ZYLONVELTAB ($BF99). 
;
; (11) Launch a Zylon photon torpedo?
;
;      Prepare launching a Zylon photon torpedo if either of the following
;      conditions are met:
;
;      o   PLAYER2 is not used as a photon torpedo
;
;      o   The y-coordinate of the Zylon ship is in the range of -768..+767
;          (-$0300..+$2FF) <KM>. 
;
;      or if
;
;      o   The Zylon photon torpedo is not alive
;
;      o   The corresponding Zylon photon torpedo delay timer has reached a
;          value of 0
;
;      o   The y-coordinate of the Zylon ship is in the range of -768..+767
;          (-$0300..+$2FF) <KM>. 
;
;      At this point the z-velocity vector component of the Zylon photon torpedo
;      is preloaded with a value of -80 or +80 <KM/H> depending on the Zylon
;      ship being in front or behind of our starship, respectively. 
;
;      Launch a Zylon photon torpedo if both of the following conditions are
;      met:
;
;      o   The Zylon ship is in front or behind of our starship, with the
;          exception of a Zylon ship behind our starship in a NOVICE mission
;          (our starship will never be shot in the back in a NOVICE mission).
;
;      o   The z-coordinate of the Zylon ship (no matter if in front or behind
;          our starship) is closer than 8192 ($20**) <KM>.
;
;      Finally, the Zylon photon torpedo is launched with a lifetime of 62 game
;      loop iterations. Its position vector is copied from the launching Zylon
;      ship in subroutine CopyPositionVector ($ACAF). In addition, the Zylon ship is
;      earmarked for the tracking computer.

L_CTRLDZYLON    = $6A                   ; Index of currently game-controlled Zylon ship.
                                        ; Used values are:
                                        ;   0 -> Control Zylon ship 0
                                        ;   1 -> Control Zylon ship 1
NEG             = $80                   ; Negative sign bit for velocity vector component


;======================================
; Maneuver our starship's and Zylon
; photon torpedoes and Zylon ships
;======================================
MANEUVER        .proc
                lda WARPSTATE           ; Return if in starbase sector or hyperwarp engaged
                ora ISSTARBASESECT      ;
                bne PROJECTION._XIT     ;

;*** Update x and y velocity of both our starship's photon torpedoes 0 and 1 ***
                lda ISTRACKING          ; Skip this if ship's torpedoes not tracking a target
                beq _2                  ;

                ldx PLTRACKED           ; Load PLAYER index of tracked target space object

                sec                     ; Prep A := PLAYER row number of target...
                lda PL0ROWNEW,X         ; ...- PLAYER row number photon torpedo 0
                sbc PL3ROWNEW           ;
                bcc _1                  ; Skip if target above our starship's photon torpedo

                lda #0                  ; Prep A := 0
_1              jsr HOMINGVEL           ; Get y-velocity for homing photon torpedo 0 and 1

                sta PL3YVEL             ; Store y-velocity photon torpedo 0
                sta PL4YVEL             ; Store y-velocity photon torpedo 1

                sec                     ; Prep A := PLAYER column number of target...
                lda PL3COLUMN           ; ...- PLAYER column number of photon torpedo 0
                sbc PL0COLUMN,X         ;
                jsr HOMINGVEL           ; Get x-velocity for homing photon torpedo 0
                sta PL3XVEL             ; Store x-velocity of photon torpedo 0

                sec                     ; Prep A := PLAYER column number of target...
                lda PL4COLUMN           ; ...- PLAYER column number of photon torpedo 1
                sbc PL0COLUMN,X         ;
                jsr HOMINGVEL           ; Get x-velocity for homing photon torpedo 1
                sta PL4XVEL             ; Store x-velocity of photon torpedo 1

;*** Make Zylon ships follow rotation of our starship **************************
_2              ldx #3                  ; Loop over x and y velocity indices of both Zylons
_next1          dec ZYLONTIMX0,X        ; Decrement Zylon timer
                bpl _7                  ; Next timer if this one still counting down

                txa                     ; Prep joystick (x or y) value in -1, 0, +1
                lsr                     ;
                tay                     ;
                lda JOYSTICKX,Y         ;

                ldy SHIPVIEW            ; Skip if in Front view
                beq _3                  ;

                eor #$FF                ; Invert joystick value (when in Aft view)
                clc                     ; (two's-complement)
                adc #1                  ;

_3              clc                     ; Add joystick value to Zylon velocity index
                adc ZYLONVELINDX0,X     ;
                bpl _4                  ;

                lda #0                  ;
_4              cmp #16                 ; Limit new Zylon velocity index to 0..15 ...
                bcc _5                  ;

                lda #15                 ;
_5              sta ZYLONVELINDX0,X     ; ...and store new Zylon velocity index

                cmp #8                  ; Calc new Zylon timer value in 0, 2, ..., 14
                bcc _6                  ;

                eor #$0F                ;
_6              asl                     ;
                sta ZYLONTIMX0,X        ; ...and store new Zylon timer value

_7              dex                     ;
                bpl _next1              ; Next Zylon timer

;*** Update x and y velocity of single Zylon photon torpedo ********************
                lda PL2SHAPTYPE         ; Skip if PLAYER2 not PHOTON TORPEDO (shape type 0)
                bne _10                 ;

                ldy MISSIONLEVEL        ; Depending on mission level...
                lda ZYLONHOMVELTAB,Y    ; ...pick (initially negative) Zylon torpedo velocity

                ldx PL2YPOSHI           ; If photon torpedo in upper screen half (y >= 0)...
                bpl _8                  ; ...don't toggle velocity sign -> torpedo goes down

                and #$7F                ; ...toggle velocity sign       -> torpedo goes up
_8              sta PL2YVEL             ; Store new y-velocity of Zylon photon torpedo

                ora #NEG                ; Restore negative sign bit of velocity

                ldx PL2XPOSHI           ; If photon torpedo in right screen half (x >= 0)...
                bpl _9                  ; ...don't toggle velocity sign -> torpedo goes left

                and #$7F                ; ...toggle velocity sign       -> torpedo goes right
_9              sta PL2XVEL             ; Store new x-velocity of Zylon photon torpedo

;*** Create new meteor? ********************************************************
_10             lda COUNT256            ; Attempt meteor creation in 7 out of 8 game loops
                and #$03                ;
                beq _12                 ;

_next2          lda PL2SHAPOFF          ; If PLAYER2 shape is initial try to create a meteor
                beq _11                 ;

                lda PL2LIFE             ; Return if PLAYER2 alive
                bne _XIT1               ;

_11             .frsRandomByte          ; Return in 98% (252:256) (do not create meteor)
                cmp #4                  ;
                bcs _XIT1               ;

;*** Create new meteor! ********************************************************
                lda #SHAP_METEOR        ; PLAYER2 is METEOR (shape type 6)
                sta PL2SHAPTYPE         ;
                ldx #2                  ; Randomize position vector of meteor
                jsr INITPOSVEC          ;
                lda #60                 ; Meteor lifetime := 60 game loops
                sta PL2LIFE             ;
                lda #NEG|8              ; SUMMARY:
                sta PL2ZVEL             ; x-velocity :=  0 <KM/H>
                lda #0                  ; y-velocity :=  0 <KM/H>
                sta PL2COLUMN           ; z-velocity := -8 <KM/H>
                sta PL2XVEL             ;
                sta PL2YVEL             ; PLAYER2 column number := 0 (offscreen)
_XIT1           rts

;*** Toggle Zylon ship control *************************************************
_12             lda CTRLDZYLON          ; Toggle control to the other Zylon ship
                eor #$01                ;
                sta CTRLDZYLON          ;

;*** Create a new Zylon ship? **************************************************
                tax                     ; Save index of controlled Zylon ship
                lda PL0LIFE,X           ; Skip creating Zylon ship if its PLAYER still alive
                bne _14                 ;

                lda PL0LIFE             ; If both Zylon ships are not alive...
                ora PL1LIFE             ;
                and #$01                ;
                ldy CURRSECTOR          ; ...and this an empty sector...
                cmp GCMEMMAP,Y          ;
                bcs _next2              ; ...attempt to create meteor and return

;*** Create a new Zylon ship! **************************************************
                lda #255                ; Zylon ship lifetime := 255 game loops (infinite)
                sta PL0LIFE,X           ;

                .frsRandomByte          ; Pick a Zylon ship shape type (1 out of 8)
                and #$07                ;
                tay                     ;
                lda ZYLONSHAPTAB,Y      ;
                sta PL0SHAPTYPE,X       ;

                lda MISSIONLEVEL        ; Init Zylon's flight pattern (0 if NOVICE mission)
                beq _13                 ;

                lda ZYLONFLPATTAB,Y     ;
_13             sta ZYLONFLPAT0,X       ;

                lda #1                  ; Zylon ship's milestone timer := 1 game loop
                sta MILESTTIM0,X        ;

                sta ZPOSSIGN,X          ; Put Zylon ship in front of our starship
                .frsRandomByte          ;
                and VICINITYMASK        ; y-coordinate (high byte) := RND(0..VICINITYMASK)
                sta YPOSHI,X            ;
                adc #19                 ; x-coordinate (high byte) := y (high byte) + 19
                sta XPOSHI,X            ;
                ora #$71                ; z-coordinate (high byte) := >= +28928 (+$71**) <KM>
                sta ZPOSHI,X            ;
                jsr RNDINVXY            ; Randomly invert x and y coordinate of pos vector

;*** Set current flight pattern to attack flight pattern? **********************
_14             lda ZPOSHI,X            ; Skip if Zylon too distant (z >= +$20** <KM>)
                cmp #$20                ;
                bcs _16                 ;

                lda ZPOSSIGN,X          ; Set attack flight pattern if Zylon is behind
                beq _15                 ;

                lda PL0SHAPOFF,X        ; Skip if Zylon shape initial
                beq _16                 ;

                cmp #$29                ; Skip if Zylon shape is Long-Range Scan blip
                beq _16                 ;

_15             lda #0                  ; Set attack flight pattern
                sta ZYLONFLPAT0,X       ;

;*** Update back-attack flag and milestone velocity indices ********************
_16             dec MILESTTIM0,X        ; Skip if milestone timer still counting down
                bpl _19                 ;

                lda #120                ; Milestone timer := 120 game loops
                sta MILESTTIM0,X        ;

                lda MISSIONLEVEL        ; Back-attack flag := 1 in 19% (48:256) of...
                .frsRandomByteY         ; ...WARRIOR or COMMANDER missions
                cpy #48                 ; ...              := 0 otherwise
                bcc _17                 ;

                lsr                     ;
_17             lsr                     ;
                sta ISBACKATTACK0,X     ;

                                        ; Loop over all 3 milestone velocity indices
                lda ZYLONFLPAT0,X       ; Set new milestone velocity index:
_next3          bit frsRandomREG        ; If Zylon flight pattern is...
                bpl _18                 ; ...0 -> milestone velocity index := either 0 or 15

                eor #$0F                ; ...1 -> milestone velocity index := either 1 or 14
_18             sta MILESTVELINDZ0,X    ; ...4 -> milestone velocity index := either 4 or 11
                inx                     ;
                inx                     ;
                cpx #6                  ;
                bcc _next3              ; Next Zylon milestone velocity index

;*** Update milestone velocity indices in attack flight pattern ****************
                ldx CTRLDZYLON          ; Reload index of controlled Zylon ship

_19             lda ZYLONFLPAT0,X       ; Skip if not in attack flight pattern
                bne _25                 ;

                ldy CTRLDZYLON          ; Reload index of controlled Zylon ship

                                        ; Loop over all 3 milestone velocity indices
_next4          cpy #$31                ; Skip to handle x and y velocity index
                bcs _21                 ;
                                        ; SUMMARY:
                lda ISBACKATTACK0,Y     ; Handle z-velocity index:
                lsr                     ;
                lda ZPOSHI,Y            ; If Zylon attacks from front...
                bcs _20                 ; z <  $0A00 <KM> -> mil vel index := 0  (+62 <KM/H>)
                cmp #$0A                ; z >= $0A00 <KM> -> mil vel index := 15 (-62 <KM/H>)
                bcc _23                 ;
                bcs _21                 ; If Zylon attacks from back...
_20             cmp #$F5                ; z >= $F500 <KM> -> mil vel index := 15 (-62 <KM/H>)
                bcs _22                 ; z <  $F500 <KM> -> mil vel index := 0  (+62 <KM/H>)

_21             lda ZPOSSIGN,Y          ; Handle x and y velocity index:
                lsr                     ;
_22             lda #15                 ; x >= 0 <KM> -> mil vel index := 15 (-62 <KM/H>)
                bcs _24                 ; x <  0 <KM> -> mil vel index := 0  (+62 <KM/H>)

_23             lda #0                  ; y >= 0 <KM> -> mil vel index := 15 (-62 <KM/H>)
_24             sta MILESTVELINDZ0,X    ; y <  0 <KM> -> mil vel index := 0  (+62 <KM/H>)

                clc                     ; Adjust position vector component index
                tya                     ;
                adc #NUMSPCOBJ_ALL      ;
                tay                     ;

                inx                     ;
                inx                     ;
                cpx #6                  ;
                bcc _next4              ; Next milestone velocity index

;*** Acceleration: Change Zylon velocity index toward milestone velocity index *
                ldx CTRLDZYLON          ; Reload index of controlled Zylon ship
_25             ldy CTRLDZYLON          ; Reload index of controlled Zylon ship

                                        ; Loop over all 3 milestone velocity indices
_next5          lda ZYLONVELINDZ0,X     ; Compare Zylon velocity index with milestone index
                cmp MILESTVELINDZ0,X    ;
                beq _27                 ; Skip if equal
                bcs _26                 ;

                inc ZYLONVELINDZ0,X     ; Increm. Zylon velocity index if < milestone index
                bcc _27                 ;

_26             dec ZYLONVELINDZ0,X     ; Decrem. Zylon velocity index if >= milestone index

_27             stx L_CTRLDZYLON        ; Save index of controlled Zylon ship
                tax                     ;
                lda ZYLONVELTAB,X       ; Pick new velocity value by Zylon velocity index
                ldx L_CTRLDZYLON        ; Reload index of controlled Zylon ship
                sta ZVEL,Y              ; Store new velocity vector component of Zylon ship

                tya                     ; Next velocity vector component
                clc                     ;
                adc #NUMSPCOBJ_ALL      ;
                tay                     ;

                inx                     ;
                inx                     ;
                cpx #6                  ;
                bcc _next5              ; Next milestone velocity index

;*** Launch Zylon photon torpedo? **********************************************

;*** Check PLAYER2 shape and lifetime ******************************************
                ldx CTRLDZYLON          ; Reload index of controlled Zylon ship

                lda PL2SHAPTYPE         ; Skip if PLAYER2 not PHOTON TORPEDO (shape type 0)
                bne _28                 ;

                lda PL2LIFE             ; Return if Zylon photon torpedo still alive
                bne _XIT2               ;

                lda TORPEDODELAY        ; Count down Zylon photon torpedo delay timer...
                beq _28                 ; ...before launching next Zylon photon torpedo
                dec TORPEDODELAY        ;
_XIT2           rts

;*** Check y-coordinate of Zylon ship ******************************************
_28             clc                     ; Return if Zylon ship's y-coordinate not...
                lda YPOSHI,X            ; ...in -768..+767 (-$(0300)..+$2FF) <KM>.
                adc #2                  ;
                cmp #5                  ;
                bcs _XIT2               ;

;*** Set Zylon photon torpedo's z-velocity *************************************
                ldy #NEG|80             ; Prep Zylon torpedo's z-velocity := -80 <KM/H>

                lda ZPOSSIGN,X          ; Prep Zylon ship's sign of z-coordinate
                lsr                     ;
                lda ZPOSHI,X            ; Prep Zylon ship's z-coordinate
                bcs _29                 ; Skip if Zylon ship in front...
                eor #$FF                ; ...else invert loaded Zylon ship's z-coordinate

                ldy MISSIONLEVEL        ; Return (no torpedo from back) if NOVICE mission
                beq _XIT2               ;

                ldy #80                 ; Preload Zylon torpedo's z-velocity := +80 <KM/H>

;*** Is Zylon ship in range? ***************************************************
_29             cmp #$20                ; Return if Zylon ship too far...
                bcs _XIT2               ; ... (ABS(z-coordinate) > 8192 ($20**) <KM>)

                sty PL2ZVEL             ; Store Zylon photon torpedo's z-velocity

;*** Launch Zylon photon torpedo! **********************************************

                lda #0                  ; PLAYER2 is PHOTON TORPEDO (shape type 0)
                sta PL2SHAPTYPE         ;
                sta PL2COLUMN           ; Zylon torpedo PLAYER column number := 0 (offscreen)
                lda #62                 ;
                sta PL2LIFE             ; Zylon torpedo lifetime := 62 game loops

                ldx #2                  ; Prep source index for position vector copy
                ldy CTRLDZYLON          ; Prep destination index for position vector copy
                sty ZYLONATTACKER       ; Save Zylon ship index for tracking computer
                jmp CopyPositionVector  ; Copy position vector from Zylon ship to its torpedo

                .endproc
