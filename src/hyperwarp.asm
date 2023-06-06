
;*******************************************************************************
;*                                                                             *
;*                                  HYPERWARP                                  *
;*                                                                             *
;*                              Handle hyperwarp                               *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Handles the hyperwarp sequence, which transports our starship from one sector
; to another. It can be divided into four phases:
;
; (1)  ACCELERATION PHASE
;
;      The ACCELERATION PHASE is entered after the hyperwarp sequence has been
;      engaged in subroutine KEYBOARD ($AFFE) by pressing the 'H' key.
;
;      The Hyperwarp Target Marker appears and our starship begins to
;      accelerate. When our starship's velocity reaches 128 <KM/H> (the VELOCITY
;      readout of the Control Panel Display displays "50"), the STAR TRAIL phase
;      is entered. 
;
;      The Hyperwarp Target Marker is represented by a space object some fixed
;      distance away in front of our starship as PLAYER3. It has a lifetime of
;      144 game loop iterations and is tracked. Thus, tracking handling in
;      subroutine UPDATTCOMP ($A7BF) provides drawing the x and y lock-on
;      markers in the Attack Computer Display when the Hyperwarp Target Marker
;      is centered. 
;
;      A temporary arrival location on the Galactic Chart was saved when the
;      hyperwarp was engaged in subroutine KEYBOARD ($AFFE). During the
;      ACCELERATION PHASE (and the subsequent STAR TRAIL PHASE) this location is
;      constantly updated depending on how much the Hyperwarp Target Marker
;      veers off its center position.
;
;      The actual arrival hyperwarp marker row and column numbers on the
;      Galactic Chart are the sum of the temporary arrival hyperwarp marker row
;      and column numbers stored when engaging the hyperwarp in subroutine
;      KEYBOARD ($AFFE) and the number of Player/Missile (PM) pixels which the
;      Hyperwarp Target Marker is off-center vertically and horizontally,
;      respectively, at the end of the STAR TRAIL PHASE.
;
;      NOTE: The used vertical center value of 119 PM pixels is the PM pixel row
;      number of the top edge of the centered Hyperwarp Target Marker (from top
;      to bottom: 8 PM pixels to the start of Display List + 16 PM pixels blank
;      lines + 100 PM pixels to the vertical PLAYFIELD center - 5 PM pixels
;      relative offset of the Hyperwarp Target Marker's shape center to the
;      shape's top edge = 119 PM pixels). Recall also that PLAYERs at
;      single-line resolution have PM pixels that are half as high as they are
;      wide.
;
;      NOTE: The used horizontal center value of 125 PM pixels is the PM pixel
;      row number of the left edge of the centered Hyperwarp Target Marker (from
;      left to right: 127 PM pixels to the PLAYFIELD center - 3 PM pixels
;      relative offset of the Hyperwarp Target Marker's shape center to the
;      shape's left edge = 125 PM pixels).
;
;      If during the ACCELERATION PHASE (and the subsequent STAR TRAIL PHASE)
;      you switch the Front view to another view, the Hyperwarp Target Marker
;      changes to a random position which results in arriving at a random
;      destination sector.
;
;      During the ACCELERATION PHASE (and the subsequent STAR TRAIL PHASE) in
;      all but NOVICE missions, the Hyperwarp Target Marker veers off with
;      random velocity in x and y direction, which is changed during 6% of game
;      loop iterations. Table VEERMASKTAB ($BED7) limits the maximum veer-off
;      velocity depending on the mission level:
;
;      +-----------+-----------------------------+
;      |  Mission  |      Veer-Off Velocity      |
;      +-----------+-----------------------------+
;      | NOVICE    |                   0  <KM/H> |
;      | PILOT     |  -63..-16, +16..+63  <KM/H> |
;      | WARRIOR   |  -95..-16, +16..+95  <KM/H> |
;      | COMMANDER | -127..-16, +16..+127 <KM/H> |
;      +-----------+-----------------------------+
;
; (2)  STAR TRAIL PHASE
;
;      When our starship's velocity reaches a velocity of 128 <KM/H> (the
;      VELOCITY readout of the Control Panel Display displays "50"), in addition
;      to all effects of the ACCELERATION PHASE, multiple star trails begin to
;      appear while our starship continues to accelerate. Each star trail is
;      initialized in subroutine INITTRAIL ($A9B4).
;
; (3)  HYPERSPACE PHASE
;
;      When our starship's velocity reaches a velocity of 254 <KM/H> (the
;      VELOCITY readout of the Control Panel Display displays "99") our starship
;      enters the HYPERSPACE PHASE (the VELOCITY readout of the Control Panel
;      Display displays the infinity symbol).
;
;      During the first pass of the HYPERSPACE PHASE the hyperwarp state is set
;      to HYPERSPACE. This makes the stars and the Hyperwarp Target Marker
;      disappear in GAMELOOP ($A1F3). Then, the beeper sound pattern HYPERWARP
;      TRANSIT is played in subroutine BEEP ($B3A6), the hyperwarp distance and
;      required hyperwarp energy is calculated in subroutine CALCWARP ($B1A7),
;      and the title line is preloaded with "HYPERSPACE". Code execution returns
;      via calling subroutine CLEANUPWARP ($A98D) where game variables are
;      already initialized to their post-hyperwarp values.
;
;      During subsequent passes of the HYPERSPACE PHASE, the calculated
;      hyperwarp energy is decremented in chunks of 10 energy units. Code
;      execution returns via calling subroutine DecreaseEnergy ($B86F), which
;      decrements our starship's energy. After the calculated hyperwarp energy
;      is spent the DECELERATION PHASE is entered.
;
; (4)  DECELERATION PHASE
;
;      The title line flashes "HYPERWARP COMPLETE", the star field reappears and
;      our starship decelerates to a stop. The Engines and the hyperwarp are
;      disengaged and stopped in subroutine ENDWARP ($A987), the arrival
;      coordinates on the Galactic Chart are initialized, as well as the
;      vicinity mask.
;
;      The vicinity mask limits the position vector components (coordinates) of
;      space objects in the arrival sector relative to our starship. The
;      vicinity mask is picked from table VICINITYMASKTAB ($BFB3) by an index
;      calculated by the arrival y-coordinate modulo 8: The more you have placed
;      the arrival hyperwarp marker in the vertical center of a sector on the
;      Galactic Chart, the closer space objects in this sector will be to our
;      starship. For example, if you placed the arrival hyperwarp marker exactly
;      in the vertical middle of the sector the index will be 3, thus the space
;      objects inside the arrival sector will be in the vicinity of <= 4095 <KM>
;      of our starship. The following table lists the possible coordinates
;      depending on the calculated index:
;
;      +-------+-----------------------+
;      | Index |    ABS(Coordinate)    |
;      +-------+-----------------------+
;      |   0   | <= 65535 ($FF**) <KM> |
;      |   1   | <= 65535 ($FF**) <KM> |
;      |   2   | <= 16383 ($3F**) <KM> |
;      |   3   | <=  4095 ($0F**) <KM> |
;      |   4   | <= 16383 ($3F**) <KM> |
;      |   5   | <= 32767 ($7F**) <KM> |
;      |   6   | <= 65535 ($FF**) <KM> |
;      |   7   | <= 65535 ($FF**) <KM> |
;      +-------+-----------------------+
;
;      If there is a starbase in the arrival sector, its x and y coordinates are
;      initialized to random values within the interval defined by the vicinity
;      mask by using subroutine RNDINVXY ($B7BE). Its z-coordinate is forced to
;      a value >= +$71** (+28928) <KM>. Its velocity vector components are set
;      to 0 <KM/H>. 
;
;      If there are Zylon ships in the arrival sector then a red alert is
;      initialized by setting the red alert lifetime to 255 game loop
;      iterations, playing the beeper sound pattern RED ALERT in subroutine BEEP
;      ($B3A6) and setting the title phrase to "RED ALERT".


;======================================
; Handle hyperwarp
;======================================
HYPERWARP       .proc
                ldy WARPSTATE           ; Return if hyperwarp not engaged
                beq _XIT1               ;

                lda VELOCITYLO          ; If velocity >= 254 <KM/H> skip to HYPERSPACE PHASE
                cmp #254                ;
                bcs _4                  ;

                cmp #128                ; If velocity < 128 <KM/H> skip to ACCELERATION PHASE
                bcc _1                  ;

;*** STAR TRAIL PHASE **********************************************************
                jsr INITTRAIL           ; Init star trail

;*** ACCELERATION PHASE ********************************************************
_1              lda #3                  ; Track Hyperwarp Target Marker (PLAYER3)
                sta TRACKDIGIT          ;

                lda #SHAP_HYPERWARP     ; PLAYER3 is HYPERWARP TARGET MARKER (shape type 9)
                sta PL3SHAPTYPE         ;
                sta PL3LIFE             ; PLAYER3 lifetime := 144 game loops

                lda #$1F                ; PLAYER3 z-coordinate := $1F** (7936..8191) <KM>
                sta PL3ZPOSHI           ;

                sec                     ; New arrival hyperwarp marker row number is...
                lda PL3ROWNEW           ; WARPARRVROW := WARPTEMPROW + PL3ROWNEW...
                sbc #119                ; ... - 119 PM pixels (top edge of centered...
                clc                     ; ...Hyperwarp Target Marker)
                adc WARPTEMPROW         ;
                and #$7F                ; Limit WARPARRVROW to 0..127
                sta WARPARRVROW         ;

                sec                     ; New arrival hyperwarp marker column number is...
                lda PL3COLUMN           ; WARPARRVCOLUMN := WARPTEMPCOLUMN + PL3COLUMN...
                sbc #125                ; ... - 125 PM pixels (left edge of centered...
                clc                     ; ...Hyperwarp Target Marker)
                adc WARPTEMPCOLUMN      ;
                and #$7F                ; Limit WARPARRVCOLUMN to 0..127
                sta WARPARRVCOLUMN      ;

                lda MISSIONLEVEL        ; Skip if NOVICE mission
                beq _3                  ;

                .randomByte             ; Prep random number
                ldy SHIPVIEW            ; Skip if in Front view
                beq _2                  ;

                sta PL3COLUMN           ; Randomize PM pixel row and column number...
                sta PL3ROWNEW           ; ...of Hyperwarp Target Marker

_2              cmp #16                 ; Return in 94% (240:256) of game loops
                bcs _XIT1               ;

;*** Veer off Hyperwarp Target Marker and return *******************************
_3              .randomByte             ; Prep random x-velocity of Hyperwarp Target Marker
                ora #$10                ; Velocity value >= 16 <KM/H>
                and VEERMASK            ; Limit velocity value by mission level
                sta PL3XVEL             ; PLAYER3 x-velocity := velocity value

                .randomByte             ; Prep random y-velocity of Hyperwarp Target Marker
                ora #$10                ; Velocity value >= 16 <KM/H>
                and VEERMASK            ; Limit velocity value by mission level
                sta PL3YVEL             ; PLAYER3 y-velocity := velocity value
_XIT1           rts

;*** HYPERSPACE PHASE **********************************************************
_4              tya                     ; Skip if already in HYPERSPACE PHASE
                bmi _5                  ;

;*** HYPERSPACE PHASE (First pass) *********************************************
                lda #$FF                ; Set hyperwarp state to HYPERSPACE PHASE
                sta WARPSTATE           ;

                ldx #$00                ; Play beeper sound pattern HYPERWARP TRANSIT
                jsr BEEP                ;

                jsr CALCWARP            ; Calc hyperwarp energy

                ldy #$1B                ; Prep title phrase "HYPERSPACE"
                jmp CLEANUPWARP         ; Return via CLEANUPWARP

;*** HYPERSPACE PHASE (Second and later passes) ********************************
_5              dec WARPENERGY          ; Decrement energy in chunks of 10 energy units
                beq _6                  ; Skip to DECELERATION PHASE if hyperwarp energy zero

                ldx #2                  ; ENERGY := ENERGY - 10 and return
                jmp DecreaseEnergy      ;

;*** DECELERATION PHASE ********************************************************
_6              ldy #$19                ; Prep title phrase "HYPERWARP COMPLETE"
                jsr ENDWARP             ; Stop our starship

                lda WARPARRVCOLUMN      ; Make the arrival hyperwarp marker column number...
                sta WARPDEPRCOLUMN      ; ...the departure hyperwarp marker column number
                lda WARPARRVROW         ; Make the arrival hyperwarp marker row number...
                sta WARPDEPRROW         ; ...the departure hyperwarp marker row number

                lsr                     ; B3..1 of arrival hyperwarp marker row number...
                and #$07                ; ...pick vicinity mask
                tax                     ;
                lda VICINITYMASKTAB,X   ;
                sta VICINITYMASK        ; Store vicinity mask (limits space obj coordinates)

                ldy ARRVSECTOR          ; Make the arrival sector the current sector
                sty CURRSECTOR          ;

;*** Init starbase in arrival sector *******************************************
                lda #0                  ; Clear starbase-in-sector flag
                sta ISSTARBASESECT      ;

                ldx GCMEMMAP,Y          ; Skip if no starbase in arrival sector
                bpl _7                  ;

                lda #$FF                ; Set starbase-in-sector flag
                sta ISSTARBASESECT      ;

;*** Set position vector and velocity vector of starbase ***********************
                ldy #0                  ;
_next1          lda #0                  ; Loop over all coordinates of starbase
                sta PL2ZVEL,Y           ; Starbase velocity vector component := 0 <KM/H>
                lda #1                  ;
                sta PL2ZPOSSIGN,Y       ; Starbase coordinate sign := + (positive)
                .randomByte             ; Prep random number...
                and VICINITYMASK        ; ...limit number range by vicinity mask, then...
                sta PL2ZPOSHI,Y         ; ...store in starbase coordinate (high byte)

                tya                     ;
                clc                     ;
                adc #NUMSPCOBJ_ALL      ;
                tay                     ;
                cmp #NUMSPCOBJ_ALL*3    ;
                bcc _next1              ; Next starbase coordinate

                lda PL2ZPOSHI           ; Force starbase z-coordinate >= +$71** <KM>
                ora #$71                ;
                sta PL2ZPOSHI           ;
                ldx #2                  ; Randomly invert starbase x and y coordinates...
                jmp RNDINVXY            ; ...and return

;*** Flash red alert if Zylon sector entered ***********************************
_7              beq _XIT                ; Skip if no Zylon ships in sector

                lda #255                ; Red alert lifetime := 255 game loops
                sta REDALERTLIFE        ;

                ldx #$06                ; Play beeper sound pattern RED ALERT
                jsr BEEP                ;

                ldy #$75                ; Set title phrase "RED ALERT"
                jsr SETTITLE            ;

_XIT            rts
                .endproc


;*******************************************************************************
;*                                                                             *
;*                                  ABORTWARP                                  *
;*                                                                             *
;*                               Abort hyperwarp                               *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Aborts hyperwarp.
;
; This subroutine is entered from subroutine KEYBOARD ($AFFE). It subtracts 100
; energy units for aborting the hyperwarp and preloads the title phrase with
; "HYPERWARP ABORTED". Code execution continues into subroutine ENDWARP
; ($A987). 


;--------------------------------------
; Abort hyperwarp
;--------------------------------------
ABORTWARP       .proc
                ldx #1                  ; ENERGY := ENERGY - 100 after hyperwarp abort
                jsr DecreaseEnergy      ;

                ldy #$17                ; Prep title phrase "HYPERWARP ABORTED"

                .endproc

                ;[fall-through]


;*******************************************************************************
;*                                                                             *
;*                                   ENDWARP                                   *
;*                                                                             *
;*                                End hyperwarp                                *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Ends hyperwarp.
;
; This subroutine stops our starship's Engines and resets the hyperwarp state.
; Code execution continues into subroutine CLEANUPWARP ($A98D).


;======================================
; End hyperwarp
;======================================
ENDWARP         .proc
                lda #0                  ; Stop Engines
                sta NEWVELOCITY         ;
                sta WARPSTATE           ; Disengage hyperwarp

                .endproc

                ;[fall-through]


;*******************************************************************************
;*                                                                             *
;*                                 CLEANUPWARP                                 *
;*                                                                             *
;*                        Clean up hyperwarp variables                         *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Cleans up after a hyperwarp.
;
; This subroutine restores many hyperwarp related variables to their
; post-hyperwarp values: The number of used space objects is set to the regular
; value of 16 (5 PLAYER space objects + 12 PLAYFIELD space objects (stars),
; counted 0..16), our starship's velocity (high byte) is cleared as well as the
; explosion lifetime, the hit badness, the PLAYER3 shape type (Hyperwarp Target
; Marker), the Engines energy drain rate, and the lifetimes of the PLAYERs. The
; docking state is reset as well as the tracking digit. The title phrase is
; updated with either "HYPERSPACE" or "HYPERWARP ABORTED".
;
; INPUT
;
;   Y = Title phrase offset. Used values are: 
;     $17 -> "HYPERWARP ABORTED"
;     $1B -> "HYPERSPACE"


;--------------------------------------
; Clean up hyperwarp variables
;--------------------------------------
CLEANUPWARP     .proc
                lda #NUMSPCOBJ_NORM-1   ; Set normal number of space objects
                sta MAXSPCOBJIND        ; (5 PLAYER spc objs + 12 PLAYFIELD spc objs (stars))

                lda #0                  ;
                sta VELOCITYHI          ; Turn off hyperwarp velocity
                sta EXPLLIFE            ; Explosion lifetime := 0 game loops
                sta HITBADNESS          ; HITBADNESS := NO HIT
                sta PL3SHAPTYPE         ; Clear PLAYER3 shape type
                sta DRAINENGINES        ; Clear Engines energy drain rate
                cpy #$17                ; Skip if hyperwarp was aborted
                beq _1                  ;

                sta PL0LIFE             ; Zylon ship 0 lifetime := 0 game loops
                sta PL1LIFE             ; Zylon ship 1 lifetime := 0 game loops

_1              sta PL2LIFE             ; Zylon photon torpedo lifetime := 0 game loops
                sta PL3LIFE             ; Hyperwarp Target Marker lifetime := 0 game loops
                sta PL4LIFE             ; Photon torpedo 1 lifetime := 0  game loops
                sta DOCKSTATE           ; DOCKSTATE := NO DOCKING
                sta TRACKDIGIT          ; Clear index of tracked space object
                jmp SETTITLE            ; Set title phrase and return

                .endproc


;*******************************************************************************
;*                                                                             *
;*                                  INITTRAIL                                  *
;*                                                                             *
;*         Initialize star trail during STAR TRAIL PHASE of hyperwarp          *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; BACKGROUND
;
; Star trails are displayed during the STAR TRAIL PHASE, that is, after the
; ACCELERATION PHASE and before the HYPERSPACE PHASE of the hyperwarp. 
;
; A star trail is formed by 6 stars represented by 6 PLAYFIELD space objects
; with continuous position vector indices in 17..48 (indices are wrapped around
; when greater than 48). Between the creations of two star trails there is delay
; of 4 game loop iterations.
;
; DETAILS
;
; This subroutine first decrements this star trail creation delay, returning if
; the delay is still counting down. If the delay falls below 0 then it continues
; accelerating our starship's velocity toward hyperwarp speed and then creates a
; new star trail:
;
; First, it raises the maximum index of used space objects to 48 (increasing the
; number of displayed space objects to 49), resets the star trail creation delay
; to 4 game loop iterations, and then forms a new star trail of 6 stars
; represented by 6 PLAYFIELD space objects. The x and y coordinates for all 6
; stars are the same, picked randomly from tables WARPSTARXTAB ($BB3A) and
; WARPSTARYTAB ($BB3E), respectively, with their signs changed randomly. Their
; z-coordinates are computed in increasing depth from at least +4608 (+$12**)
; <KM> in intervals of +80 (+$0050) <KM>. Their velocity vector components are
; set to 0 <KM/H>.

L_RANGE         = $68                   ; z-coordinate of star in star trail (16-bit value)
L_TRAILCNT      = $6E                   ; Star's index in star trail. Used values are: 0..5.


;======================================
; Initialize star trail during STAR
; TRAIL PHASE of hyperwarp
;======================================
INITTRAIL       .proc
                dec TRAILDELAY          ; Decrement star trail delay
                bpl _XIT                ; Return if delay still counting

                lda #1                  ; Turn on hyperwarp velocity
                sta VELOCITYHI          ;

                lda #NUMSPCOBJ_ALL-1    ; Max index of space objects (for star trail stars)
                sta MAXSPCOBJIND        ;

                lda #3                  ; Star trail delay := 3(+1) game loops
                sta TRAILDELAY          ;

                ldx TRAILIND            ; Next avail. space obj index for star of star trail

                lda #$12                ; Star z-coordinate := >= +$12** (+4608) <KM>
                sta L_RANGE+1           ;

                .randomByte             ; Calc random index to pick initial star coordinates
                and #$03                ;
                tay                     ;
                lda WARPSTARXTAB,Y      ; Pick x-coordinate (high byte) of star from table
                sta XPOSHI,X            ;
                lda WARPSTARYTAB,Y      ;
                sta YPOSHI,X            ; Pick y-coordinate (high byte) of star from table
                jsr RNDINVXY            ; Randomize signs of x and y coordinates of star

                txa                     ; Save space object index
                tay                     ;
                lda #5                  ; Loop over 5(+1) stars that form the star trail
                sta L_TRAILCNT          ; Store star counter of star trail

_next1          clc                     ; Place stars in z-coordinate intervals of +80 <KM>
                lda L_RANGE             ;
                adc #80                 ;
                sta L_RANGE             ;
                sta ZPOSLO,X            ;
                lda L_RANGE+1           ;
                adc #0                  ;
                sta L_RANGE+1           ;
                sta ZPOSHI,X            ;

                lda #0                  ; Star's velocity vector components := 0 <KM/H>
                sta ZVEL,X              ;
                sta XVEL,X              ;
                sta YVEL,X              ;
                lda #1                  ; Star's z-coordinate sign := + (= ahead of starship)
                sta ZPOSSIGN,X          ;

                lda #99                 ; Init pixel row and column numbers to magic...
                sta PIXELROWNEW,X       ; ...offscreen value (triggers automatic recalc in...
                sta PIXELCOLUMN,X       ; ...GAMELOOP's calls to SCREENCOLUMN and SCREENROW)

                jsr CopyPositionXY           ; Copy x and y coordinate from previous star in trail

                dex                     ; Decrement space object index to next star
                cpx #NUMSPCOBJ_NORM     ; If index reaches minimum value...
                bcs _1                  ;
                ldx #NUMSPCOBJ_ALL-1    ; ...wrap-around to maximum space object index
_1              dec L_TRAILCNT          ;
                bpl _next1              ; Next star of star trail

                stx TRAILIND            ; Save space object index of star trail's last star
_XIT            rts
                .endproc
