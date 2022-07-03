;*******************************************************************************
;*                                                                             *
;*                                  GAMELOOP                                   *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; The game loop is the main part of the game. It is basically an infinite loop
; that collects input, computes the game state, and updates the display. It
; executes the following steps:
;
; (1)  Synchronize the start of the game loop with the vertical blank phase of
;      the TV beam, which flagged by the Vertical Blank Interrupt handler
;      VBIHNDLR ($A6D1). This prevents screen flicker while the PLAYFIELD is
;      redrawn at the beginning of the game loop, because during the vertical
;      blank phase the TV beam is turned off and nothing is rendered on the TV
;      display. 
;
; (2)  Erase all PLAYFIELD space objects (stars, explosion fragments) from the
;      PLAYFIELD that were drawn in the previous game loop iteration.
;
; (3)  Draw the updated PLAYFIELD space objects (stars, explosion fragments)
;      into the PLAYFIELD (skip this if in hyperspace).
;
; (4)  If the idle counter has reached its trigger value then clear the center
;      of the PLAYFIELD, an 8 x 2 pixel rectangle with a top-left position at
;      pixel column number 76 and pixel row number 49 (?).
;
; (5)  Clear all PLAYER shapes.
;
; (6)  Update the vertical position of all PLAYERs and update all PLAYER shapes.
;
; (7)  Update the horizontal position of all PLAYERs.
;
; (8)  Rotate the position vector of all space objects horizontally and
;      vertically, according to the saved joystick position (skip this if in
;      Galactic Chart view) using subroutine Rotate ($B69B).
;
; (9)  Move our starship forward in space. Our starship is always located at the
;      center of the game's 3D coordinate system, so all space objects are moved
;      along the z-axis toward our starship by subtracting a displacement from
;      their z-coordinate. The amount of the displacement depends on our
;      starship's velocity.
;
;      BUG (at $A3C1): This operation is not applied to Photon torpedoes (?).
;      Suggested fix: Remove lda PL0SHAPTYPE,X and beq SKIP011. 
;
; (10) Add the proper velocity vector of all space objects to their position
;      vector (except for stars, which do not have any proper motion).
;
;      BUG (at $A419): The correct maximum loop index is NUMSPCOBJ_ALL*3 = 147
;      instead of 144. Suggested fix: Replace cmp #144 with cmp #147.
;
; (11) Correct the position vector components (coordinates) of all PLAYER space
;      objects if they have over- or underflowed during the calculations of the
;      previous steps.
;
; (12) Calculate the perspective projection of the position vectors of all space
;      objects and from that their pixel row and column number (applies to Front
;      and Aft view) using subroutines PROJECTION ($AA21), ScreenColumn ($B6FB),
;      and ScreenRow ($B71E). If a space object (star, explosion fragment) moved
;      offscreen then a new space object is automatically created in subroutine
;      ScreenColumn ($B6FB).
;
; (13) Handle hyperwarp marker selection in the Galactic Chart view in
;      subroutine SELECTWARP ($B162).
;
; (14) If in Long-Range Scan view, compute the pixel column number and the pixel
;      row number of all PLAYFIELD space objects (stars, explosion fragments) on
;      the plane established by the z and x axis of the 3D coordinate system
;      using subroutines ScreenColumn ($B6FB) and ScreenRow ($B71E). Our
;      starship's shape is drawn using subroutine DRAWLINES ($A76F). If the
;      Long-Range Scan is OK then PLAYFIELD space object pixel numbers are
;      computed and drawn. This is skipped if the Long-Range Scan is destroyed. 
;
; (15) Update all PLAYER shapes, heights, and colors (see detailed description
;      below).
;
; (16) Flash a red alert when leaving hyperspace into a sector containing Zylon
;      ships by setting appropriate colors to PLAYFIELD2 and BACKGROUND.
;
; (17) Update the color of all PLAYFIELD space objects (stars, explosion
;      fragments). The color calculation is similar to that of the PLAYER color
;      calculation in (15). It also computes a range index and uses the same
;      color lookup table FOURCOLORPIXEL ($BA90). If a star in the Aft view
;      became too distant (z-coordinate < -$F000 (-4096) <KM>) its position is
;      re-initialized in subroutine InitPosVec ($B764).
;
; (18) If in demo mode skip input handling and jump directly to function key
;      handling (28).
;
; (19) Handle keyboard input in subroutine KEYBOARD ($AFFE).
;
; (20) Handle joystick input. Store the current joystick directions in JOYSTICKX
;      ($C8) and JOYSTICKY ($C9).
;
; (21) Check if our starship's photon torpedoes have hit a target in subroutine
;      Collision ($AF3D). This subroutine triggers a game over if all Zylon
;      ships have been destroyed.
;
; (22) Handle the joystick trigger in subroutine TRIGGER ($AE29).
;
; (23) Handle the Attack Computer and Tracking Computer. If the Attack Computer
;      is neither destroyed nor switched off then execute the following steps:
;
;      o   Update the Attack Computer Display's blip and lock-on markers in
;          subroutine UPDATTCOMP ($A7BF) (if in Front view).
;
;      o   Update the tracking index of the currently tracked PLAYER space
;          object. If a Zylon ship is tracked, then make sure to always track
;          the Zylon ship that launched the last Zylon photon torpedo. If this
;          Zylon ship is not alive then track the other Zylon ship - if alive.
;
;      o   If the Tracking Computer is on then switch to the view that shows the
;          tracked PLAYER space object by emulating pressing the 'F' (Front
;          view) or 'A' (Aft view) key (only if in Front or Aft view).
;
; (24) Handle docking at a starbase in subroutine Docking ($ACE6).
;
; (25) Handle maneuvering both of our starship's photon torpedoes, the single
;      Zylon photon torpedo, and the attacking Zylon ships in subroutine
;      MANEUVER ($AA79). This subroutine also automatically creates meteors and
;      new Zylon ships.
;
; (26) Check if our starship was hit by a Zylon photon torpedo (skip this if in
;      a starbase sector): Its x, y, and z coordinates must be within a range of
;      -($0100)..+$00FF (-256..+255) <KM> of our starship.
;
; (27) If our starship was hit then execute the following steps:
;
;      o   Damage or destroy one of our starship's subsystems in subroutine
;          Damage ($AEE1).
;
;      o   Trigger an explosion in subroutine InitExpl ($AC6B), 
;
;      o   Store the severity of the hit.
;
;      o   End the lifetime of the Zylon photon torpedo.
;
;      o   Subtract 100 energy units for being hit by the Zylon photon torpedo
;          in subroutine DECENERGY ($B86F). 
;
;      o   Trigger the noise sound pattern SHIELD EXPLOSION in subroutine NOISE
;          ($AEA8). 
;
;      If the Shields were down during the hit, our starship is destroyed.
;      Execute the following steps:
;
;      o   Switch to Front view.
;
;      o   Flash the title phrase "SHIP DESTROYED BY ZYLON FIRE".
;
;      o   Add the mission bonus to the internal game score in subroutine
;          GAMEOVER ($B10A).
;
;      o   Hide the Control Panel Display (bottom text window) in subroutine
;          ModDLST ($ADF1).
;
;      o   Clear the PLAYFIELD in subroutine ClrPlayfield ($AE0D).
;
;      o   Enable the STARSHIP EXPLOSION noise. 
;
; (28) Handle the function keys START and SELECT. If SELECT has been pressed
;      cycle through the next of the 4 mission levels. If either START or SELECT
;      have been pressed, reset the idle counter, then jump to the corresponding
;      game initialization subroutines INITSTART ($A15E) or INITSELECT ($A15A),
;      respectively. 
;
; (29) Update the Control Panel Display in subroutine UPDPANEL ($B804).
;
; (30) Handle hyperwarp in subroutine HYPERWARP ($A89B).
;
; (31) Update the text in the title line in subroutine UPDTITLE ($B216).
;
; (32) Move Zylon units, decrease lifetime of photon torpedoes, elapse game
;      time, etc. in subroutine FLUSHGAMELOOP ($B4E4). This subroutine also
;      triggers a game over if our starship's energy is zero.
;
; (33) Jump back to the start of the game loop for the next game loop iteration.

L_HEIGHTCNT     = $6A                   ; Height counter during copying a PLAYER shape
L_ZPOSOFF       = $6E                   ; Offset to z-coordinate
L_VELOCITYHI    = $6B                   ; Velocity vector component (high byte)
L_VECCOMPIND    = $6A                   ; Position vector component index. Used values are:
                                        ;   0 -> z-component
                                        ;   1 -> x-component
                                        ;   2 -> y-component
L_RANGEINDEX    = $6A                   ; Range index for space object, computed from the
                                        ; distance to our starship. Used to pick the shape
                                        ; cell index of the PLAYERs shape data and shape
                                        ; height. Used values are: 0..15.
L_FOURCOLORPIX  = $6A                   ; 1-byte bit pattern for 4 pixels of same color
L_COLORMASK     = $6B                   ; Color/brightness to modify PLAYER color

;*** (1) Synchronize game loop with execution of VBI ***************************
GAMELOOP        lda ISVBISYNC           ; Wait for execution of VBI
                beq GAMELOOP            ;

                lda #0                  ; VBI is executed, clear VBI sync flag
                sta ISVBISYNC           ;

;*** (2) Erase PLAYFIELD space objects (stars, explosion fragments) ************
                lda OLDMAXSPCOBJIND     ; Skip if no space objects in use
                beq SKIP002             ;

                ldx #NUMSPCOBJ_PL-1     ; Loop over all PLAYFIELD space objs (X index > 4)
LOOP002         inx                     ;
                ldy PIXELROW,X          ; Load pixel row number of PLAYFIELD space object

                lda PFMEMROWLO,Y        ; Point MEMPTR to start of pixel's row...
                sta MEMPTR              ; ...in PLAYFIELD memory
                lda PFMEMROWHI,Y        ;
                sta MEMPTR+1            ;

                ldy PIXELBYTEOFF,X      ; Get within-row-offset to byte with space obj pixel
                lda PIXELSAVE,X         ; Load saved byte
                sta (MEMPTR),Y          ; Restore byte of PLAYFIELD memory

                cpx OLDMAXSPCOBJIND     ;
                bcc LOOP002             ; Next PLAYFIELD space object

                lda #0                  ; Clear number of space objects
                sta OLDMAXSPCOBJIND     ;

;*** (3) Draw PLAYFIELD space objects (stars, explosion fragments) *************
SKIP002         lda WARPSTATE           ; Skip during hyperspace
                bmi SKIP003             ;

                ldx MAXSPCOBJIND        ; Update number of space objects
                stx OLDMAXSPCOBJIND     ;

LOOP003         lda PIXELROWNEW,X       ; Loop over all PLAYFIELD space objs (X index > 4)
                sta PIXELROW,X          ; Update pixel row number of PLAYFIELD space object

                tay                     ;
                lda PFMEMROWLO,Y        ; Point MEMPTR to start of pixel's row...
                sta MEMPTR              ; ...in PLAYFIELD memory
                lda PFMEMROWHI,Y        ;
                sta MEMPTR+1            ;

                lda PIXELCOLUMN,X       ; Convert pixel column number to within-row-offset
                lsr A                   ; ...of byte with space obj pixel (4 pixels = 1 byte)
                lsr A                   ;
                sta PIXELBYTEOFF,X      ; Store within-row-offset

                tay                     ;
                lda (MEMPTR),Y          ; Load pixel's byte from PLAYFIELD memory
                sta PIXELSAVE,X         ; Save it (for restoring it in next game loop)
                ora PIXELBYTE,X         ; Blend with pixel's color bit-pattern
                sta (MEMPTR),Y          ; Store byte in PLAYFIELD memory

                dex                     ;
                cpx #NUMSPCOBJ_PL-1     ;
                bne LOOP003             ; Next PLAYFIELD space object

;*** (4) Clear PLAYFIELD center if idle counter is up (?) **********************
                                        ; PLAYFIELD addresses of...
PFMEM_C76R49    = PFMEM+49*40+76/4      ; ...pixel column number 76, row number 49
PFMEM_C80R49    = PFMEM+49*40+80/4      ; ...pixel column number 80, row number 49
PFMEM_C76R50    = PFMEM+50*40+76/4      ; ...pixel column number 76, row number 50
PFMEM_C80R50    = PFMEM+50*40+80/4      ; ...pixel column number 80, row number 50

SKIP003         lda IDLECNTHI           ; Skip if idle counter not negative
                bpl SKIP004             ;

                lda #0                  ; Clear pixels of 8 x 2 pixel rectangle...
                sta PFMEM_C76R50        ; ...@ column number 76, row number 49 (?)
                sta PFMEM_C80R50        ;
                sta PFMEM_C80R49        ;
                sta PFMEM_C76R49        ;

;*** (5) Clear all PLAYER shapes ***********************************************
SKIP004         lda #0                  ; Clear shape of PLAYER4
                ldy PL4ROW              ;
                ldx PL4HEIGHT           ;
LOOP004         sta PL4DATA,Y           ;
                iny                     ;
                dex                     ;
                bpl LOOP004             ;

                ldy PL3ROW              ; Clear shape of PLAYER3
                ldx PL3HEIGHT           ;
LOOP005         sta PL3DATA,Y           ;
                iny                     ;
                dex                     ;
                bpl LOOP005             ;

                ldy PL2ROW              ; Clear shape of PLAYER2
                ldx PL2HEIGHT           ;
LOOP006         sta PL2DATA,Y           ;
                iny                     ;
                dex                     ;
                bpl LOOP006             ;

                ldy PL1ROW              ; Clear shape of PLAYER1
                ldx PL1HEIGHT           ;
LOOP007         sta PL1DATA,Y           ;
                iny                     ;
                dex                     ;
                bpl LOOP007             ;

                ldy PL0ROW              ; Clear shape of PLAYER0
                ldx PL0HEIGHT           ;
LOOP008         sta PL0DATA,Y           ;
                iny                     ;
                dex                     ;
                bpl LOOP008             ;

;*** (6) Update PLAYER vertical positions and update PLAYER shapes *************
                lda PL4SHAPTYPE         ; CARRY := PLAYER4 a PHOTON TORPEDO (shape type 0)?
                cmp #1                  ;
                ldy PL4SHAPOFF          ; Load PLAYER4 shape data offset

                ldx PL4ROWNEW           ; Update vertical position of PLAYER4
                stx PL4ROW              ;

                lda PL4HEIGHTNEW        ; Update PLAYER4 shape height
                sta L_HEIGHTCNT         ;
                sta PL4HEIGHT           ;

LOOP009         lda PLSHAP1TAB,Y        ; Load PLAYER4 shape byte from shape data table
                bcs SKIP005             ; Skip if PLAYER4 not PHOTON TORPEDO (shape type 0)
                and RANDOM              ; and random bits to shape byte
SKIP005         sta PL4DATA,X           ; Store shape byte in PLAYER4 data area
                iny                     ;
                inx                     ;
                dec L_HEIGHTCNT         ;
                bpl LOOP009             ; Next row of PLAYER4 shape

                lda PL3SHAPTYPE         ; Repeat above with PLAYER3
                cmp #1                  ;
                ldy PL3SHAPOFF          ;
                ldx PL3ROWNEW           ;
                stx PL3ROW              ;
                lda PL3HEIGHTNEW        ;
                sta L_HEIGHTCNT         ;
                sta PL3HEIGHT           ;
LOOP010         lda PLSHAP1TAB,Y        ;
                bcs SKIP006             ;
                and RANDOM              ;
SKIP006         sta PL3DATA,X           ;
                inx                     ;
                iny                     ;
                dec L_HEIGHTCNT         ;
                bpl LOOP010             ;

                lda PL2SHAPTYPE         ; Repeat above with PLAYER2
                cmp #1                  ;
                ldy PL2SHAPOFF          ;
                ldx PL2ROWNEW           ;
                stx PL2ROW              ;
                lda PL2HEIGHTNEW        ;
                sta L_HEIGHTCNT         ;
                sta PL2HEIGHT           ;
LOOP011         lda PLSHAP1TAB,Y        ;
                bcs SKIP007             ;
                and RANDOM              ;
SKIP007         sta PL2DATA,X           ;
                inx                     ;
                iny                     ;
                dec L_HEIGHTCNT         ;
                bpl LOOP011             ;

                ldy PL1SHAPOFF          ; Repeat above with PLAYER1 (without torpedo part)
                ldx PL1ROWNEW           ;
                stx PL1ROW              ;
                lda PL1HEIGHTNEW        ;
                sta L_HEIGHTCNT         ;
                sta PL1HEIGHT           ;
LOOP012         lda PLSHAP2TAB,Y        ;
                sta PL1DATA,X           ;
                inx                     ;
                iny                     ;
                dec L_HEIGHTCNT         ;
                bpl LOOP012             ;

                ldy PL0SHAPOFF          ; Repeat above with PLAYER0 (without torpedo part)
                ldx PL0ROWNEW           ;
                stx PL0ROW              ;
                lda PL0HEIGHTNEW        ;
                sta L_HEIGHTCNT         ;
                sta PL0HEIGHT           ;
LOOP013         lda PLSHAP2TAB,Y        ;
                sta PL0DATA,X           ;
                inx                     ;
                iny                     ;
                dec L_HEIGHTCNT         ;
                bpl LOOP013             ;

;*** (7) Update PLAYER horizontal positions ************************************
                lda PL0COLUMN           ; Update horizontal position of PLAYER0
                sta HPOSP0              ;
                lda PL1COLUMN           ; Update horizontal position of PLAYER1
                sta HPOSP1              ;
                lda PL2COLUMN           ; Update horizontal position of PLAYER2
                sta HPOSP2              ;
                lda PL3COLUMN           ; Update horizontal position of PLAYER3
                sta HPOSP3              ;
                lda PL4COLUMN           ; Update horizontal position of PLAYER4
                sta HPOSM3              ;
                clc                     ;
                adc #2                  ;
                sta HPOSM2              ;
                adc #2                  ;
                sta HPOSM1              ;
                adc #2                  ;
                sta HPOSM0              ;

;*** (8) Rotate space objects horizontally and vertically **********************
                bit SHIPVIEW            ; Skip if in Galactic Chart view
                bmi SKIP009             ;

;*** Rotate horizontally *******************************************************
                lda JOYSTICKX           ; Skip if joystick centered horizontally
                beq SKIP008             ;

                sta JOYSTICKDELTA       ; Save JOYSTICKX (used in subroutine Rotate)
                ldy MAXSPCOBJIND        ; Loop over all space objects in use
LOOP014         sty L_ZPOSOFF           ; Save offset to z-coordinate
                clc                     ;

                tya                     ;
                tax                     ; X := offset to z-coordinate
                adc #NUMSPCOBJ_ALL      ;
                tay                     ; Y := offset to x-coordinate
                jsr Rotate              ; Calc new x-coordinate (horizontal rot @ y-axis)

                tya                     ;
                tax                     ; X := offset to x-coordinate
                ldy L_ZPOSOFF           ; Y := offset to z-coordinate
                jsr Rotate              ; Calc new z-coordinate (horizontal rot @ y-axis)
                dey                     ;
                bpl LOOP014             ; Next space object

;*** Rotate vertically *********************************************************
SKIP008         lda JOYSTICKY           ; Skip if joystick centered vertically
                beq SKIP009             ;

                sta JOYSTICKDELTA       ; Save JOYSTICKY (used in subroutine Rotate)
                ldy MAXSPCOBJIND        ; Loop over all space objects in use
LOOP015         sty L_ZPOSOFF           ; Save offset to z-coordinate
                clc                     ;

                tya                     ;
                tax                     ; X := offset to z-coordinate
                adc #NUMSPCOBJ_ALL*2    ;
                tay                     ; Y := offset to y-coordinate
                jsr Rotate              ; Calc new y-coordinate (vertical rot @ x-axis)

                tya                     ;
                tax                     ; X := offset to y-coordinate
                ldy L_ZPOSOFF           ; Y := offset to z-coordinate
                jsr Rotate              ; Calc new z-coordinate (vertical rot @ x-axis)
                dey                     ;
                bpl LOOP015             ; Next space object

;*** (9) Move all space objects along z-axis (toward our starship) *************
SKIP009         ldx MAXSPCOBJIND        ; Loop over all space objects in use
LOOP016         cpx #NUMSPCOBJ_PL       ; Skip if PLAYFIELD space object (X index > 4)
                bcs SKIP010             ;

                lda PL0SHAPTYPE,X       ; Skip if next PLAYER space obj is PHOTON TORPEDO (!)
                beq SKIP011             ;

SKIP010         sec                     ; New z-coordinate := old z-coordinate -
                lda ZPOSLO,X            ; ...our starship's velocity
                sbc VELOCITYLO          ; (signed 24-bit subtraction)
                sta ZPOSLO,X            ;
                lda ZPOSHI,X            ;
                sbc VELOCITYHI          ;
                sta ZPOSHI,X            ;
                lda ZPOSSIGN,X          ;
                sbc #0                  ;
                sta ZPOSSIGN,X          ;

SKIP011         dex                     ;
                bpl LOOP016             ; Next space object

;*** (10) Add space object's velocity vector to space object's position vector *
                ldx MAXSPCOBJIND        ; Loop over all space objects in use
LOOP017         cpx #NUMSPCOBJ_NORM-1   ; Skip if space object is star (X index 5..16)...
                bne SKIP012             ; ...because stars don't move by themselves
                ldx #4                  ;

SKIP012         txa                     ;
LOOP018         tay                     ; Loop over all 3 coordinates

                lda #0                  ; Expand 8-bit velocity vector component to 16-bit:
                sta L_VELOCITYHI        ; ...16-bit velocity (high byte) = L_VELOCITYHI := 0
                lda ZVEL,Y              ; ...16-bit velocity (low byte)  = A := ZVEL,Y
                bpl SKIP013             ; Skip if 16-bit velocity >= 0 (positive)

                eor #$7F                ; 16-bit velocity < 0 (negative)...
                clc                     ; ...calculate two's-complement of 16-bit velocity
                adc #1                  ;
                bcs SKIP013             ;
                dec L_VELOCITYHI        ;

SKIP013         clc                     ; New coordinate := old coordinate + 16-bit velocity
                adc ZPOSLO,Y            ; (signed 24-bit addition)
                sta ZPOSLO,Y            ;
                lda ZPOSHI,Y            ;
                adc L_VELOCITYHI        ;
                sta ZPOSHI,Y            ;
                lda ZPOSSIGN,Y          ;
                adc L_VELOCITYHI        ;
                sta ZPOSSIGN,Y          ;

                tya                     ;
                clc                     ;
                adc #NUMSPCOBJ_ALL      ;
                cmp #144                ; (!)
                bcc LOOP018             ; Next coordinate

                dex                     ;
                bpl LOOP017             ; Next space object

;*** (11) Correct over/underflow of PLAYER space objects' position vector ******
                ldy #NUMSPCOBJ_PL-1     ;
LOOP019         tya                     ; Loop over all PLAYER space objects (X index < 5)
                tax                     ;

                lda #2                  ; Loop over all 3 coordinates
                sta L_VECCOMPIND        ;

LOOP020         lda ZPOSSIGN,X          ; Load sign of coordinate
                cmp #2                  ;
                bcc SKIP015             ; Skip if sign = 0 (negative) or 1 (positive)

                asl A                   ; SUMMARY: Space object out-of-bounds correction
                lda #0                  ; If new coordinate > +65535 <KM> subtract 256 <KM>
                sta ZPOSSIGN,X          ; ...until new coordinate <= +65535 <KM>
                bcs SKIP014             ; If new coordinate < -65536 <KM> add 256 <KM>
                inc ZPOSSIGN,X          ; ...until new coordinate >= -65536 <KM>
                eor #$FF                ;
SKIP014         sta ZPOSHI,X            ;

SKIP015         txa                     ;
                clc                     ;
                adc #NUMSPCOBJ_ALL      ;
                tax                     ;
                dec L_VECCOMPIND        ;
                bpl LOOP020             ; Next coordinate

                dey                     ;
                bpl LOOP019             ; Next space object

;*** (12) Calc perspective projection of space objects *************************
                lda SHIPVIEW            ; Skip if in Long-Range Scan or Galactic Chart view
                cmp #$02                ;
                bcs SKIP019             ;

                ldx MAXSPCOBJIND        ; Loop over all space objects in use
LOOP021         lda #255                ; Prep magic offscreen pixel number value
                ldy ZPOSSIGN,X          ; Compare sign of z-coordinate with view mode
                cpy SHIPVIEW            ;
                beq SKIP018             ; Equal? Space object is offscreen -> New space obj!

                lda YPOSSIGN,X          ; Prepare projection division...
                bne SKIP016             ; DIVIDEND (16-bit value) := ABS(y-coordinate)
                sec                     ; (used in subroutine PROJECTION)
                lda #0                  ;
                sbc YPOSLO,X            ;
                sta DIVIDEND            ;
                lda #0                  ;
                sbc YPOSHI,X            ;
                sta DIVIDEND+1          ;
                jmp JUMP001             ;
SKIP016         lda YPOSLO,X            ;
                sta DIVIDEND            ;
                lda YPOSHI,X            ;
                sta DIVIDEND+1          ;

JUMP001         jsr PROJECTION          ; Calc pixel row number rel. to screen center
                jsr ScreenRow           ; Calc pixel row number rel. to top-left of screen

                lda XPOSSIGN,X          ; Prepare projection division...
                bne SKIP017             ; DIVIDEND (16-bit value) := ABS(x-coordinate)
                sec                     ; (used in subroutine PROJECTION)
                lda #0                  ;
                sbc XPOSLO,X            ;
                sta DIVIDEND            ;
                lda #0                  ;
                sbc XPOSHI,X            ;
                sta DIVIDEND+1          ;
                jmp JUMP002             ;
SKIP017         lda XPOSLO,X            ;
                sta DIVIDEND            ;
                lda XPOSHI,X            ;
                sta DIVIDEND+1          ;

JUMP002         jsr PROJECTION          ; Calc pixel column number rel. to screen center
SKIP018         jsr ScreenColumn        ; Calc pixel column number rel. to top-left of screen
                dex                     ;
                bpl LOOP021             ; Next space object

;*** (13) Handle hyperwarp marker selection in Galactic Chart view *************
SKIP019         jsr SELECTWARP          ; Handle hyperwarp marker in Galactic Chart view

;*** (14) Compute and draw Long-Range Scan view star field on z-x plane ********
                bit SHIPVIEW            ; Skip if not in Long-Range Scan view
                bvc SKIP022             ;

                ldx #$31                ; Draw our starship's shape
                jsr DRAWLINES           ;

                bit GCSTATLRS           ; Skip if Long-Range Scan destroyed
                bvs SKIP022             ;

                ldx MAXSPCOBJIND        ; Loop over all space objects in use
LOOP022         lda ZPOSHI,X            ; Load z-coordinate (high byte)
                ldy ZPOSSIGN,X          ; Load sign of z-coordinate
                bne SKIP020             ;
                eor #$FF                ; A := ABS(z-coordinate (high byte))
SKIP020         tay                     ;
                lda MAPTO80,Y           ; Calc pixel row number rel. to screen center
                jsr ScreenRow           ; Calc pixel row number rel. to top-left of screen

                lda XPOSHI,X            ; Load x-coordinate (high byte)
                ldy XPOSSIGN,X          ; Load sign of x-coordinate
                bne SKIP021             ;
                eor #$FF                ; A := ABS(x-coordinate (high byte))
SKIP021         tay                     ;
                lda MAPTO80,Y           ; Calc pixel column number rel. to screen center
                jsr ScreenColumn        ; Calc pixel column number rel. to top-left of screen

                dex                     ;
                bpl LOOP022             ; Next space object

;*** (15) Update PLAYER shapes, heights, and colors ****************************

; DESCRIPTION
;
; In a loop over all PLAYERs, the following steps are executed:
;
; o   Clear the PLAYER shape offset and height.
;
; o   If in Galactic Chart view or in Long-Range Scan view, preload a random
;     color and a magic z-coordinate (distance value) for PLAYER3..4
;     (representing hyperwarp markers in Galactic Chart view and blips in the
;     Long-Range Scan view, like, for example, Zylon ships, meteors - or even
;     the Hyperwarp Target Marker during hyperwarp!).
;
; o   If in Front or Aft view, execute the following steps:
;
;      o   Skip dead PLAYERs.
;
;      o   Preload the distance value for the remaining live PLAYERs.
;
;      o   If we are in a starbase sector, combine PLAYER0..2 into a three-part
;          starbase shape. Compute the pixel column numbers and pixel row
;          numbers of PLAYER0..1 such that they are arranged left (PLAYER0) and
;          right (PLAYER1) of PLAYER2. In addition, preload a color mask, a
;          counter actually, that will make the starbase pulsate in brightness.
;
;     BUG (at $A512): The code at $A512 that skips the combination operation for
;     PLAYER2..4 jumps for PLAYER3..4 to SKIP025 at $A52A instead of SKIP026 at
;     $A52E. Thus it stores a color mask which does not only make the starbase
;     PLAYER0..2 pulsate in brightness but also PLAYER3..4 in a starbase sector,
;     for example the transfer vessel, photon torpedoes, etc. - or even the
;     Hyperwarp Target Marker when hyperwarping out of such a sector! Suggested
;     fix: None, code hard to untwist.
;
; o   After storing the color mask, check if the PLAYER shape is still above the
;     bottom edge of the PLAYFIELD.
;
;     BUG (at $A534): The test checks the vertical position of the top edge of
;     the PLAYER against the bottom edge of the PLAYFIELD above the Console
;     Panel Display (= Player/Missile pixel row number 204). This is not
;     completely accurate as the Console Panel Display starts at PM pixel row
;     number 208. For example, if you carefully navigate a starbase to the
;     bottom edge of the PLAYFIELD, at a certain point the center of the
;     starbase shape bleeds over the bottom edge of the PLAYFIELD (while
;     sometimes even losing its left and right wings!). Suggested fix: None, as
;     a more elaborate test may consume too many bytes of the cartridge ROM
;     memory in order to fix a rarely noticed visual glitch.
;
; o   Convert the preloaded distance value of a PLAYER space object closer than
;     $2000 (8192) <KM> into a range index of 0..15. PLAYER space objects more
;     distant than $2000 (8192) <KM> are skipped and not displayed.
;
;     Later, this range index will pick not only the correct brightness for the
;     PLAYER (the closer the space object the brighter its PLAYER) but also the
;     correct PLAYER shape cell and height (the closer the space object the
;     larger the PLAYER shape and height). 
;
; o   Update the PLAYER's shape offset and height. On the way to the shape
;     offset and height add the PLAYER's shape type to the range index and
;     divide it by 2 to arrive at the shape offset index and height index (the
;     same value). Use this index to pick the correct shape data and shape
;     heights from a set of shape cells and their corresponding heights, stored
;     in tables PLSHAPOFFTAB ($BE2F) and PLSHAPHEIGHTTAB ($BE7F), respectively.
;
;     Remember that magic distance value used in the Galactic Chart and
;     Long-Range Scan view? Its value of $F2 is actually part of a negative
;     z-coordinate which is inverted to $0D00, leading to a range index of 13,
;     which, after the division by 2, picks shape cell 6. Shape cell 6 (the
;     seventh shape cell) of all space objects (except the starbase) is the
;     Long-Range Scan blip's dot (see PLSHAPOFFTAB ($BE2F) and PLSHAPHEIGHTTAB
;     ($BE7F)).
;
; o   Update the PLAYER's color/brightness by picking the appropriate values
;     with the range index from lookup tables PLSHAPCOLORTAB ($BFD1) and
;     PLSHAPBRITTAB ($BFDB). Apply some special effects to the color/brightness
;     of certain PLAYERs, such as using random colors for Zylon basestars, or
;     using the precomputed pulsating brightness value for a starbase.

SKIP022         ldx #NUMSPCOBJ_PL       ; Loop over all PLAYER space objects (X index < 5)
LOOP023         dex                     ;
                bpl SKIP023             ; Jump into loop body below
                jmp JUMP003             ; Loop is finished, skip loop body

;*** Clear PLAYER shape offsets and heights ************************************
SKIP023         lda #0                  ;
                sta PL0SHAPOFF,X        ; Clear PLAYER shape offset
                sta PL0HEIGHTNEW,X      ; Clear new PLAYER shape height

;*** Preload stuff for hyperwarp markers and Long-Range Scan blips *************
                bit SHIPVIEW            ; Skip if not in Galactic Chart view
                bpl SKIP024             ;

                cpx #3                  ; Next PLAYER space object if PLAYER0..2
                bcc LOOP023             ;

LOOP024         lda RANDOM              ; Prep random color mask for warp markers/LRS blips
                ldy #$F2                ; Prep magic z-coordinate for warp markers/LRS blips
                bmi SKIP026             ; Unconditional jump

SKIP024         cmp PL0LIFE,X           ; Next PLAYER space object if this PLAYER not alive
                beq LOOP023             ;

                bvs LOOP024             ; Skip back if in Long-Range Scan view

;*** Preload stuff for other views *********************************************

                ldy PL0ZPOSHI,X         ; Prep z-coordinate (high byte)

;*** Combine PLAYER0..2 to starbase shape **************************************
                bit ISSTARBASESECT      ; Skip if no starbase in this sector
                bvc SKIP026             ;

                cpx #2                  ; Skip if PLAYER2..4
                bcs SKIP025             ; (!)

                lda PL2COLUMN           ; Calc new PM pixel column number for PLAYER0..1:
                clc                     ; Load PLAYER2 (starbase center) pixel column number
                adc PLSTARBAOFFTAB,X    ; ...add PLAYER left/right offset (starbase wings)
                sta PL0COLUMN,X         ; Store new PM pixel column number of starbase wing

                lda PL2ROWNEW           ; Calc new PM pixel row number for PLAYER0..1:
                clc                     ; Add vertical offset (= 4 PM pixels) to PLAYER2's
                adc #4                  ;
                sta PL0ROWNEW,X         ; Store new PM pixel row number of starbase wing

                ldy PL2ZPOSHI           ; Prep Y with z-coordinate (high byte) of starbase

SKIP025         lda COUNT256            ; Prep color mask with B3..0 of counter
                and #$0F                ; ...(= brightness bits cause pulsating brightness)

SKIP026         sta L_COLORMASK         ; Store color mask

;*** Check if PLAYER is below PLAYFIELD bottom edge ****************************
                tya                     ; A := z-coordinate (high byte)

                ldy PL0ROWNEW,X         ; Next PLAYER space object if top of PM shape...
                cpy #204                ; ...is below PLAYFIELD bottom... (!)
                bcs LOOP023             ; ...(PM pixel row number >= 204)

;*** Convert PLAYER z-coordinate to range index in 0..15 ***********************
                ldy SHIPVIEW            ; Skip if in Front view...
                beq SKIP027             ;
                eor #$FF                ; ...else invert z-coordinate (high byte)

SKIP027         cmp #$20                ; Next PLAYER space object if this one too far away
                bcs LOOP023             ; ...(z-coordinate >= $20** (8192) <KM>)

                cmp #16                 ; Load z-coordinate (high byte) and...
                bcc SKIP028             ;
                lda #15                 ;
SKIP028         sta L_RANGEINDEX        ; ...trim to range index in 0..15

;*** Update PLAYER shape offset and height *************************************
                ora PL0SHAPTYPE,X       ; Calc offset to shape table (shape type+range index)
                lsr A                   ;
                tay                     ; Divide by 2 to get offset in 0..7 into shape data
                lda PLSHAPOFFTAB,Y      ; Update new PLAYER shape offset
                sta PL0SHAPOFF,X        ;
                lda PLSHAPHEIGHTTAB,Y   ; Update new PLAYER shape height
                sta PL0HEIGHTNEW,X      ;

;*** Calculate PLAYER color/brightness value ***********************************
                tya                     ; Pick color (B7..4) using PLAYER shape type
                lsr A                   ;
                lsr A                   ;
                lsr A                   ;
                tay                     ;
                lda PLSHAPCOLORTAB,Y    ;
                cpy #8                  ; Pick random color if ZYLON BASESTAR (shape type 8)
                bne SKIP029             ;
                eor RANDOM              ;
SKIP029         ldy L_RANGEINDEX        ;
                eor PLSHAPBRITTAB,Y     ; Pick brightness (B3..0) using range index and merge

                eor L_COLORMASK         ; Modify color/brightness of PLAYER

                ldy PLCOLOROFFTAB,X     ; Get PLAYER color offset
                sta PL0COLOR,Y          ; Store color in PLAYER color register
                jmp LOOP023             ; Next PLAYER space object

;*** (16) Flash red alert ******************************************************
JUMP003         ldy #$AF                ; Prep PLAYFIELD2 color {BRIGHT BLUE-GREEN}
                ldx SHIELDSCOLOR        ; Prep Shields color {DARK GREEN} or {BLACK}

                lda REDALERTLIFE        ; Skip if red alert is over
                beq SKIP030             ;

                dec REDALERTLIFE        ; Decrement lifetime of red alert
                ldy #$4F                ; Prep PLAYFIELD2 color {BRIGHT ORANGE}

                and #$20                ; Switch colors every 64 game loops
                beq SKIP030             ;

                ldx #$42                ; Load BACKGROUND color {DARK ORANGE}
                ldy #$60                ; Load PLAYFIELD2 color {DARK PURPLE BLUE}

SKIP030         sty PF2COLOR            ; Store PLAYFIELD2 color
                stx BGRCOLOR            ; Store BACKGROUND color

;*** (17) Update color of PLAYFIELD space objects (stars, explosion fragments) *
                ldx MAXSPCOBJIND        ; Loop over all PLAYFIELD space objs (X index > 4)
LOOP025         lda ZPOSHI,X            ; Prep z-coordinate (high byte)
                ldy SHIPVIEW            ;
                cpy #1                  ; Skip if not in Aft view
                bne SKIP032             ;

                cmp #$F0                ; Skip if star not too far (z < $F0** (-4096) <KM>)
                bcs SKIP031             ;
                jsr InitPosVec          ; Re-init position vector
SKIP031         eor #$FF                ; Invert z-coordinate (high byte)

SKIP032         cmp #16                 ; Convert z-coordinate (high byte)
                bcc SKIP033             ; ...into range index 0..15
                lda #15                 ;

SKIP033         asl A                   ; Compute index to pixel color table:
                and #$1C                ; Use bits B3..1 from range index as B4..2.
                ora COUNT8              ; Combine with random bits B3..0 from counter

                tay                     ;
                lda FOURCOLORPIXEL,Y    ; Load 1-byte bit pattern for 4 pixels of same color
                sta L_FOURCOLORPIX      ; ...and temporarily save it

                lda PIXELCOLUMN,X       ; Load pixel mask to mask 1 pixel out of 4 pixels:
                and #$03                ; Use B1..0 from pixel column number...
                tay                     ;
                lda PIXELMASKTAB,Y      ; ...to pick mask to filter pixel in byte
                and L_FOURCOLORPIX      ; ...and with 1-byte bit pattern for 4 pixels
                sta PIXELBYTE,X         ; ...store byte (used in repaint step of game loop)

                dex                     ;
                cpx #NUMSPCOBJ_PL       ;
                bcs LOOP025             ; Next PLAYFIELD space object

;*** (18) Skip input handling if in demo mode **********************************
                bit ISDEMOMODE          ; If in demo mode skip to function keys
                bvc SKIP034             ;
                jmp SKIP040             ;

;*** (19) Handle keyboard input ************************************************
SKIP034         jsr KEYBOARD            ; Handle keyboard input

;*** (20) Handle joystick input ************************************************
                lda PORTA               ; Load Joystick 0 directions
                tay                     ; ...Bits B0..3 -> Right, left, down, up.
                and #$03                ; ...Bit = 0/1 -> Stick pressed/not pressed
                tax                     ; JOYSTICKY := +1 -> Up
                lda STICKINCTAB,X       ; JOYSTICKY :=  0 -> Centered
                sta JOYSTICKY           ; JOYSTICKY := -1 -> Down
                tya                     ;
                lsr A                   ;
                lsr A                   ;
                and #$03                ;
                tax                     ; JOYSTICKX := -1 -> Left
                lda STICKINCTAB,X       ; JOYSTICKX :=  0 -> Centered
                sta JOYSTICKX           ; JOYSTICKX := +1 -> Right

;*** (21) Check if our starship's photon torpedoes have hit a target ***********
                jsr Collision           ; Check if our starship's photon torpedoes have hit

;*** (22) Handle joystick trigger **********************************************
                jsr TRIGGER             ; Handle joystick trigger

;*** (23) Handle Attack Computer and Tracking Computer *************************
                bit GCSTATCOM           ; Skip if Attack Computer destroyed
                bvs SKIP038             ;

                lda DRAINATTCOMP        ; Skip if Attack Computer off
                beq SKIP038             ;

                lda SHIPVIEW            ; Skip if not in Front view
                bne SKIP035             ;

                jsr UPDATTCOMP          ; Update Attack Computer Display

SKIP035         ldx TRACKDIGIT          ; Load index of tracked space object

                lda ZYLONATTACKER       ; Skip if ship of current Zylon torpedo is tracked
                bmi SKIP036             ;
                tax                     ; ...else override Tracking Computer...
                ora #$80                ;
                sta ZYLONATTACKER       ; ...and mark Zylon torpedo's ship as being tracked

SKIP036         lda PL0LIFE,X           ; Skip if tracked space object still alive
                bne SKIP037             ;

                txa                     ;
                eor #$01                ;
                tax                     ;
                lda PL0LIFE,X           ; Check if other Zylon ship still alive
                bne SKIP037             ; ...yes -> Keep new index
                ldx TRACKDIGIT          ; ...no  -> Revert to old index of tracked space obj

SKIP037         stx TRACKDIGIT          ; Store index of tracked space object

                lda ISTRACKCOMPON       ; Skip if tracking computer is turned off
                beq SKIP038             ;

                lda SHIPVIEW            ; Skip if in Long-Range Scan or Galactic Chart view
                cmp #2                  ;
                bcs SKIP038             ;

                eor #$01                ;
                cmp ZPOSSIGN,X          ; Skip if tracked space object in our starship's...
                beq SKIP038             ; ...view direction

                tax                     ;
                lda TRACKKEYSTAB,X      ; Pick 'F' or 'A' (Front or Aft view) keyboard code
                sta KEYCODE             ; ...and store it (= emulate pressing 'F' or 'A' key)

;*** (24) Handle docking to starbase *******************************************
SKIP038         jsr Docking             ; Handle docking to starbase

;*** (25) Handle maneuvering ***************************************************
                jsr MANEUVER            ; Handle maneuvering photon torpedoes and Zylon ships

;*** (26) Was our starship hit by Zylon photon torpedo? ************************
                lda ISSTARBASESECT      ; Skip hit check if in starbase sector
                bne SKIP040             ;

                lda PL2LIFE             ; Skip hit check if PLAYER2 (Zylon photon torpedo)...
                beq SKIP040             ; ...not alive

                ldy PL2ZPOSHI           ; Our starship was not hit if Zylon photon torpedo's
                iny                     ; ...z-coordinate is not in -256..255 <KM> or...
                cpy #$02                ;
                bcs SKIP040             ;

                ldy PL2XPOSHI           ; ...x-coordinate is not in -256..255 <KM> or...
                iny                     ;
                cpy #$02                ;
                bcs SKIP040             ;

                ldy PL2YPOSHI           ; ...y-coordinate is not in -256..255 <KM>.
                iny                     ;
                cpy #$02                ;
                bcs SKIP040             ;

;*** (27) Our starship was hit! ************************************************
                jsr Damage              ; Damage or destroy some subsystem

                ldy #2                  ; Trigger explosion at PLAYER2 (Zylon photon torpedo)
                jsr InitExpl            ;

                ldx #$7F                ; Prep HITBADNESS := SHIELDS HIT
                lda SHIELDSCOLOR        ; Skip if Shields are up (SHIELDSCOLOR not {BLACK}).
                bne SKIP039             ;

                ldx #$0A                ; Set Front view
                jsr SETVIEW             ;

                ldy #$23                ; Set title phrase "SHIP DESTROYED BY ZYLON FIRE"
                ldx #8                  ; Set mission bonus offset
                jsr GAMEOVER            ; Game over

                ldx #$5F                ; Hide Control Panel Display (bottom text window)
                ldy #$80                ;
                lda #$08                ;
                jsr ModDLST             ;

                jsr ClrPlayfield        ; Clear PLAYFIELD

                ldx #64                 ; Enable STARSHIP EXPLOSION noise (see SOUND)
                stx NOISEHITLIFE        ;

                ldx #$FF                ; Prep HITBADNESS := STARSHIP DESTROYED

SKIP039         stx HITBADNESS          ; Store HITBADNESS
                lda #0                  ; Zylon photon torpedo lifetime := 0 game loops
                sta PL2LIFE             ;
                lda #2                  ; Init Zylon photon torpedo trigger
                sta TORPEDODELAY        ;

                ldx #1                  ; ENERGY := ENERGY - 100 after photon torpedo hit
                jsr DECENERGY           ;

                ldx #$0A                ; Play noise sound pattern SHIELD EXPLOSION
                jsr NOISE               ;

;*** (28) Handle function keys *************************************************
SKIP040         ldy FKEYCODE            ; Prep old function key code
                lda CONSOL              ; POKEY: Load function key code

                eor #$FF                ; Store inverted and masked function key code
                and #$03                ;
                sta FKEYCODE            ;
                beq SKIP042             ; Skip if no function key pressed

                dey                     ;
                bpl SKIP042             ; Skip if SELECT or START still pressed
                sta IDLECNTHI           ; Reset idle counter to a value in 1..3 (?)
                cmp #2                  ; Skip if SELECT function key pressed
                bcs SKIP041             ;

                lda #0                  ; START function key pressed:
                tay                     ; Prep empty title phrase offset
                jmp INITSTART           ; Reenter game loop via INITSTART

SKIP041         inc MISSIONLEVEL        ; SELECT function key pressed:
                lda MISSIONLEVEL        ; Cycle through next of 4 mission levels
                and #$03                ;
                sta MISSIONLEVEL        ;
                jmp INITSELECT          ; Reenter game loop via INITSELECT

;*** (29) Update Control Panel Display *****************************************
SKIP042         jsr UPDPANEL            ; Update Control Panel Display

;*** (30) Handle hyperwarp *****************************************************
                jsr HYPERWARP           ; Handle hyperwarp

;*** (31) Update title line ****************************************************
                jsr UPDTITLE            ; Update title line

;*** (32) Flush game loop iteration ********************************************
                jsr FLUSHGAMELOOP       ; Move Zylon units, age torpedoes, elapse time

;*** (33) Jump back to begin of game loop **************************************
                jmp GAMELOOP            ; Next game loop iteration
