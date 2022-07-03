; *******************************************************************************
; *                                                                             *
; *                                   Rotate                                    *
; *                                                                             *
; *        Rotate position vector component (coordinate) by fixed angle         *
; *                                                                             *
; *******************************************************************************

; DESCRIPTION
;
; This subroutine rotates a position vector component (coordinate) of a space
; object by a fixed angle around the center of the 3D coordinate system, the
; location of our starship. This is used in the Front, Aft, and Long-Range Scan
; views to rotate space objects in and out of the view. Although the code is
; deceptively short, there is some interesting math involved, so a more detailed
; discussion is in order.
;
; ROTATION MATHEMATICS
;
; The game uses a left-handed 3D coordinate system with the positive x-axis
; pointing to the right, the positive y-axis pointing up, and the positive
; z-axis pointing into flight direction.
;
; A rotation in this coordinate system around the y-axis (horizontal rotation)
; can be expressed as 
;
;     x' :=   cos(ry) * x + sin(ry) * z    (1a)
;     z' := - sin(ry) * x + cos(ry) * z    (1b)
;
; where ry is the clockwise rotation angle around the y-axis, x and z are the
; coordinates before this rotation, and the primed coordinates x' and z' the
; coordinates after this rotation. The y-coordinate is not changed by this
; rotation.
;
; A rotation in this coordinate system around the x-axis (vertical rotation) can
; be expressed as 
;
;     z' :=   cos(rx) * z + sin(rx) * y    (2a)
;     y' := - sin(rx) * z + cos(rx) * y    (2b)
;
; where rx is the clockwise rotation angle around the x-axis, z and y are the
; coordinates before this rotation, and the primed coordinates z' and y' the
; coordinates after this rotation. The x-coordinate is not changed by this
; rotation.
;
; SUBROUTINE IMPLEMENTATION OVERVIEW
;
; A single call of this subroutine is able to compute one of the four
; expressions (1a)-(2b). To compute all four expressions to get the new set of
; coordinates, this subroutine has to be called four times. This is done twice
; in pairs in GAMELOOP ($A1F3) at $A391 and $A398, and at $A3AE and $A3B5,
; respectively.
;
; The first pair of calls calculates the new x and z coordinates of a space
; object due to a horizontal (left/right) rotation of our starship around the
; y-axis following expressions (1a) and (1b).
;
; The second pair of calls calculates the new y and z coordinates of the same
; space object due to a vertical (up/down) rotation of our starship around the
; x-axis following expressions (2a) and (2b).
;
; If you look at the code, you may be wondering how this calculation is actually
; executed, as there is neither a sin() nor a cos() function call. What you'll
; actually find implemented, however, are the following calculations:
;
;     Joystick left                        Joystick right
;     ---------------------                ---------------------
;     x :=  x      + z / 64    (3a)        x :=  x      - z / 64    (4a)
;     z := -x / 64 + z         (3b)        z :=  x / 64 + z         (4b)
;
;     Joystick down                        Joystick up
;     ---------------------                ---------------------
;     y :=  y      + z / 64    (5a)        y :=  y      - z / 64    (6a)
;     z := -y / 64 + z         (5b)        z :=  y / 64 + z         (6b)
;
; CORDIC ALGORITHM
;
; When you compare expressions (1a)-(2b) with (3a)-(6b), notice the similarity
; between the expressions if you substitute
;
;     sin(ry) -> 1 / 64, 
;     cos(ry) -> 1,
;     sin(rx) -> 1 / 64, and
;     cos(rx) -> 1.
;
; From sin(ry) = 1 / 64 and sin(rx) = 1 / 64 you can derive that the rotation
; angles ry and rx by which the space object is rotated per game loop iteration
; have a constant value of 0.89 degrees, as arcsine(1 / 64) = 0.89 degrees.
;
; What about cos(ry) and cos(rx)? The substitution does not match our derived
; angle exactly, because cos(0.89 degrees) = 0.99988 and is not exactly 1.
; However, this value is so close to 1 that substituting cos(0.89 degrees) with
; 1 is a very good approximation, simplifying calculations significantly.
;
; Another significant simplification results from the division by 64, because
; the actual division operation can be replaced with a much faster bit shift
; operation.
;
; This calculation-friendly way of computing rotations is known as the "CORDIC
; (COordinate Rotation DIgital Computer)" algorithm.
;
; MINSKY ROTATION
;
; There is one more interesting mathematical subtlety: Did you notice that
; expressions (1a)-(2b) use a new (primed) pair of variables to store the
; resulting coordinates, whereas in the implemented expressions (3a)-(6b) the
; value of the first coordinate of a coordinate pair is overwritten with its new
; value and this value is used in the subsequent calculation of the second
; coordinate? For example, when the joystick is pushed left, the first call of
; this subroutine calculates the new value of x according to expression (3a),
; overwriting the old value of x. During the second call to calculate z
; according to expression (3b), the new value of x is used instead of the old
; one. Is this to save the memory needed to temporarily store the old value of
; x? Is this a bug? If so, why does the rotation calculation actually work?
;
; Have a look at the expression pair (3a) and (3b) (the other expression pairs
; (4a)-(6b) work in a similar fashion):
;
;     x :=  x      + z / 64
;     z := -x / 64 + z
;
; With the substitution 1 / 64 -> e, we get
;
;     x :=  x     + e * z
;     z := -e * x + z
;
; Note that x is calculated first and then used in the second expression. When
; using primed coordinates for the resulting coordinates after calculating the
; two expressions we get
;
;     x' := x + e * z
;     z' := -e * x' + z = -e * (x + e * z) + z = -e * x + (1 - e^2) * z
;
; or in matrix form
;
;     |x'| := | 1       e   | * |x|
;     |z'|    |-e  (1 - e^2)|   |z|
;
; Surprisingly, this turns out to be a rotation matrix, because its determinant
; is (1 * (1 - e^2) - (e * -e)) = 1.
;
; (Incidentally, the column vectors of this matrix do not form an orthogonal
; basis, as their scalar product is 1 * e + (-e * (1 - e^2)) = -e^2.
; Orthogonality holds for e = 0 only.)
;
; This kind of rotation calculation is described by Marvin Minsky in ["AIM 239
; HAKMEM", Item 149, p. 73, MIT AI Lab, February 1972] and is called "Minsky
; Rotation".
;
; SUBROUTINE IMPLEMENTATION DETAILS
;
; To better understand how the implementation of this subroutine works, have
; again a look at expressions (3a)-(6b). If you rearrange the expressions a
; little their structure is always of the form
;
;     TERM1 := TERM1 SIGN TERM2 / 64
;
;     or shorter
;
;     TERM1 := TERM1 SIGN TERM3
;
;     where
;
;     TERM3 := TERM2 / 64
;     SIGN := + or -
;
; and where TERM1 and TERM2 are position vector components (coordinates). In
; fact, this is all this subroutine actually does: It simply adds TERM2 divided
; by 64 to TERM1 or subtracts TERM2 divided by 64 from TERM1. 
;
; When calling this subroutine the correct indices for the appropriate position
; vector components (coordinates) TERM1 and TERM2 are passed in the Y and X
; registers, respectively.
;
; What about SIGN between TERM1 and TERM3? Have again a look at expressions
; (3a)-(6b). To compute the two new coordinates after a rotation, the SIGN
; toggles from plus to minus and vice versa. The SIGN is initialized with
; JOYSTICKDELTA ($6D) before calling subroutine Rotate ($B69B) and is toggled
; inside every call of this subroutine before the addition or subtraction of the
; terms takes place there. The initial value of SIGN should be positive (+) if
; the rotation is clockwise (the joystick is pushed right or up) and negative
; (-) if the rotation is counter-clockwise (the joystick is pushed left or
; down), respectively. Because SIGN is always toggled inside the subroutine
; before the addition or subtraction of the terms actually happens there, you
; have to pass the already toggled value with the first call.
;
; NOTE: Unclear still are three instructions starting at address $B6AD. They
; seem to set the two least significant bits of TERM3 in a random fashion. Could
; this be some quick hack to avoid messing with exact but potentially lengthy
; two-complement's arithmetic here?
;
; INPUT
;
;   X = Position vector component index of TERM2. Used values are:
;     $00..$30 -> z-component (z-coordinate) of position vector 0..48
;     $31..$61 -> x-component (x-coordinate) of position vector 0..48
;     $62..$92 -> y-component (y-coordinate) of position vector 0..48
;
;   Y = Position vector component index of TERM1. Used values are: 
;     $00..$30 -> z-component (z-coordinate) of position vector 0..48
;     $31..$61 -> x-component (x-coordinate) of position vector 0..48
;     $62..$92 -> y-component (y-coordinate) of position vector 0..48
;
;   JOYSTICKDELTA ($6D) = Initial value of SIGN. Used values are:
;     $01 -> (= Positive) Rotate right or up
;     $FF -> (= Negative) Rotate left or down


;======================================
; Rotate position vector component
;======================================
Rotate          .proc
                                        ; TERM3 is a 24-bit value, represented by 3 bytes as
                                        ; $(sign)(high byte)(low byte)
L_TERM3LO       = $6A                   ; TERM3 (high byte), where TERM3 := TERM2 / 64
L_TERM3HI       = $6B                   ; TERM3 (low byte),  where TERM3 := TERM2 / 64
L_TERM3SIGN     = $6C                   ; TERM3 (sign),      where TERM3 := TERM2 / 64
;---

                lda ZPOSSIGN,X          ;
                eor #$01                ;
                beq _1                  ; Skip if sign of TERM2 is positive
                lda #$FF                ;

_1              sta L_TERM3HI           ; If TERM2 pos. -> TERM3 := $0000xx (= TERM2 / 256)
                sta L_TERM3SIGN         ; If TERM2 neg. -> TERM3 := $FFFFxx (= TERM2 / 256)
                lda ZPOSHI,X            ; where xx := TERM2 (high byte)
                sta L_TERM3LO           ;

                lda RANDOM              ; (?) Hack to avoid messing with two-complement's
                ora #$BF                ; (?) arithmetic? Provides two least significant
                eor ZPOSLO,X            ; (?) bits B1..0 in TERM3.

                asl A                   ; TERM3 := TERM3 * 4 (= TERM2 / 256 * 4 = TERM2 / 64)
                rol L_TERM3LO           ;
                rol L_TERM3HI           ;
                asl A                   ;
                rol L_TERM3LO           ;
                rol L_TERM3HI           ;

                lda JOYSTICKDELTA       ; Toggle SIGN for next call of Rotate
                eor #$FF                ;
                sta JOYSTICKDELTA       ;
                bmi _2                  ; If SIGN negative then subtract, else add TERM3

;*** Addition ******************************************************************
                clc                     ; TERM1 := TERM1 + TERM3
                lda ZPOSLO,Y            ; (24-bit addition)
                adc L_TERM3LO           ;
                sta ZPOSLO,Y            ;

                lda ZPOSHI,Y            ;
                adc L_TERM3HI           ;
                sta ZPOSHI,Y            ;

                lda ZPOSSIGN,Y          ;
                adc L_TERM3SIGN         ;
                sta ZPOSSIGN,Y          ;
                rts                     ;

;*** Subtraction ***************************************************************
_2              sec                     ; TERM1 := TERM1 - TERM3
                lda ZPOSLO,Y            ; (24-bit subtraction)
                sbc L_TERM3LO           ;
                sta ZPOSLO,Y            ;

                lda ZPOSHI,Y            ;
                sbc L_TERM3HI           ;
                sta ZPOSHI,Y            ;

                lda ZPOSSIGN,Y          ;
                sbc L_TERM3SIGN         ;
                sta ZPOSSIGN,Y          ;
                rts                     ;
                .endproc


;*******************************************************************************
;*                                                                             *
;*                                ScreenColumn                                 *
;*                                                                             *
;*       Calculate pixel column number from centered pixel column number       *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Converts a pixel column number relative to the horizontal screen center to a
; pixel column number relative to the top-left corner of the screen and stores
; the result in table PIXELCOLUMN ($0C2A). The passed relative pixel column
; number is always positive. The sign is picked from the corresponding
; x-component of the position vector (x-coordinate).
;
; If the passed relative pixel column number is offscreen horizontally the
; calculation is skipped and code execution returns. If the position vector
; corresponding to this pixel represents a PLAYFIELD space object (star,
; explosion fragments) a new position vector is initialized before code
; execution returns. If it represents a PLAYER space object the PLAYER is pushed
; offscreen before code execution returns.
;
; NOTE: The horizontal screen center's pixel column number for PLAYFIELD space
; objects has a value of 80 = 160 PLAYFIELD pixels / 2. For PLAYER space objects
; it has a value of 125 Player/Missile (PM) pixels (from left to right: 128 PM
; pixels to the horizontal screen center - 3 PM pixels relative offset of the
; PLAYER shape's horizontal center to its left edge = 125 PM pixels).
;
; INPUT
;
;   A = Pixel column number relative to the horizontal screen center, always
;       positive. Used values are:
;     0..80 -> Regular values, pixel is onscreen
;     $FF   -> Pixel is offscreen
;
;   X = Position vector index. Used values are:
;     0..4  -> Position vector of a PLAYER space object
;     5..48 -> Position vector of a PLAYFIELD space object


;======================================
; Calculate pixel column number
;======================================
ScreenColumn    .proc
L_PIXELCOLUMN   = $6D                   ; Saves relative pixel column number
;---

                cmp #80                 ; If pixel is offscreen (A > 79)...
                bcs ScreenRow._6        ; ...return via initializing a new position vector

                sta L_PIXELCOLUMN       ; Save relative pixel column number
                lda #80                 ; If PLAYFIELD space object -> A := CENTERCOL = 80
                cpx #NUMSPCOBJ_PL       ; If PLAYER space object    -> A := CENTERCOL = 125
                bcs _1                  ;
                lda #125                ;

_1              ldy XPOSSIGN,X          ; Skip if x-coordinate positive
                bne _2                  ;

                sec                     ; Pixel in left screen half (x-coordinate negative)
                inc L_PIXELCOLUMN       ;
                sbc L_PIXELCOLUMN       ;
                sta PIXELCOLUMN,X       ; Pixel column := CENTERCOL - (rel. pixel column + 1)
                rts

_2              clc                     ; Pixel in right screen half (x-coordinate positive)
                adc L_PIXELCOLUMN       ;
                sta PIXELCOLUMN,X       ; Pixel column := CENTERCOL + relative pixel column
                rts
                .endproc


;*******************************************************************************
;*                                                                             *
;*                                  ScreenRow                                  *
;*                                                                             *
;*          Calculate pixel row number from centered pixel row number          *
;*                                                                             *
;*******************************************************************************

; Converts a pixel row number relative to the vertical screen center to a pixel
; row number relative to the top-left corner of the screen and stores the result
; in table PIXELROWNEW ($0BF9). The passed relative pixel row number is always
; positive. The sign is picked from the corresponding y-component of the
; position vector (y-coordinate).
;
; If the passed relative pixel row number is offscreen vertically the
; calculation is skipped and code execution returns. If the position vector
; corresponding to this pixel represents a PLAYFIELD space object (star,
; explosion fragments) a new position vector is initialized in subroutine
; InitPosVec ($B764) before code execution returns. If it represents a PLAYER
; space object the PLAYER is pushed offscreen before code execution returns.
;
; NOTE: The vertical screen center's pixel row number for PLAYFIELD space
; objects has a value of 50 = 100 PLAYFIELD pixels / 2. For PLAYER space objects
; it has a value of 122 Player/Missile (PM) pixels (from top to bottom: 8 PM
; pixels to start of Display List + 16 PM pixels to begin of PLAYFIELD + 100 PM
; pixels to vertical screen center - 2 PM pixels (?) = 122 PM pixels).
;
; NOTE: If the position vector corresponding to the pixel represents a PLAYER
; space object the passed pixel row number is doubled because 1 PLAYFIELD pixel
; has the same height as 2 PM pixels at single-line resolution.
;
; When in Long-Range Scan view the z-coordinate takes the place of the
; y-coordinate of the Front or Aft view. If the Long-Range Scan is damaged the
; passed pixel row number is treated randomly as a negative or positive value
; (mirror effect).
;
; INPUT
;
;   A = Pixel row number relative to the vertical screen center, always
;       positive. Used values are:
;     0..50 -> Regular values, pixel is onscreen
;     $FF   -> Pixel is offscreen
;
;   X = Position vector index. Used values are:
;     0..4  -> Position vector of a PLAYER space object
;     5..48 -> Position vector of a PLAYFIELD space object


;======================================
; Calculate pixel row number
;======================================
ScreenRow       .proc
L_PIXELROW      = $6D                   ; Saves relative pixel row number
;---

                cmp #50                 ; If pixel is offscreen (A > 49)...
                bcs _6                  ; ...return via initializing a new position vector

                sta L_PIXELROW          ; Save relative pixel row number
                lda #50                 ; If PLAYFIELD space object -> A := CENTERROW = 50
                cpx #NUMSPCOBJ_PL       ;
                bcs _1                  ;
                asl L_PIXELROW          ; If PLAYER space object -> Double pixel row number
                lda #122                ; If PLAYER space object ->    A := CENTERROW = 122

_1              bit SHIPVIEW            ; Skip if not in Long-Range Scan view
                bvc _3                  ;

                bit GCSTATLRS           ; Skip if Long-Range Scan OK
                bpl _2                  ;

                bit RANDOM              ; Long-Range Scan damaged...
                bvc _4                  ; ...branch randomly to pixel row number calculation
                bvs _5                  ; ...(mirror effect)

_2              ldy ZPOSSIGN,X          ;
                bne _4                  ; Skip if z-coordinate pos. (Long-Range Scan view)
                beq _5                  ; Skip if z-coordinate neg. (Long-Range Scan view)

_3              ldy YPOSSIGN,X          ;
                beq _5                  ; Skip if y-coordinate neg. (Front or Aft view)

_4              sec                     ; Pixel in upper screen half (z or y coordinate pos.)
                inc L_PIXELROW          ;
                sbc L_PIXELROW          ;
                sta PIXELROWNEW,X       ; Pixel row  := CENTERROW - (rel. pixel row + 1)
                rts

_5              clc                     ; Pixel in lower screen half (y or z coordinate neg.)
                adc L_PIXELROW          ;
                sta PIXELROWNEW,X       ; Pixel row := CENTERROW + relative pixel row
                rts

_6              cpx #NUMSPCOBJ_PL       ; Space object is offscreen. If it is a...
                bcs InitPosVec          ; ...PLAYFIELD space object -> New position vector
                lda #251                ; ...PLAYER space object    -> Push PLAYER offscreen
                sta PIXELROWNEW,X       ;                              Why a value of 251 (?)
_XIT            rts
                .endproc


;*******************************************************************************
;*                                                                             *
;*                                 InitPosVec                                  *
;*                                                                             *
;*                Initialize position vector of a space object                 *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Initializes the position vector of a space object.
;
; This subroutine executes the following steps:
;
; (1)  Set the pixel row and column number to an offscreen value (= 99).
;
; (2)  If the position vector represents an explosion fragment space object then
;      return code execution immediately. This avoids generating new explosion
;      fragment space objects. They are separately initialized in subroutine
;      CopyPosVec ($ACAF), which is called from subroutine InitExpl ($AC6B).
;
; (3)  Assign default values (see below) to the position vector components
;      (coordinates) depending on our starship's view.
;
; Code execution continues into subroutine RndInvXY ($B7BE) where x and y
; coordinates are inverted randomly.
;
; After passing through this and the next subroutine RndInvXY ($B7BE) the
; components of a position vector (coordinates) are assigned to one of the
; following values depending on our starship's view:
;
; o   FRONT VIEW
;
;     +------------+---------------------------------------+
;     | Coordinate |                 Values                |
;     +------------+---------------------------------------+
;     |     x      |  -4095..+4095 (-($0***)..+$0***) <KM> |
;     |     y      |  -4095..+4095 (-($0***)..+$0***) <KM> | 
;     |     z      |  +3840..+4095 (          +$0F**) <KM> |
;     +------------+---------------------------------------+
;
; o   AFT VIEW
;
;     +------------+---------------------------------------+
;     | Coordinate |                 Values                |
;     +------------+---------------------------------------+ 
;     |     x      |  -3840..+3840 (-($0*00)..+$0*00) <KM> |
;     |     y      |  -3840..+3840 (-($0*00)..+$0*00) <KM> |
;     |     z      |  -3968.. -128 (-($0*80)        ) <KM> |
;     +------------+---------------------------------------+ 
;     Values of x, y, and z coordinates change in increments of 256.
;     Second digit of z-coordinate is -MAX(RNDY,RNDX), where
;     RNDY := RND($00..$0F), RNDX := RND($00..$0F).
;
; o   LONG-RANGE SCAN VIEW
;
;     +------------+---------------------------------------+
;     | Coordinate |                 Values                |
;     +------------+---------------------------------------+
;     |     x      | -65535..+65535 (-($****)..$****) <KM> |
;     |     y      |  -4095..+4095  (-($0***)..$0***) <KM> |
;     |     z      | -65535..+65535 (-($****)..$****) <KM> |
;     +------------+---------------------------------------+
;
; INPUT
;
;   X = Position vector index. Used values are: 0..48.


;======================================
; Initialize position vector
;======================================
InitPosVec      .proc
L_MAXRNDXY      = $6A                   ; Saves MAX(new y-coordinate (high byte), ...
                                        ;  ...new x-coordinate (high byte))
;---

                lda #99                 ; Init to offscreen pixel row and column numbers
                sta PIXELROWNEW,X       ;
                sta PIXELCOLUMN,X       ;

                cpx #NUMSPCOBJ_NORM     ; Return if pos vector is explosion frag space obj
                bcs ScreenRow._XIT      ; This avoids creating new explosion frag space objs

                lda RANDOM              ; RNDY := RND($00..$0F)
                and #$0F                ;
                sta L_MAXRNDXY          ; Save RNDY
                sta YPOSHI,X            ; y-coordinate (high byte) := RNDY

                lda RANDOM              ; RNDX := RND($00..$0F)
                and #$0F                ;
                cmp L_MAXRNDXY          ;
                bcc _1                  ;
                sta L_MAXRNDXY          ; Save MAX(RNDY,RNDX)
_1              sta XPOSHI,X            ; x-coordinate (high byte) := RNDX

                lda #$0F                ; z-coordinate (high byte) := $0F
                sta ZPOSHI,X            ;

                lda SHIPVIEW            ; z-coordinate (sign) := 1 or 0 (Front or Aft view)
                eor #$01                ;
                and #$01                ;
                sta ZPOSSIGN,X          ;
                bne _2                  ; Skip if in Front or Long-Range Scan view

                                        ; Aft view only:
                sta XPOSLO,X            ; x-coordinate (low byte) := 0
                sta YPOSLO,X            ; y-coordinate (low byte) := 0
                sec                     ; z-coordinate (high byte) := -MAX(RNDY,RNDX)
                sbc L_MAXRNDXY          ;
                sta ZPOSHI,X            ;
                lda #$80                ; z-coordinate (low byte) := $80
                sta ZPOSLO,X            ;

_2              bit SHIPVIEW            ; If not in Long-Range Scan view skip to RndInvXY
                bvc RndInvXY            ;

                                        ; Long-Range Scan view only:
                lda RANDOM              ; x-coordinate (high byte) := RND($00..$FF)
                sta XPOSHI,X            ;
                lda RANDOM              ; z-coordinate (high byte) := RND($00..$FF)
                sta ZPOSHI,X            ;
                and #$01                ; Invert z-coordinate randomly
                sta ZPOSSIGN,X          ;

                .endproc

                ;[fall-through]


;*******************************************************************************
;*                                                                             *
;*                                  RndInvXY                                   *
;*                                                                             *
;*         Randomly invert the x and y components of a position vector         *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Randomly inverts the x and y components of a position vector (x and y
; coordinates). See also subroutine InitPosVec ($B764).
;
; INPUT
;
;   X = Position vector index. Used values are: 0..48.


;======================================
;
;======================================
RndInvXY        .proc
                lda RANDOM              ; Set sign of y-coordinate randomly
                and #$01                ;
                sta YPOSSIGN,X          ;
                bne _1                  ; Skip if sign positive

                sec                     ; Sign negative -> Calc negative y-coordinate
                sbc YPOSLO,X            ; (calculate two's-complement of 16-bit value)
                sta YPOSLO,X            ;
                lda #0                  ;
                sbc YPOSHI,X            ;
                sta YPOSHI,X            ;

_1              lda RANDOM              ; Set sign of x-coordinate randomly
                and #$01                ;
                sta XPOSSIGN,X          ;
                bne _XIT                ; Skip if sign positive

                sec                     ; Sign negative -> Calc negative x-coordinate
                sbc XPOSLO,X            ; (calculate two's-complement of 16-bit value)
                sta XPOSLO,X            ;
                lda #0                  ;
                sbc XPOSHI,X            ;
                sta XPOSHI,X            ;
_XIT            rts
                .endproc
