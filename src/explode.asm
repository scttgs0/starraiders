
;*******************************************************************************
;*                                                                             *
;*                                  INITEXPL                                   *
;*                                                                             *
;*                            Initialize explosion                             *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Initializes the explosion's lifetime, the explosion fragments' position and
; velocity vectors as well as their pixel row and column numbers.
;
; An explosion has a lifetime of 128 game loop iterations. It consists of 32
; explosion fragment space objects with indices 17..48. The position vector of
; each explosion fragment is copied from the exploding PLAYER space object.
;
; The pixel column number of each explosion fragment is initialized to
;
;     PIXEL COLUMN NUMBER := PLAYER column number - 48 + RND(0..15)
;
; To convert PLAYER column numbers (in Player/Missile (PM) pixels) into pixel
; column numbers, the PLAYER column number of the left PLAYFIELD border (= 48)
; is subtracted and a random number is added.
;
; BUG (at $AC76): The added random number should not be in 0..15 but in 0..7
; because the exploding PLAYER is 8 pixels wide. The PLAYER column number
; represents the left edge of the PLAYER shape. When using a random number in
; 0..15, half of the pixels are located off to the right of the PLAYER, outside
; the PLAYER area. Suggested fix: Replace instruction and #$0F with and #$07. 
;
; The pixel row number of each explosion fragment is initialized to
;
;     PIXEL ROW NUMBER := (PLAYER row number - RND(0..15)) / 2 - 16
;
; BUG (at $AC88): To convert PLAYER row numbers (in PM pixels) into pixel row
; numbers, the PLAYER row number to the top PLAYFIELD border (= 16) should be
; subtracted first, then the division by 2 (instruction LRS A) should be applied
; to reduce the double-line PM resolution to the single-line PLAYFIELD
; resolution. Suggested fix: Swap instruction LRS A with sbc #16 which leads to
; the following formula for the pixel row number:
;
;     PIXEL ROW NUMBER := (PLAYER row number - 16 + RND(0..15)) / 2
;
; Incidentally, adding a random number in 0..15 is correct. PLAYER row number
; represents the top edge of the PLAYER shape, which is typically 16 PM pixels
; tall when representing a close space object.
;
; The velocity vector of explosion fragments is set to random x, y, and z
; velocity vector components in -7..+7 <KM/H>.
;
; INPUT
;
;   Y = PLAYER index from which the explosion originates. Used values are:
;     0 -> Explosion of PLAYER0 (Zylon ship 0)
;     1 -> Explosion of PLAYER1 (Zylon ship 1)
;     2 -> Explosion of PLAYER2 (Zylon photon torpedo, starbase, or meteor)


;======================================
; Initialize explosion
;======================================
INITEXPL        .proc
                lda #128                ; Explosion lifetime := 128 game loops
                sta EXPLLIFE            ;

                ldx #NUMSPCOBJ_ALL-1    ; Max index of space objects (for explosion frags)
                stx MAXSPCOBJIND        ;

                                        ; Loop over all explosion fragment position vectors
                                        ; (index 48..17)
_next1          lda RANDOM              ; PIXEL COLUMN NUM := PLAYER column - 48 + RND(0..15)
                and #$0F                ; (!)
                adc PL0COLUMN,Y         ;
                sbc #48                 ;
                sta PIXELCOLUMN,X       ;

                lda RANDOM              ; PIXEL ROW NUM := (PLAYER row + RND(0..15)) / 2 - 16
                and #$0F                ;
                adc PL0ROWNEW,Y         ;
                lsr                     ; (!)
                sbc #16                 ;
                sta PIXELROWNEW,X       ;

                jsr CopyPositionVector          ; Copy position vector of PLAYER to explosion frag

                lda RANDOM              ; z-velocity := RND(-7..+7) <KM/H>
                and #NEG|7              ;
                sta ZVEL,X              ;
                lda RANDOM              ; x-velocity := RND(-7..+7) <KM/H>
                and #NEG|7              ;
                sta XVEL,X              ;
                lda RANDOM              ; y-velocity := RND(-7..+7) <KM/H>
                and #NEG|7              ;
                sta YVEL,X              ;

                dex                     ; Next explosion fragment position vector
                cpx #16                 ;
                bne _next1              ;
                rts
                .endproc


;*******************************************************************************
;*                                                                             *
;*                             CopyPositionVector                              *
;*                                                                             *
;*                           Copy a position vector                            *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Copies a position vector.
;
; Actually, this subroutine copies the z-coordinate only, then code execution
; continues into subroutine CopyPositionXY ($ACC1) to copy the x and y coordinate.
;
; INPUT
;
;   X = Destination position vector index. Used values are: 0..48.
;   Y = Source position vector index. Used values are: 0..48.


;======================================
; Copy a position vector
;======================================
CopyPositionVector .proc
                lda ZPOSSIGN,Y
                sta ZPOSSIGN,X

                lda ZPOSHI,Y
                sta ZPOSHI,X

                lda ZPOSLO,Y
                sta ZPOSLO,X

                .endproc

                ;[fall-through]


;*******************************************************************************
;*                                                                             *
;*                              CopyPositionXY                                 *
;*                                                                             *
;*          Copy x and y components (coordinates) of position vector           *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Copies the x and y components (coordinates) of a position vector.
;
; INPUT
;
;   X = Destination position vector index. Used values are: 0..48.
;   Y = Source position vector index. Used values are: 0..48.


;======================================
; Copy x and y components (coordinates)
; of position vector
;======================================
CopyPositionXY .proc
                lda XPOSSIGN,Y
                sta XPOSSIGN,X

                lda XPOSHI,Y
                sta XPOSHI,X

                lda YPOSSIGN,Y
                sta YPOSSIGN,X

                lda YPOSHI,Y
                sta YPOSHI,X

                lda XPOSLO,Y
                sta XPOSLO,X

                lda YPOSLO,Y
                sta YPOSLO,X

_XIT            rts
                .endproc
