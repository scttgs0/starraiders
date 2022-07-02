;*******************************************************************************
;*                                                                             *
;*                                  DRAWLINES                                  *
;*                                                                             *
;*                     Draw horizontal and vertical lines                      *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Draws the Attack Computer Display (in Front view), cross hairs (in Front and
; Aft view), and our starship's shape (in Long-Range Scan view) on the PLAYFIELD
; (if the Attack Computer is not destroyed) by being passed an offset to table
; DRAWLINESTAB ($BAF9). This table consists of a list of 3-byte elements,
; terminated by an end marker byte ($FE). Each such element defines a single
; horizontal or vertical line, and is passed via memory addresses DIRLEN ($A4),
; PENROW ($A5), and PENCOLUMN ($A6) to subroutine DRAWLINE ($A782), which
; executes the actual drawing. See subroutine DRAWLINE ($A782) and table
; DRAWLINESTAB ($BAF9) for a description of the 3-byte elements. 
;
; With every call of this subroutine the blip cycle counter is initialized to
; the start of the DELAY phase (see subroutine UPDATTCOMP ($A7BF)).
;
; NOTE: The entry to this subroutine is in mid-code, not at the beginning.
;
; INPUT
;
;   X = Offset into DRAWLINESTAB ($BAF9). Used values are:
;     $00 -> Draw Attack Computer Display and cross hairs (Front view)
;     $2A -> Draw Aft view cross hairs (Aft view)
;     $31 -> Draw our starship's shape (Long-Range Scan view)

LOOP028         sta DIRLEN,Y            ; Store byte of 3-byte element
                inx                     ;
                dey                     ;
                bpl SKIP048             ; Next byte of 3-byte element until 3 bytes copied
                jsr DRAWLINE            ; Draw line on PLAYFIELD

DRAWLINES       lda #5                  ; Init blip cycle to DELAY phase...
                sta BLIPCYCLECNT        ; ...delays drawing each row

                bit GCSTATCOM           ; Return if Attack Computer destroyed
                bvs SKIP049             ;

                ldy #2                  ;
SKIP048         lda DRAWLINESTAB,X      ; Load byte of 3-byte element
                cmp #$FE                ; Loop until end marker byte ($FE) encountered
                bne LOOP028             ;
SKIP049         rts                     ; Return

;*******************************************************************************
;*                                                                             *
;*                                  DRAWLINE                                   *
;*                                                                             *
;*                  Draw a single horizontal or vertical line                  *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Draws a single horizontal or vertical transparent line.
;
; There are two entries to this subroutine:
;
; (1)  DRAWLINE ($A782) is entered from subroutine DRAWLINES ($A76F) to draw a
;      line in COLOR1.
;
; (2)  DRAWLINE2 ($A784) is entered from subroutine UPDATTCOMP ($A7BF) to draw
;      the blip in COLOR2 in the Attack Computer Display.
;
; The position, direction, and length of the line is defined by three bytes
; passed in memory addresses DIRLEN ($A4), PENROW ($A5), and PENCOLUMN ($A6). 
;
; A drawing operation draws one transparent line. It uses both the color
; register number of the overwritten (old) and the overwriting (new) pixel to
; decide on the new pixel color register number. This results in a transparent
; drawing effect. See the table below for all resulting combinations of color
; registers.
;
; +-----------+---------------+
; |           |   Old Color   |
; |           |   Register    |
; | New Color +---------------+
; | Register  | 0 | 1 | 2 | 3 |
; +-----------+---+---+---+---+
; |         0 | 0 | 1 | 2 | 3 |
; +-----------+---+---+---+---+
; |         1 | 1 | 1 | 3 | 3 |
; +-----------+---+---+---+---+
; |         2 | 2 | 3 | 2 | 3 |
; +-----------+---+---+---+---+
; |         3 | 3 | 3 | 3 | 3 |
; +-----------+---+---+---+---+
;
; For example, COLOR1 overwritten by COLOR2 yields COLOR3. If you look closely
; at the blip (in COLOR2) on the Attack Computer Display (in COLOR1) the lines
; of the Attack Computer Display shine through (in COLOR3) where they overlap.
;
; INPUT
;
;   DIRLEN    ($A4) = B7 = 0 -> Draw line to the right
;                     B7 = 1 -> Draw line downward
;                     B6..0  -> Length of line in pixels
;   PENROW    ($A5) = Start pixel row number of line
;   PENCOLUMN ($A6) = Start pixel column number of line

L_PIXELBYTEOFF  = $6A                   ; Within-row-offset to byte with pixel in PLAYFIELD
L_BITPAT        = $6B                   ; 1-byte bit pattern for 4 pixels of same color
L_DIRSAV        = $6E                   ; Saves DIRLEN

DRAWLINE        lda #$55                ; Copy 1-byte bit pattern for 4 pixels of COLOR1
DRAWLINE2       sta L_BITPAT            ;
                lda DIRLEN              ; Copy direction (and length) of line
                sta L_DIRSAV            ;
                and #$7F                ; Strip direction bit
                sta DIRLEN              ; Store length of line

LOOP029         ldy PENROW              ; Loop over length of line to be drawn
                lda PFMEMROWLO,Y        ; Point MEMPTR to start of pen's pixel row...
                sta MEMPTR              ; ...in PLAYFIELD memory
                lda PFMEMROWHI,Y        ;
                sta MEMPTR+1            ;

                lda PENCOLUMN           ; Calc and store pen's byte-within-row offset
                lsr A                   ;
                lsr A                   ;
                sta L_PIXELBYTEOFF      ;

                lda PENCOLUMN           ; Calc pixel-within-byte index
                and #$03                ;
                tay                     ;

                lda PIXELMASKTAB,Y      ; Pick mask to filter pixel in byte
                and L_BITPAT            ; ...and with bit pattern for 4 pixels of same color
                ldy L_PIXELBYTEOFF      ;
                ora (MEMPTR),Y          ; Blend byte with new pixel and PLAYFIELD byte
                sta (MEMPTR),Y          ; ...and store it back in PLAYFIELD memory

                bit L_DIRSAV            ; Check direction bit B7
                bpl SKIP050             ;
                inc PENROW              ; If B7 = 1 -> Increment pen's pixel row number
                bne SKIP051             ;
SKIP050         inc PENCOLUMN           ; If B7 = 0 -> Increment pen's pixel column number

SKIP051         dec DIRLEN              ;
                bne LOOP029             ; Next pixel of line
                rts                     ; Return
