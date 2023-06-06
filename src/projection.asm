
;*******************************************************************************
;*                                                                             *
;*                                 PROJECTION                                  *
;*                                                                             *
;*         Calculate pixel column (or row) number from position vector         *
;*                                                                             *
;*******************************************************************************

; Calculates the pixel column (or row) number of a position vector x (or y)
; component relative to the PLAYFIELD center by computing the perspective
; projection quotient
;
;     QUOTIENT := DIVIDEND / DIVISOR * 128
;
; with
;
;     DIVIDEND := ABS(x-coordinate (or y-coordinate)) / 2
;     DIVISOR  := ABS(z-coordinate) / 2  
;
; If the QUOTIENT is in 0..255, it is used as an index to pick the pixel column
; (or row) number from table MAPTO80 ($0DE9), returning values in 0..80.
;
; If the QUOTIENT is larger than 255 ("dividend overflow") or if the
; z-coordinate = 0 ("division by zero") then the error value 255 is returned.
;
; INPUT
;
;   X                   = Position vector index. Used values are: 0..48.
;   DIVIDEND ($6A..$6B) = Dividend (positive 16-bit value), contains the
;                         absolute value of the x (or y) coordinate.
;
; OUTPUT
;
;   A = Pixel column (or row) number relative to PLAYFIELD center. Used values
;       are: 
;     0..80 -> Pixel number
;     255   -> Error value indicating "dividend overflow" or "division by zero"

L_DIVISOR       = $68                   ; Divisor (16-bit value)
L_QUOTIENT      = $6D                   ; Division result (unsigned 8-bit value)
L_LOOPCNT       = $6E                   ; Division loop counter. Used values are: 7..0.


;======================================
; Calculate pixel column (or row) number
; from position vector
;======================================
PROJECTION      .proc
                lda #0                  ; Init quotient result
                sta L_QUOTIENT          ;

                lda #7                  ; Init division loop counter
                sta L_LOOPCNT           ;

                lsr DIVIDEND+1          ; DIVIDEND := x-coordinate (or y-coordinate) / 2
                ror DIVIDEND            ; (division by 2 to make B15 = 0?) (?)

                lda SHIPVIEW            ; Skip if in Aft view
                bne _1                  ;

                lda ZPOSHI,X            ; If in Front view -> DIVISOR := z-coordinate / 2
                lsr                     ; (division by 2 to make B15 = 0?) (?)
                sta L_DIVISOR+1         ;
                lda ZPOSLO,X            ;
                ror A                   ;
                sta L_DIVISOR           ;
                jmp _next1              ;

_1              sec                     ; If in Aft view -> DIVISOR := - z-coordinate / 2
                lda #0                  ; (division by 2 to make B15 = 0?) (?)
                sbc ZPOSLO,X            ;
                sta L_DIVISOR           ;
                lda #0                  ;
                sbc ZPOSHI,X            ;
                lsr                     ;
                sta L_DIVISOR+1         ;
                ror L_DIVISOR           ;

_next1          asl L_QUOTIENT          ; QUOTIENT := DIVIDEND / DIVISOR * 128
                sec                     ;
                lda DIVIDEND            ;
                sbc L_DIVISOR           ;
                tay                     ;
                lda DIVIDEND+1          ;
                sbc L_DIVISOR+1         ;
                bcc _2                  ;

                sta DIVIDEND+1          ;
                sty DIVIDEND            ;
                inc L_QUOTIENT          ;

_2              asl DIVIDEND            ;
                rol DIVIDEND+1          ;
                bcc _3                  ;

                lda #255                ; Return 255 if division by zero or dividend overflow
                rts                     ;

_3              dec L_LOOPCNT           ;
                bpl _next1              ; Next division loop iteration

                ldy L_QUOTIENT          ; Prep with quotient
                lda MAPTO80,Y           ; Pick and return pixel column (or row) number...
_XIT            rts                     ; ...relative to PLAYFIELD center
                .endproc
