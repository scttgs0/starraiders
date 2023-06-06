;*******************************************************************************
;*                                                                             *
;*                                   MODDLST                                   *
;*                                                                             *
;*                             Modify Display List                             *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Modifies the Display List to show and hide title, headers, and the Control
; Panel Display.
;
; INPUT
;
;   A = Number of bytes to copy into the Display List
;   X = Offset into Display List DSPLST ($0280)
;   Y = Offset into Display List fragment table DLSTFRAG ($BA62). If Y = $80
;       then no bytes are copied but the specified locations of the Display List
;       are overwritten with Display List instruction $0D (one row of
;       GRAPHICS7).
;
;   Used values are:
;
;    A    X    Y
;   $08  $5F  $00 -> Show Control Panel Display (bottom text window)
;   $08  $5F  $80 -> Hide Control Panel Display (bottom text window)
;   $07  $0F  $23 -> Show title line
;   $07  $0F  $80 -> Hide title line
;   $08  $02  $1B -> Show Display List header line of Front view
;   $08  $02  $13 -> Show Display List header line of Aft view
;   $08  $02  $0B -> Show Display List header line of Long-Range Scan view
;   $08  $02  $08 -> Show Display List header line of Galactic Chart view

L_NUMBYTES      = $6A                   ; Number of bytes to copy

MODDLST         sei                     ; Disable IRQ
                sta L_NUMBYTES          ; Save number of bytes to copy

LOOP043         lda VCOUNT              ; Wait for ANTIC line counter >= 124 (PLAYFIELD...
                cmp #124                ; ...bottom) before changing the Display List
                bcc LOOP043             ;

LOOP044         lda DLSTFRAG,Y          ; Load byte from Display List fragment table
                iny                     ;
                bpl SKIP123             ; Skip if fragment table index < $80
                lda #$0D                ; Prep Display List instruction $0D (GRAPHICS7)
SKIP123         sta DSPLST,X            ; Store byte in Display List
                inx                     ;
                dec L_NUMBYTES          ;
                bne LOOP044             ; Copy next byte

                cli                     ; Enable IRQ
                rts                     ; Return

;*******************************************************************************
;*                                                                             *
;*                                CLRPLAYFIELD                                 *
;*                                                                             *
;*                           Clear PLAYFIELD memory                            *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Clears PLAYFIELD memory from $1000 to $1FFF.
;
; This subroutine sets the start address of the memory to be cleared then code
; execution continues into subroutine CLRMEM ($AE0F) where the memory is
; actually cleared.

CLRPLAYFIELD    lda #$10

;*******************************************************************************
;*                                                                             *
;*                                   CLRMEM                                    *
;*                                                                             *
;*                                Clear memory                                 *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Clears memory from a given start address to memory address $1FFF. This
; subroutine is called in the following situations:
;
; (1)  In routine INITCOLD ($A14A) at the beginning of the game to initialize
;      the game's variables 
;
; (2)  In subroutine CLRPLAYFIELD ($AE0D) to clear PLAYFIELD memory.
;
; As a side effect this subroutine also clears the saved number of space objects
; and the lock-on flag.
;
; INPUT
;
;   A = Start address (high byte) of memory to be cleared. Used values are:
;     $02 -> Clear memory $0200..$1FFF during game initialization
;     $10 -> Clear PLAYFIELD memory $1000..$1FFF

CLRMEM          sta MEMPTR+1            ; Store start address (high byte) to be cleared
                lda #0                  ; Store start address (low byte) to be cleared
                tay                     ;
                sta MEMPTR              ;

                sta ISINLOCKON          ; Clear lock-on flag
                sta OLDMAXSPCOBJIND     ; Clear saved number of space objects

LOOP045         sta (MEMPTR),Y          ; Clear memory location
                iny                     ;
                bne LOOP045             ;

                inc MEMPTR+1            ; Next page (= 256-byte block)
                ldy MEMPTR+1            ;
                cpy #$20                ;
                tay                     ;
                bcc LOOP045             ; Loop until memory address $2000 reached
                rts                     ; Return
