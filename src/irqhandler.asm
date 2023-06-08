
;*******************************************************************************
;*                                                                             *
;*                                  VBIHNDLR                                   *
;*                                                                             *
;*                      Vertical Blank Interrupt Handler                       *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine is executed during the Vertical Blank Interrupt (VBI) when the
; TV beam has reached the bottom-right corner of the TV screen and is switched
; off to return to the top-left position. This situation is called the "vertical
; blank phase".
;
; This subroutine signals its execution with flag ISVBISYNC ($67) (which is
; examined by GAMELOOP ($A1F3) to synchronize the execution of the game loop
; with the start of this subroutine). Then it switches the character set to the
; ROM character set, sets the BACKGROUND color depending on the severity of a
; Zylon photon torpedo hit and view mode, copies PLAYER and PLAYFIELD color
; registers to their corresponding hardware registers, clears the Player/Missile
; collision registers, calls the sound effects code in subroutine SOUND ($B2AB),
; and increments the idle counter. If the idle counter reaches the value $8000
; the title phrase is cleared and the game is switched to demo mode.
;
; BUG (at $A6EC): Because the values of SHIPVIEW ($D0) are $00, $01, $40, and
; $80, a value of 3 overspecifies the comparison. Suggested fix: Replace cmp #3
; with cmp #2, which may make the code clearer.
;
; BUG (at $A712): Demo mode is entered via a jmp instruction, which proceeds
; directly into GAMELOOP ($A1F3). Thus code execution never returns to pop the
; registers pushed on the stack during entry of this subroutine. Suggested fix:
; None.

VBIHNDLR        ;.proc
                lda #$FF                ; Signals entering Vertical Blank Interrupt
                sta ISVBISYNC           ;

                ldx BGRCOLOR            ; Preload BACKGROUND color
                .randomByte             ; Preload random number
                bit HITBADNESS          ; Check if our starship was hit
                bvc _2                  ; If HITBADNESS has a value of...
                bmi _1                  ; $00 -> NO HIT             (BGR color := unchanged)

                and #$72                ; $7F -> SHIELDS HIT        (BGR color := %01rr00r0)
                ora #$40                ; $FF -> STARSHIP DESTROYED (BGR color := %01rr00r0)
_1              tax                     ;
_2              lda SHIPVIEW            ; Skip if in Front or Aft view
                cmp #3                  ; (!)
                bcc _3                  ;

                ldx #$A0                ; Preload BACKGROUND color {DARK BLUE GREEN}...
_3              stx BGRCOLOR            ; Store BACKGROUND color

                ldx #8                  ; Copy all color registers to hardware registers
_next1          lda PL0COLOR,X          ;
                ;--sta COLPM0,X            ;
                dex                     ;
                bpl _next1              ;

                ;--sta HITCLR              ; Clear Player/Missile collision registers

                jsr SOUND               ; Call sound effects

                inc IDLECNTLO           ; Increment 16-bit idle counter
                bne _XIT                ;
                lda IDLECNTHI           ;
                bmi _XIT                ;
                inc IDLECNTHI           ;
                bpl _XIT                ; Skip if idle counter value of $8000 not reached yet

                ldy #$00                ; Prep empty title phrase offset
                jmp InitGame.DEMO       ; Enter demo mode (!)

_XIT            jmp DLSTHNDLR._XIT      ; Return via DLI return code

                ;.endproc


;*******************************************************************************
;*                                                                             *
;*                                  DLSTHNDLR                                  *
;*                                                                             *
;*                       Display List Interrupt Handler                        *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine is executed during the Display List Interrupt (DLI). It
; switches the character set to the ROM character set if the DLI occurs at ANTIC
; line 96 (video line 192), otherwise to the custom character set. The former
; happens in the Galactic Chart view where the ROM character set is used in the
; Galactic Chart Panel Display.
;
; Then, the DLI PLAYFIELD colors are copied to the corresponding hardware
; registers and the values of the collision hardware registers for PLAYER3..4
; (our starship's photon torpedoes) are copied to the corresponding zero page
; variables PL3HIT ($82) and PL4HIT ($83).

DLSTHNDLR       ;.proc
                pha                     ; Push A
                txa                     ;
                pha                     ; Push X
                tya                     ;
                pha                     ; Push Y

                ldx #4                  ; Loop over all PLAYFIELD colors
                ;--sta WSYNC               ; Stop and wait for horizontal TV beam sync
_next1          lda PF0COLORDLI,X       ; Copy DLI PLAYFIELD colors to hardware registers
                ;--sta COLPF0,X            ;
                dex                     ;
                bpl _next1              ; Next PLAYFIELD color

                ;--lda M0PL                ; Merge MISSILE-to-PLAYER collision registers...
                ;--ora M1PL                ;
                ;--ora M2PL                ;
                ;--ora M3PL                ;
                sta PL4HIT              ; ...and store them in PL4HIT
                ;--lda P3PL                ; Copy PLAYER3-to-PLAYER coll. register to PL3HIT
                sta PL3HIT              ;

_XIT            pla                     ; Pop Y
                tay                     ;
                pla                     ; Pop X
                tax                     ;
                pla                     ; Pop A
                rti                     ; Return from interrupt
                ;.endproc


;*******************************************************************************
;*                                                                             *
;*                                  IRQHNDLR                                   *
;*                                                                             *
;*                       Interrupt Request (IRQ) Handler                       *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine is executed during immediate interrupt requests (IRQs), such
; as after pressing a key on the keyboard. It clears and disables all IRQs
; except the interrupt raised by a pressed key. If a key has been pressed, its
; hardware code is collected and the bits of the SHIFT and CONTROL keys are
; added. The resulting keyboard code is stored in KEYCODE ($CA).

IRQHNDLR        ;.proc
                pha                     ; Push A
                lda #0                  ; POKEY: Disable all IRQs
                ;--sta IRQEN               ;
                lda #$40                ; POKEY: Enable keyboard interrupt (IRQ)
                ;--sta IRQEN               ;
                ;--lda KBCODE              ; POKEY: Load keyboard key code
                ora #$C0                ; Combine with SHIFT and CONTROL key bits
                sta KEYCODE             ; Store keyboard code
                pla                     ; Pop A
                rti                     ; Return from interrupt
                ;.endproc
