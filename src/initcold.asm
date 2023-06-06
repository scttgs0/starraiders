;*******************************************************************************
;*                                                                             *
;*                                  INITCOLD                                   *
;*                                                                             *
;*                        Initialize game (Cold start)                         *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Initializes the game, then continues into the game loop at GAMELOOP ($A1F3).
;
; There are four entry points to initialization:
;
; (1)  INITCOLD ($A14A) is entered at initial cartridge startup (cold start).
;      This initializes POKEY, resets the idle counter, sets the mission level
;      to NOVICE mission, and clears the function key code. POKEY is enabled to
;      receive keyboard input. Code execution continues into INITSELECT ($A15A)
;      below.
;
; (2)  INITSELECT ($A15A) is entered from GAMELOOP ($A1F3) after the SELECT
;      function key has been pressed. This loads the title phrase offset for the
;      copyright notice. Code execution continues into INITDEMO ($A15C) below. 
;
; (3)  INITDEMO ($A15C) is entered when the game switches into demo mode. This
;      loads the demo mode flag. Code execution continues into INITSTART ($A15E)
;      below.
;
; (4)  INITSTART ($A15E) is entered from GAMELOOP ($A1F3) after the START
;      function key has been pressed. This enqueues the new title phrase and
;      enables or disables demo mode, depending on the preloaded value.
;
; Initialization continues with the following steps: 
;
; (1)  Clear the custom chip registers and zero page game variables from
;      ISVBISYNC ($0067) on.
;
;      NOTE: Because of loop jamming there is a loop index overshoot. This
;      clears memory at $0067..$0166 instead of the game's zero page memory at
;      $0067..$00FB. However, this does no harm because memory at $0100..$0166
;      is - at this point in time - a yet unused part of the 6502 CPU stack
;      (memory addresses $0100..$01FF).
;
;      NOTE: At address $A175 a hack is necessary in the source code to force an
;      sta ISVBISYNC,X instruction with a 16-bit address operand, as opposed to
;      an 8-bit (zero page) address operand. The latter would be chosen by
;      virtually all 6502 assemblers, as ISVBISYNC ($0067) is located in the
;      zero page (memory addresses $0000..$00FF). The reason to force a 16-bit
;      address operand is the following: The instruction sta ISVBISYNC,X is used
;      in a loop which iterates the CPU's X register from 0 to 255 to clear
;      memory. By using this instruction with a 16-bit address operand
;      ("indexed, absolute" mode), memory at $0067..$0166 is cleared. Had the
;      code been using the same operation with an 8-bit address operand
;      ("indexed, zero page" mode), memory at $0067..$00FF would have been
;      cleared first, then the indexed address would have wrapped back to $0000
;      and cleared memory at $0000..$0066, thus effectively overwriting already
;      initialized memory locations.
;
; (2)  Initialize the 6502 CPU (reset the stack pointer, disable decimal mode).
;
; (3)  Clear game memory from $0200..$1FFF in subroutine CLRMEM ($AE0F).
;
; (4)  Set the address vectors of the IRQ, VBI, and DLI handlers.
;
; (5)  Enable input from Joystick 0.
;
; (6)  Enable Player/Missile graphics, providing a fifth PLAYER, and set
;      PLAYER-PLAYFIELD priority.
;
;      BUG (at $A1A6): The set PLAYER-PLAYFIELD priority arranges PLAYERs
;      (PL0..4) in front of the PLAYFIELD (PF0..4) in this specific order, from
;      front to back:
;
;          PL0 > PL1 > PL2 > PL3 > PL4 > PF0, PF1, PF2 > PF4 (BGR)
;
;      This makes sense as space objects represented by PLAYERs (for example,
;      Zylon ships, photon torpedoes, and meteors) move in front of the stars,
;      which are part of the PLAYFIELD. However, PLAYERs also move in front of
;      the cross hairs, which are also part of the PLAYFIELD. Suggested fix:
;      None, technically not possible.  
;
; (7)  Do more initialization in subroutine INITIALIZE ($B3BA).
;
; (8)  Set display to Front view.
;
; (9)  Show or hide the Control Panel Display (bottom text window) in subroutine
;      MODDLST ($ADF1), depending on the demo mode flag.
;
; (10) Initialize our starship's velocity equivalent to speed key '6'.
;
; (11) Enable the Display List.
;
; (12) Initialize the number of space objects to 16 (5 PLAYER space objects + 12
;      PLAYFIELD space objects (stars), counted 0..16).
;
; (13) Set the title phrase to the selected mission level in subroutine SETTITLE
;      ($B223).
;
; (14) Enable the IRQ, DLI, and VBI interrupts.
;
; Code execution continues into the game loop at GAMELOOP ($A1F3).

INITCOLD        lda #0                  ;
                sta SKCTL               ; POKEY: Initialization
                sta IDLECNTHI           ; Reset idle counter
                sta MISSIONLEVEL        ; Mission level := NOVICE mission
                sta FKEYCODE            ; Clear function key code
                lda #$03                ; POKEY: Enable keyboard scan and debounce
                sta SKCTL               ;

;*** Entry point when SELECT function key was pressed **************************
INITSELECT      ldy #$2F                ; Prep title phrase "COPYRIGHT ATARI 1979"

;*** Entry point when game switches into demo mode *****************************
INITDEMO        lda #$FF                ; Prep demo mode flag

;*** Entry point when START function key was pressed ***************************
INITSTART       sty NEWTITLEPHR         ; Enqueue new title phrase
                sta ISDEMOMODE          ; Store demo mode flag

;*** More initialization *******************************************************
                lda #0                  ; Clear custom chip registers, zero page variables
                tax                     ;
LOOP001         sta HPOSP0,X            ; Clear $D000..$D0FF (GTIA registers)
                sta DMACTL,X            ; Clear $D400..$D4FF (ANTIC registers)
                cpx #$0F                ;
                bcs SKIP001             ;
                sta AUDF1,X             ; Clear $D200..$D20E (POKEY registers)

SKIP001         sta PORTA,X             ; Clear $D300..$D3FF (PIA registers)
                                        ; Clear $0067..$0166 (zero page game variables)
                .byte $9D               ; HACK: Force ISVBISYNC,X with 16-bit address
                .word ISVBISYNC         ; (loop jamming)
                inx                     ;
                bne LOOP001             ;

                dex                     ; Reset 6502 CPU stack pointer
                txs                     ;

                cld                     ; Clear 6502 CPU decimal mode

                lda #$02                ; Clear $0200..$1FFF (game memory)
                jsr CLRMEM              ;

                lda #<IRQHNDLR          ; Set IRQ handler (VIMIRQ)
                sta VIMIRQ              ;
                lda #>IRQHNDLR          ;
                sta VIMIRQ+1            ;

                lda #<VBIHNDLR          ; Set VBI and DLI handler (VVBLKI and VDSLST)
                sta VVBLKI              ;
                lda #<DLSTHNDLR         ;
                sta VDSLST              ;
                lda #>VBIHNDLR          ;
                sta VVBLKI+1            ;
                lda #>DLSTHNDLR         ;
                sta VDSLST+1            ;

                lda #$04                ; PIA: Enable PORTA (Joystick 0)
                sta PACTL               ;
                lda #$11                ; GTIA: Enable PLAYER4, prio: PLs > PFs > BGR (!)
                sta PRIOR               ; (PLAYERs in front of stars - and cross hairs)
                lda #$03                ; GTIA: Enable DMA for PLAYERs and MISSILEs
                sta GRACTL              ;

                jsr INITIALIZE          ; Init Display List, tables, Galactic Chart, etc.

                ldx #$0A                ; Set Front view
                jsr SETVIEW             ;

                lda ISDEMOMODE          ; If in/not in demo mode hide/show...
                and #$80                ; ...Control Panel Display (bottom text window)
                tay                     ;
                ldx #$5F                ;
                lda #$08                ;
                jsr MODDLST             ;

                lda #32                 ; Init our starship's velocity (= speed key '6')
                sta NEWVELOCITY         ;

                lda #<DSPLST            ; ANTIC: Set Display List
                sta DLIST               ;
                lda #>DSPLST            ;
                sta DLIST+1             ;

                lda #$3E                ; ANTIC: Enable Display List DMA, single-line PM
                sta DMACTL              ; resolution, PM DMA, normal-width PLAYFIELD

                lda #0                  ; ANTIC: Set PM memory base address
                sta PMBASE              ;

                lda #NUMSPCOBJ_NORM-1   ; Set normal number of space objects
                sta MAXSPCOBJIND        ; (5 PLAYER spc objs + 12 PLAYFIELD spc objs (stars))

                ldx MISSIONLEVEL        ; Set title phrase
                ldy MISSIONPHRTAB,X     ; NOVICE, PILOT, WARRIOR, or COMMANDER MISSION
                jsr SETTITLE            ;

                lda #$40                ; POKEY: Enable keyboard interrupt (IRQ)
                sta IRQEN               ;

                cli                     ; Enable all IRQs

                lda #$C0                ; ANTIC: Enable DLI and VBI
                sta NMIEN               ;
