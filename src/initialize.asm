
;*******************************************************************************
;*                                                                             *
;*                                 INITIALIZE                                  *
;*                                                                             *
;*                          More game initialization                           *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine executes the following initialization steps:
;
; (1)  Set up Display List
;
;      A Display List is created at DSPLST ($0280). It starts with 2 x 8 = 16
;      blank video lines, followed by 90 GRAPHICS7 rows. After a deliberate gap
;      in Display List instructions, which will be filled dynamically during the
;      game by calls to subroutine MODDLST ($ADF1), it ends with a Display List
;      wait-and-jump-back instruction to the start of the Display List at DSPLST
;      ($0280).
;
;      NOTE: The PLAYFIELD color table PFCOLORTAB ($BFA9) is copied to zero page
;      table PF0COLOR ($F2) by loop jamming.
;
; (2)  Create lookup tables
;
;      The first lookup table MAPTO80 ($0DE9) maps a byte value of 0..255 to
;      0..80. This table is used to map unsigned (absolute) position vector
;      components (coordinates) to pixel (or PLAYER) row and column numbers.
;
;      NOTE: The PLAYFIELD is 160 pixels wide. Pixel column numbers relative the
;      horizontal PLAYFIELD center are in -80..79, hence the range of this
;      lookup table. Pixel row numbers relative the vertical PLAYFIELD center
;      are in -50..49, thus they also fit in the range of this lookup table.
;
;      The second lookup table MAPTOBCD99 ($0EE9) maps a byte value of 0..255 to
;      a BCD-encoded value in 00..99. This table is used to convert byte values
;      into decimal 2-digit values displayed by the THETA (in "gradons"), PHI
;      (in "gradons"), RANGE (in "centrons"), and VELOCITY (in "metrons per
;      second") readouts of the Console Panel Display.
;
;      The third and fourth lookup tables accelerate drawing of PLAYFIELD space
;      objects: In combination they contain the 16-bit start addresses of each
;      of the 100 rows of PLAYFIELD memory. The low bytes of the 16-bit
;      addresses are stored in table PFMEMROWLO ($0800). The high bytes are
;      stored in table PFMEMROWHI ($0864).
;
;      NOTE: The address increment is 40 (40 bytes = 160 pixels in GRAPHICS7
;      mode = 1 PLAYFIELD row of pixels). 
;
;      NOTE: The PLAYFIELD consists of 90 GRAPHICS7 rows when the Control Panel
;      Display is displayed at the bottom. When the Control Panel Display is not
;      displayed, for example in demo mode, the PLAYFIELD contains additional
;      GRAPHICS7 rows.  
;
; (3)  Copy Control Panel Display and Galactic Chart Panel Display texts
;
;      The texts of the Control Panel Display and the Galactic Chart Panel
;      Display are copied to their respective locations in memory by loop
;      jamming.
;
; (4)  Initialize Zylon unit movement timer
;
;      The timer that triggers the movement of Zylon units in the Galactic Chart
;      is initialized to a value of 99. See subroutine FLUSHGAMELOOP ($B4E4) for
;      more information on Zylon unit movement.
;
; (5)  Create Galactic Chart
;
;      The Galactic Chart memory map GCMEMMAP ($08C9) is initialized. It
;      represents 16 columns x 8 rows of sectors. Each sector contains one of
;      the 4 sector types stored in table SECTORTYPETAB ($BBA6) (starbase, 4
;      Zylon ships, 3 Zylon ships, and 2 or 1 Zylon ships), and empty sectors.
;      Before distributing the sector types, the initial position of our
;      starship is blocked on the Galactic Chart at sector row 4, sector column
;      8, so that it will not be inadvertently occupied while other sector types
;      are distributed. The number of each of the sector types to be distributed
;      is the mission level plus 2. While Zylon units can be placed anywhere,
;      starbases are placed neither at the borders of the Galactic Chart nor in
;      a sector adjacent to an occupied sector. 
;
;      After having initialized the Galactic Chart memory map, the top border of
;      the Galactic Chart is drawn with characters from the custom character
;      set.
;
;      Finally, the sector in which our starship is located and the arrival and
;      departure hyperwarp marker column and row numbers are initialized.
;
; (6)  Apply a final tweak
;
;      The last entry of lookup table MAPTOBCD99 ($0EE9) is tweaked to a value
;      of CCS_INF * 16 + CCS_SPC. It is used to display an infinity symbol by
;      the RANGE readout of the Control Panel Display in subroutine SHOWCOORD
;      ($B8A7).
;
; Code execution continues into subroutine DrawGalacticChart ($B4B9), which draws
; the content of the Galactic Chart with characters from the custom character set.

L_MEMPTR1       = $68                   ; 16-bit memory pointer
L_MEMPTR2       = $6A                   ; 16-bit memory pointer
L_SECTORTYPE    = $6A                   ; Saves sector type. Used values are:
                                        ;   $CF -> Sector contains starbase
                                        ;   $04 -> Sector contains 4 Zylon ships
                                        ;   $03 -> Sector contains 3 Zylon ships
                                        ;   $02 -> Sector contains 2 or 1 Zylon ships
L_SECTORCNT     = $6B                   ; Saves number of sectors of the current sector type


;======================================
; Initialize Display List and copy
; color table
;======================================
INITIALIZE      .proc
                ldx #89                 ; Set 89(+1) GRAPHICS7 rows from DSPLST+5 on
_next1          lda #$0D                ; Prep DL instruction $0D (one row of GRAPHICS7)
                sta DSPLST+5,X          ; DSPLST+5,X := one row of GRAPHICS7
                cpx #10                 ;
                bcs _1                  ;
                lda PFCOLORTAB,X        ; Copy PLAYFIELD color table to zero page table
                sta PF0COLOR,X          ; (loop jamming)
_1              dex                     ;
                bpl _next1              ;

                lda #$70                ; DSPLST     := BLK8
                sta DSPLST              ; DSPLST+1   := BLK8
                sta DSPLST+1            ;
                lda #$41                ; DSPLST+103 := WAITJMP @ DSPLST
                sta DSPLST+103          ;
                lda #<DSPLST            ;
                sta DSPLST+104          ;
                lda #>DSPLST            ;
                sta DSPLST+105          ;

;*** Calculate lookup tables ***************************************************
                ldx #0                  ; Clear both 16-bit memory pointers
                stx L_MEMPTR1           ;
                stx L_MEMPTR1+1         ;
                stx L_MEMPTR2           ;
                stx L_MEMPTR2+1         ;

;*** Calc MAPTO80 map (converts value of $00..$FF to value in 0..80) ***********
_next2          clc                     ;
                lda L_MEMPTR1           ;
                adc #81                 ;
                sta L_MEMPTR1           ;
                lda L_MEMPTR1+1         ;
                sta MAPTO80,X           ;
                adc #0                  ;
                sta L_MEMPTR1+1         ;

;*** Calc MAPTOBCD99 map (converts value of $00..$FF to BCD-value in 00..99) ***
                clc                     ;
                lda L_MEMPTR2           ;
                adc #100                ;
                sta L_MEMPTR2           ;
                lda L_MEMPTR2+1         ;
                sta MAPTOBCD99,X        ;
                sed                     ;
                adc #0                  ;
                cld                     ;
                sta L_MEMPTR2+1         ;
                inx                     ;
                bne _next2              ;

;*** Calculate PLAYFIELD memory row addresses, copy Panel Display texts ********
                ldx #<PFMEM             ; Point L_MEMPTR1 to start of PLAYFIELD memory
                stx L_MEMPTR1           ; (X = 0, because PFMEM is at $1000)
                lda #>PFMEM             ;
                sta L_MEMPTR1+1         ;

_next3          clc                     ;
                lda L_MEMPTR1           ;
                sta PFMEMROWLO,X        ; Store 16-bit value of L_MEMPTR1 in PFMEMROWHI/LO
                adc #40                 ; Add 40 to L_MEMPTR
                sta L_MEMPTR1           ; (40 bytes = 160 pixels = 1 PLAYFIELD row)
                lda L_MEMPTR1+1         ;
                sta PFMEMROWHI,X        ;
                adc #0                  ;
                sta L_MEMPTR1+1         ;

                lda PANELTXTTAB,X       ; Copy Control and Galactic Chart Panel Display texts
                sta PANELTXT,X          ; (loop jamming)

                inx                     ;
                cpx #100                ;
                bcc _next3              ; Loop 100 times

;*** Set Zylon unit movement timer *********************************************
                dex                     ;
                stx ZYLONUNITTIM        ; Init Zylon unit movement timer to 99 game loops

;*** Create memory map of the Galactic Chart ***********************************
                ldx #3                  ; Loop over all 3(+1) sector types
                stx GCMEMMAP+4*16+8     ; Block our starship's initial position at center of
                                        ; ...Galactic Chart (sector row 4, sector column 8)

_next4          lda SECTORTYPETAB,X     ; Prep sector type
                sta L_SECTORTYPE        ;

                ldy MISSIONLEVEL        ; Number sectors of current type := mission level + 2
                iny                     ;
                iny                     ;
                sty L_SECTORCNT         ;

_next5          .frsRandomByte          ; Load random sector 0..127 from GC memory map
                and #$7F                ;
                tay                     ;
                lda GCMEMMAP,Y          ;
                bne _next5              ; If sector already occupied, pick another

                lda L_SECTORTYPE        ; Reload sector type
                bpl _2                  ; Skip if sector not to be occupied by starbase

                cpy #$10                ; Place starbase...
                bcc _next5              ; ...not in first sector row of Galactic Chart
                cpy #$70                ;
                bcs _next5              ; ...not in last sector row of Galactic Chart
                tya                     ;
                and #$0F                ;
                beq _next5              ; ...not in first sector column of Galactic Chart
                cmp #15                 ;
                beq _next5              ; ...not in last sector column of Galactic Chart
                lda GCMEMMAP-1,Y        ; ...not east  of an occupied sector
                ora GCMEMMAP+1,Y        ; ...not west  of an occupied sector
                ora GCMEMMAP+16,Y       ; ...not south of an occupied sector
                ora GCMEMMAP-16,Y       ; ...not north of an occupied sector
                bne _next5              ;

                lda L_SECTORTYPE        ; Reload sector type

_2              sta GCMEMMAP,Y          ; Store sector type in Galactic Chart memory map
                dec L_SECTORCNT         ;
                bpl _next5              ; Next sector
                dex                     ;
                bpl _next4              ; Next sector type

;*** Clear Galactic Chart and draw top border **********************************
                ldx #180                ; Clear Galactic Chart PLAYFIELD
_next6          lda #CCS_SPC            ;
                sta GCPFMEM-1,X         ;
                dex                     ;
                bne _next6              ;

                ldx #15                 ; Draw top border (15(+1) characters)
_next7          lda #CCS_BORDERS        ;
                sta GCPFMEM+2,X         ;
                dex                     ;
                bpl _next7              ;

                lda #CCS_CORNERSW       ; Draw NORTHEAST corner (1 character)
                sta GCPFMEM+18          ;

                lda #0                  ; Release starship's position at center of Galactic
                sta GCMEMMAP+4*16+8     ; ...Chart (sector row 4, sector column 8)

;*** Initialize current sector and hyperwarp marker column and row numbers *****
                lda #$48                ; Place our starship's current sector at
                sta CURRSECTOR          ; ...sector row 4, sector column 8
                lda #$43                ; Init departure & arrival hyperwarp marker column
                sta WARPDEPRCOLUMN      ;
                sta WARPARRVCOLUMN      ;
                lda #$47                ; Init departure & arrival hyperwarp marker row
                sta WARPARRVROW         ;
                sta WARPDEPRROW         ;

;*** Tweak last entry of MAPTOBCD99 ********************************************
                lda #CCS_INF*16+CCS_SPC ; Last entry of MAPTOBCD99: 'INFINITY'+'SPACE' char
                sta MAPTOBCD99+255      ;

                .endproc

                ;[fall-through]
