
;*******************************************************************************
;*                                                                             *
;*                                 UPDATTCOMP                                  *
;*                                                                             *
;*                       Update Attack Computer Display                        *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Draws the blip of the tracked space object and the lock-on markers into the
; Attack Computer Display. The actual drawing follows a cycle of 11 game loop
; iterations (numbered by this subroutine as "blip cycles" 0..10), which can be
; divided into three phases:
;
; (1)  Blip cycle 0..4: Draw blip shape row-by-row
;
;      Draw the blip's shape into the Attack Computer Display, one row each blip
;      cycle. After 5 blip cycles the blip shape is complete and completely
;      visible because between blip cycles, that is, game loop iterations, the
;      PLAYFIELD is not erased (only the PLAYFIELD space objects are). Drawing
;      is executed by branching to entry DrawLine._ENTRY1 ($A784) of subroutine
;      DrawLine ($A782). The blip shape is retrieved from table BLIPSHAPTAB
;      ($BF6E).
;
; (2)  Blip cycle 5..9: Delay
;
;      Delay the execution of blip cycle 10.
;
; (3)  Blip cycle 10: Update Attack Computer Display
;
;      After verifying that the tracked space object is alive, calculate the
;      blip's relative top-left pixel column and row number. The resulting
;      values are in -11..11 and -6..4, relative to the blip's top-left
;      reference position at pixel column number 131 and pixel row number 77,
;      respectively. 
;
;      Filter the Attack Computer Display area: Only pixels of COLOR1 within the
;      inner frame area (a 28 pixel wide x 15 pixel high rectangle with its
;      top-left corner at pixel column number 120 and pixel row number 71) pass
;      the filter operation. This effectively erases the blip.
;
;      If the blip is within -2..+2 pixels off its horizontal reference position
;      (pixel column numbers 129..132) then the tracked space object is in x
;      lock-on. Draw the x lock-on marker. 
;
;      If the tracked space object is in x lock-on and the blip is within -2..+1
;      pixels off its vertical reference position (pixel column numbers 75..78)
;      then the tracked space object is in x and y lock-on. Draw also the y
;      lock-on marker. 
;
;      If the tracked space object is in x and y lock-on and the tracked space
;      object's z-coordinate < +3072 (+$0C**) <KM> then the tracked space object
;      is in x, y and z lock-on. Draw also the z lock-on marker. 
;
;      If the tracked space object is in x, y, and z lock-on (and thus in
;      optimal firing range) set the ISINLOCKON ($A3) flag.
;
;      The following sketches show the Attack Computer Display area overlaid
;      with the Attack Computer Display frame:
;
;          119                                 119
;      70 ##############################    70 ##############################
;         #         ....#....          #       #             #              #
;         #         ....#....          #       #             #              #
;         #         ....#....          #       #             #              #
;         #         ....#....          #       #             #              #
;         #      ###############       #       #......###############.......#
;         #XXXX  #  .........  #   XXXX#       #......#.............#.......#
;         #      #  ..$......  #       #       #......#....$........#.......#
;         ########  .........  #########       ########.............#########
;         #      #  .........  #       #       #......#.............#.......#
;         #      #  .........  #       #       #YYYY..#.............#...YYYY#
;         #      ###############       #       #......###############.......#
;         #         ....#....          #       #.............#..............#
;         #         ....#....          #       #             #              #
;         #         ....#....          #       #             #              #
;         #         ....#....          #       #             #              #
;         ##############################       ##############################
;
;         X = x lock-on marker                 Y = y lock-on marker
;         . = x lock-on blip zone              . = y lock-on blip zone
;         $ = Blip's top-left reference        $ = Blip's top-left reference
;             position                             position
;
;         119
;      70 ##############################
;         #             #              #
;         #             #              #
;         #             #              #
;         #             #              #
;         #      ###############       #
;         #      #             #       #
;         #      #    $        #       #
;         ########             #########
;         #      #             #       #
;         #      #             #       #
;         #      ###############       #
;         #             #              #
;         #             #              #
;         #        ZZ   #  ZZ          #
;         #        ZZ   #  ZZ          #
;         ##############################
;
;         Z = z lock-on marker
;         $ = Blip's top-left reference
;             position

L_SHIFTSHAP     = $6C                   ; Saves shifted byte of blip shape bit pattern


;======================================
; Update Attack Computer Display
;======================================
UPDATTCOMP      .proc
                ldx TRACKDIGIT          ; Load index of tracked space object
                ldy BLIPCYCLECNT        ; Load blip cycle counter
                cpy #5                  ;
                bcs _2                  ; Skip drawing blip if blip cycle > 5

;*** Blip cycle 0..4: Draw blip shape one row each cycle ***********************
                lda BLIPCOLUMN          ; Init pen's pixel column number...
                sta PENCOLUMN           ; ...with top position of blip shape
                lda BLIPSHAPTAB,Y       ; Load bit pattern of one row of blip shape
_next1          asl                     ; Shift bit pattern one position to the left
                sta L_SHIFTSHAP         ; Temporarily save shifted shape byte
                bcc _1                  ; Skip if shifted-out bit = 0

                lda #$81                ; Store "draw a line of 1 pixel length downward"
                sta DIRLEN              ; ...for call to DrawLine._ENTRY1

                lda BLIPROW             ; Init pen's pixel row number...
                sta PENROW              ; ...with leftmost position of blip shape
                lda #$AA                ; Load 1-byte bit pattern for 4 pixels of COLOR2
                jsr DrawLine._ENTRY1    ; Draw pixel on PLAYFIELD

_1              inc PENCOLUMN           ; Move pen one pixel to the right
                lda L_SHIFTSHAP         ; Reload shifted shape byte
                bne _next1              ; Next horizontal pixel of blip shape

                inc BLIPROW             ; Move pen one pixel downward
_next2          inc BLIPCYCLECNT        ; Increment blip cycle counter
                rts

;*** Blip cycle 5..9: Delay ****************************************************
_2              cpy #10                 ; Return if blip cycle < 10
                bcc _next2              ;

;*** Blip cycle 10: Calculate new blip pixel row and column numbers ************
                lda PL0LIFE,X           ; Skip if tracked object not alive
                beq SKIP059             ;

                lda XPOSHI,X            ; Map x-coordinate of tracked space obj to -11..11:
                ldy XPOSSIGN,X          ; Skip if tracked object on left screen half (x >= 0)
                beq _3                  ;

                cmp #12                 ; Skip if x of tracked obj < +$0C** (< 3327) <KM>
                bcc _4                  ;
                lda #11                 ; Prep relative pixel column number of 11, skip
                bpl _4                  ;

_3              cmp #-11                ; Skip if x of tracked obj >= -($0B**) (>=-2816) <KM>
                bcs _4                  ;
                lda #-11                ; Prep relative pixel column number of -11

_4              clc                     ; Add 131 (= blip's top-left reference pixel column)
                adc #131                ;
                sta BLIPCOLUMN          ; BLIPCOLUMN := 131 + -11..11

                lda YPOSHI,X            ; Map y-coordinate of tracked space obj to -6..4:
                eor #$FF                ; Mirror y-coordinate on y-axis (displacement of +1)
                ldy YPOSSIGN,X          ; Skip if tracked obj on lower screen half (y < 0)
                bne _5                  ;

                cmp #5                  ; Skip if mirrored y of tracked obj < +$05** <KM>
                bcc _6                  ;
                lda #4                  ; Prep relative pixel row number of 4, skip
                bpl _6                  ;

_5              cmp #-6                 ; Skip if mirrored y of tracked obj >= -($06**) <KM>
                bcs _6                  ;
                lda #-6                 ; Prep relative pixel row number of -6

_6              clc                     ; Add 77 (= blip's top-left ref. pixel row number)
                adc #77                 ;
                sta BLIPROW             ; BLIPROW := 77 + -6..4

                lda #0                  ; Reset blip cycle
                sta BLIPCYCLECNT        ;

;*** Filter Attack Computer Display frame area *********************************
                                        ; PLAYFIELD address of top-left of Attack Computer
PFMEM_C120R71   = PFMEM+71*40+120/4     ; Display's inner frame @ pixel column 120, row 71

SKIP059         lda #<PFMEM_C120R71     ; Point MEMPTR to start of frame's...
                sta MEMPTR              ; ...inner top-left corner at column 120, row 71...
                lda #>PFMEM_C120R71     ; ...in PLAYFIELD memory
                sta MEMPTR+1            ;

                ldx #14                 ; Traverse a 28 x 15 pixel rect of PLAYFIELD memory
_next1          ldy #6                  ;
_next2          lda (MEMPTR),Y          ; Load byte (4 pixels) from PLAYFIELD memory
                and #$55                ; Filter COLOR1 pixels
                sta (MEMPTR),Y          ; Store byte (4 pixels) back to PLAYFIELD memory
                dey                     ;
                bpl _next2              ; Next 4 pixels in x-direction

                clc                     ; Add 40 to MEMPTR
                lda MEMPTR              ; (40 bytes = 160 pixels = 1 PLAYFIELD row of pixels)
                adc #40                 ;
                sta MEMPTR              ;
                bcc _1                  ;
                inc MEMPTR+1            ;

_1              dex                     ;
                bpl _next1              ; Next row of pixels in y-direction

;*** Prepare lock-on marker checks *********************************************
                ldx TRACKDIGIT          ; Preload index of tracked space obj to check z-range
                iny                     ; Y := 0, preloaded value of ISINLOCKON

;*** Draw lock-on markers ******************************************************
                                        ; PLAYFIELD addresses of
PFMEM_C120R76   = PFMEM+76*40+120/4     ; ...x lock-on marker @ pixel column 120, row 76
PFMEM_C144R76   = PFMEM+76*40+144/4     ; ...x lock-on marker @ pixel column 144, row 76
PFMEM_C120R80   = PFMEM+80*40+120/4     ; ...y lock-on marker @ pixel column 120, row 80
PFMEM_C144R80   = PFMEM+80*40+144/4     ; ...y lock-on marker @ pixel column 144, row 80
PFMEM_C128R84   = PFMEM+84*40+128/4     ; ...z lock-on marker @ pixel column 128, row 84
PFMEM_C128R85   = PFMEM+85*40+128/4     ; ...z lock-on marker @ pixel column 128, row 85
PFMEM_C136R84   = PFMEM+84*40+136/4     ; ...z lock-on marker @ pixel column 136, row 84
PFMEM_C136R85   = PFMEM+85*40+136/4     ; ...z lock-on marker @ pixel column 136, row 85

                lda LOCKONLIFE          ; If lock-on lifetime expired redraw lock-on markers
                beq _2                  ;

                dec LOCKONLIFE          ; else decrem. lock-on lifetime, skip drawing markers
                bne _3                  ;

_2              lda BLIPCOLUMN          ; Skip x, y, and z lock-on marker if blip's...
                cmp #129                ; ...top-left pixel column number not in 129..132
                bcc _3                  ;
                cmp #133                ;
                bcs _3                  ;

                lda #$AA                ; Draw x lock-on marker (4 horiz. pixels of COLOR2)
                sta PFMEM_C120R76       ; ...at pixel column 120, row 76
                sta PFMEM_C144R76       ; ...at pixel column 144, row 76

                lda BLIPROW             ; Skip y and z lock-on marker if blip's...
                cmp #75                 ; ...top-left pixel row number not in 75...78
                bcc _3                  ;
                cmp #79                 ;
                bcs _3                  ;

                lda #$AA                ; Draw y lock-on marker (4 horiz. pixels of COLOR2)
                sta PFMEM_C120R80       ; ...at pixel column 120, row 80
                sta PFMEM_C144R80       ; ...at pixel column 144, row 80

                lda ZPOSHI,X            ; Skip z lock-on marker if z >= +$0C** (>= 3072) <KM>
                cmp #12                 ;
                bcs _3                  ;

                ldy #$A0                ; Draw z lock-on marker (2 horiz. pixels of COLOR2)
                sty PFMEM_C128R84       ; ...at pixel column 128, row 84 (prep lock-on flag)
                sty PFMEM_C128R85       ; ...at pixel column 128, row 85
                sty PFMEM_C136R84       ; ...at pixel column 136, row 84
                sty PFMEM_C136R85       ; ...at pixel column 136, row 85

_3              sty ISINLOCKON          ; Store lock-on flag (> 0 -> Tracked obj locked on)
                rts
                .endproc
