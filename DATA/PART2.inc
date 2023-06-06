
;*******************************************************************************
;*                                                                             *
;*                G A M E   D A T A   ( P A R T   2   O F   2 )                *
;*                                                                             *
;*******************************************************************************

;*** Color register offsets of PLAYER0..4 **************************************
PLCOLOROFFTAB   .byte 0                               ; PLAYER0
                .byte 1                               ; PLAYER1
                .byte 2                               ; PLAYER2
                .byte 3                               ; PLAYER3
                .byte 7                               ; PLAYER4

;*** Shape table 1 (PLAYER2..4) ************************************************
PLSHAP1TAB      .byte $00                             ; ........
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $76                             ; .###.##.
                .byte $F7                             ; ####.###
                .byte $DF                             ; ##.#####
                .byte $DF                             ; ##.#####
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $F7                             ; ####.###
                .byte $76                             ; .###.##.
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $7C                             ; .#####..
                .byte $7C                             ; .#####..
                .byte $FE                             ; #######.
                .byte $DE                             ; ##.####.
                .byte $DA                             ; ##.##.#.
                .byte $FA                             ; #####.#.
                .byte $EE                             ; ###.###.
                .byte $EE                             ; ###.###.
                .byte $7C                             ; .#####..
                .byte $7C                             ; .#####..
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $6E                             ; .##.###.
                .byte $7A                             ; .####.#.
                .byte $7E                             ; .######.
                .byte $76                             ; .###.##.
                .byte $7E                             ; .######.
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $7C                             ; .#####..
                .byte $74                             ; .###.#..
                .byte $7C                             ; .#####..
                .byte $6C                             ; .##.##..
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....
                .byte $10                             ; ...#....
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $2C                             ; ..#.##..
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $08                             ; ....#...
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $28                             ; ..#.#...
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $24                             ; ..#..#..
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $5A                             ; .#.##.#.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $42                             ; .#....#.
                .byte $42                             ; .#....#.
                .byte $42                             ; .#....#.
                .byte $42                             ; .#....#.
                .byte $42                             ; .#....#.
                .byte $42                             ; .#....#.
                .byte $1C                             ; ...###..
                .byte $1C                             ; ...###..
                .byte $14                             ; ...#.#..
                .byte $3E                             ; ..#####.
                .byte $3E                             ; ..#####.
                .byte $3E                             ; ..#####.
                .byte $2A                             ; ..#.#.#.
                .byte $7F                             ; .#######
                .byte $7F                             ; .#######
                .byte $22                             ; ..#...#.
                .byte $22                             ; ..#...#.
                .byte $22                             ; ..#...#.
                .byte $22                             ; ..#...#.
                .byte $22                             ; ..#...#.
                .byte $18                             ; ...##...
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $24                             ; ..#..#..
                .byte $24                             ; ..#..#..
                .byte $24                             ; ..#..#..
                .byte $24                             ; ..#..#..
                .byte $10                             ; ...#....
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $7C                             ; .#####..
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $18                             ; ...##...
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....
                .byte $18                             ; ...##...
                .byte $7E                             ; .######.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $E7                             ; ###..###
                .byte $E7                             ; ###..###
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $00                             ; ........
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $E7                             ; ###..###
                .byte $66                             ; .##..##.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $00                             ; ........
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $E7                             ; ###..###
                .byte $66                             ; .##..##.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $E7                             ; ###..###
                .byte $66                             ; .##..##.
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $7E                             ; .######.
                .byte $3C                             ; ..####..
                .byte $00                             ; ........
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $FF                             ; ########
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $EE                             ; ###.###.
                .byte $00                             ; ........
                .byte $00                             ; ........
                .byte $EE                             ; ###.###.
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...
                .byte $28                             ; ..#.#...

;*** Shape table 2 (PLAYER0..1) ************************************************
PLSHAP2TAB      .byte $00                             ; ........
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $BD                             ; #.####.#
                .byte $FF                             ; ########
                .byte $FF                             ; ########
                .byte $BD                             ; #.####.#
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $82                             ; #.....#.
                .byte $82                             ; #.....#.
                .byte $BA                             ; #.###.#.
                .byte $FE                             ; #######.
                .byte $FE                             ; #######.
                .byte $BA                             ; #.###.#.
                .byte $82                             ; #.....#.
                .byte $82                             ; #.....#.
                .byte $42                             ; .#....#.
                .byte $5A                             ; .#.##.#.
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $5A                             ; .#.##.#.
                .byte $42                             ; .#....#.
                .byte $44                             ; .#...#..
                .byte $54                             ; .#.#.#..
                .byte $7C                             ; .#####..
                .byte $7C                             ; .#####..
                .byte $54                             ; .#.#.#..
                .byte $44                             ; .#...#..
                .byte $24                             ; ..#..#..
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $24                             ; ..#..#..
                .byte $28                             ; ..#.#...
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $28                             ; ..#.#...
                .byte $18                             ; ...##...
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $10                             ; ...#....
                .byte $E0                             ; ###.....
                .byte $F8                             ; #####...
                .byte $F8                             ; #####...
                .byte $FE                             ; #######.
                .byte $57                             ; .#.#.###
                .byte $FE                             ; #######.
                .byte $F8                             ; #####...
                .byte $F8                             ; #####...
                .byte $C0                             ; ##......
                .byte $C0                             ; ##......
                .byte $F0                             ; ####....
                .byte $C0                             ; ##......
                .byte $F0                             ; ####....
                .byte $F0                             ; ####....
                .byte $FC                             ; ######..
                .byte $BE                             ; #.#####.
                .byte $FC                             ; ######..
                .byte $F0                             ; ####....
                .byte $80                             ; #.......
                .byte $80                             ; #.......
                .byte $C0                             ; ##......
                .byte $C0                             ; ##......
                .byte $F0                             ; ####....
                .byte $BC                             ; #.####..
                .byte $F0                             ; ####....
                .byte $C0                             ; ##......
                .byte $07                             ; .....###
                .byte $1F                             ; ...#####
                .byte $1F                             ; ...#####
                .byte $7F                             ; .#######
                .byte $EA                             ; ###.#.#.
                .byte $7F                             ; .#######
                .byte $1F                             ; ...#####
                .byte $1F                             ; ...#####
                .byte $03                             ; ......##
                .byte $03                             ; ......##
                .byte $0F                             ; ....####
                .byte $03                             ; ......##
                .byte $0F                             ; ....####
                .byte $0F                             ; ....####
                .byte $3F                             ; ..######
                .byte $7D                             ; .#####.#
                .byte $3F                             ; ..######
                .byte $0F                             ; ....####
                .byte $01                             ; .......#
                .byte $01                             ; .......#
                .byte $03                             ; ......##
                .byte $03                             ; ......##
                .byte $0F                             ; ....####
                .byte $3D                             ; ..####.#
                .byte $0F                             ; ....####
                .byte $03                             ; ......##
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $7E                             ; .######.
                .byte $DB                             ; ##.##.##
                .byte $C3                             ; ##....##
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $81                             ; #......#
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $7C                             ; .#####..
                .byte $7C                             ; .#####..
                .byte $D6                             ; ##.#.##.
                .byte $C6                             ; ##...##.
                .byte $82                             ; #.....#.
                .byte $82                             ; #.....#.
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $3C                             ; ..####..
                .byte $66                             ; .##..##.
                .byte $66                             ; .##..##.
                .byte $42                             ; .#....#.
                .byte $42                             ; .#....#.
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $6C                             ; .##.##..
                .byte $44                             ; .#...#..
                .byte $44                             ; .#...#..
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $24                             ; ..#..#..
                .byte $24                             ; ..#..#..
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $28                             ; ..#.#...
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $FF                             ; ########
                .byte $18                             ; ...##...
                .byte $18                             ; ...##...
                .byte $FF                             ; ########
                .byte $7E                             ; .######.
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $7C                             ; .#####..
                .byte $FE                             ; #######.
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $FE                             ; #######.
                .byte $7C                             ; .#####..
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $7E                             ; .######.
                .byte $18                             ; ...##...
                .byte $7E                             ; .######.
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $7C                             ; .#####..
                .byte $10                             ; ...#....
                .byte $7C                             ; .#####..
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $3C                             ; ..####..
                .byte $18                             ; ...##...
                .byte $10                             ; ...#....
                .byte $38                             ; ..###...
                .byte $38                             ; ..###...
                .byte $10                             ; ...#....

;*** Display List fragments ****************************************************
;
; LOCAL VARIABLES
PFMEM_C0R0      = PFMEM+0*40                          ; Start address of PLAYFIELD row 0
PFMEM_C0R5      = PFMEM+5*40                          ; Start address of PLAYFIELD row 5
PFMEM_C0R17     = PFMEM+17*40                         ; Start address of PLAYFIELD row 17

;*** Display List fragment for Control Panel Display (bottom text window) ******
DLSTFRAG        .byte $8D                             ; GR7 + DLI
                .byte $00                             ; BLK1
                .byte $46,<PANELTXT,>PANELTXT         ; GR1 @ PANELTXT
                .byte $20                             ; BLK3
                .byte $06                             ; GR1
                .byte $00                             ; BLK1

;*** Display List fragment for Galactic Chart view *****************************
DLSTFRAGGC      .byte $01,<DLSTGC,>DLSTGC             ; jmp @ DLSTGC

;*** Display List fragment for Long-Range Scan view ****************************
DLSTFRAGLRS     .byte $00                             ; BLK1
                .byte $00                             ; BLK1
                .byte $46,<LRSHEADER,>LRSHEADER       ; GR1 @ LRSHEADER
                .byte $4D,<PFMEM_C0R5,>PFMEM_C0R5     ; GR7 @ PFMEM_C0R5

;*** Display List fragment for Aft view ****************************************
DLSTFRAGAFT     .byte $00                             ; BLK1
                .byte $00                             ; BLK1
                .byte $46,<AFTHEADER,>AFTHEADER       ; GR1 @ AFTHEADER
                .byte $4D,<PFMEM_C0R5,>PFMEM_C0R5     ; GR7 @ PFMEM_C0R5

;*** Display List fragment for Front view and Title text line ******************
DLSTFRAGFRONT   .byte $4D,<PFMEM_C0R0,>PFMEM_C0R0     ; GR7 @ PFMEM_C0R0
                .byte $0D                             ; GR7
                .byte $0D                             ; GR7
                .byte $0D                             ; GR7
                .byte $0D                             ; GR7
                .byte $0D                             ; GR7
                .byte $30                             ; BLK4
                .byte $46,<TITLETXT,>TITLETXT         ; GR1 @ TITLETXT
                .byte $4D,<PFMEM_C0R17,>PFMEM_C0R17   ; GR7 @ PFMEM_C0R17

;*** Display List fragment offsets relative to DLSTFRAG ************************
DLSTFRAGOFFTAB  .byte DLSTFRAGFRONT-DLSTFRAG          ; Front view
                .byte DLSTFRAGAFT-DLSTFRAG            ; Aft view
                .byte DLSTFRAGLRS-DLSTFRAG            ; Long-Range Scan view
                .byte DLSTFRAGGC-DLSTFRAG             ; Galactic Chart view

;*** 1-byte bit patterns for 4 pixels of same color for PLAYFIELD space objects 
FOURCOLORPIXEL  .byte $FF                             ; COLOR3
                .byte $FF                             ; COLOR3
                .byte $FF                             ; COLOR3
                .byte $FF                             ; COLOR3
                .byte $AA                             ; COLOR2
                .byte $FF                             ; COLOR3
                .byte $AA                             ; COLOR2
                .byte $FF                             ; COLOR3
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $FF                             ; COLOR3
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $AA                             ; COLOR2
                .byte $55                             ; COLOR1
                .byte $55                             ; COLOR1
                .byte $AA                             ; COLOR2
                .byte $55                             ; COLOR1
                .byte $AA                             ; COLOR2
                .byte $55                             ; COLOR1
                .byte $55                             ; COLOR1
                .byte $55                             ; COLOR1
                .byte $AA                             ; COLOR2
                .byte $55                             ; COLOR1
                .byte $55                             ; COLOR1
                .byte $55                             ; COLOR1
                .byte $55                             ; COLOR1

;*** Masks to filter 1 pixel (2 bits) from 4 pixels (1 byte of PLAYFIELD memory)
PIXELMASKTAB    .byte $C0                             ; ##......
                .byte $30                             ; ..##....
                .byte $0C                             ; ....##..
                .byte $03                             ; ......##

;*** Velocity table ************************************************************
VELOCITYTAB     .byte 0                               ; Speed 0 =   0 <KM/H>
                .byte 1                               ; Speed 1 =   1 <KM/H>
                .byte 2                               ; Speed 2 =   2 <KM/H>
                .byte 4                               ; Speed 3 =   4 <KM/H>
                .byte 8                               ; Speed 4 =   8 <KM/H>
                .byte 16                              ; Speed 5 =  16 <KM/H>
                .byte 32                              ; Speed 6 =  32 <KM/H>
                .byte 64                              ; Speed 7 =  64 <KM/H>
                .byte 96                              ; Speed 8 =  96 <KM/H>
                .byte 112                             ; Speed 9 = 112 <KM/H>

;*** Keyboard code lookup table ************************************************
KEYTAB          .byte $F2                             ; '0'   - Speed 0
                .byte $DF                             ; '1'   - Speed 1
                .byte $DE                             ; '2'   - Speed 2
                .byte $DA                             ; '3'   - Speed 3
                .byte $D8                             ; '4'   - Speed 4
                .byte $DD                             ; '5'   - Speed 5
                .byte $DB                             ; '6'   - Speed 6
                .byte $F3                             ; '7'   - Speed 7
                .byte $F5                             ; '8'   - Speed 8
                .byte $F0                             ; '9'   - Speed 9
                .byte $F8                             ; 'F'   - Front view
                .byte $FF                             ; 'A'   - Aft view
                .byte $C0                             ; 'L'   - Long-Range Scan view
                .byte $FD                             ; 'G'   - Galactic Chart view
                .byte $ED                             ; 'T'   - Tracking on/off
                .byte $FE                             ; 'S'   - Shields on/off
                .byte $D2                             ; 'C'   - Attack Computer on/off
                .byte $F9                             ; 'H'   - Hyperwarp
                .byte $E5                             ; 'M'   - Manual Target Selector
                .byte $CA                             ; 'P'   - Pause
                .byte $E7                             ; 'INV' - Abort Mission

;*** Engines energy drain rates per game loop iteration in energy subunits *****
DRAINRATETAB    .byte 0                               ;
                .byte 4                               ;
                .byte 6                               ;
                .byte 8                               ;
                .byte 10                              ;
                .byte 12                              ;
                .byte 14                              ;
                .byte 30                              ;
                .byte 45                              ;
                .byte 60                              ;

;*** Hyperwarp energy depending on distance ************************************
WARPENERGYTAB   .byte 10                              ; =  100 energy units
                .byte 13                              ; =  130 energy units
                .byte 16                              ; =  160 energy units
                .byte 20                              ; =  200 energy units
                .byte 23                              ; =  230 energy units
                .byte 50                              ; =  500 energy units
                .byte 70                              ; =  700 energy units
                .byte 80                              ; =  800 energy units
                .byte 90                              ; =  900 energy units
                .byte 120                             ; = 1200 energy units
                .byte 125                             ; = 1250 energy units
                .byte 130                             ; = 1300 energy units
                .byte 135                             ; = 1350 energy units
                .byte 140                             ; = 1400 energy units
                .byte 155                             ; = 1550 energy units
                .byte 170                             ; = 1700 energy units
                .byte 184                             ; = 1840 energy units
                .byte 200                             ; = 2000 energy units
                .byte 208                             ; = 2080 energy units
                .byte 216                             ; = 2160 energy units
                .byte 223                             ; = 2230 energy units
                .byte 232                             ; = 2320 energy units
                .byte 241                             ; = 2410 energy units
                .byte 250                             ; = 2500 energy units

;*** Joystick increments *******************************************************
STICKINCTAB     .byte 0                               ; Centered
                .byte 1                               ; Right or up
                .char -1                              ; Left or down
                .byte 0                               ; Centered

;*** 3-byte elements to draw cross hairs and Attack Computer Display ***********
;   Byte 1 : Pixel column number of line start
;   Byte 2 : Pixel row number of line start
;   Byte 3 : B7 = 0 -> Draw line to the right
;            B7 = 1 -> Draw line down
;            B6..0  -> Length of line in pixels. Possible values are: 0..127.
;
;                   #
;                   #                              4
;                   #                      ##############################
;                   #1                     #             #              #
;                   #                      #             #11            #
;                   #                      #             #              #
;                   #                      #5            #    8         #6
;                                          #      ###############       #
;                                          #      #             #       #
;         15                  16           #   7  #             #   10  #
; ###############       ###############    ########             #########
;                                          #      #12           #       #
;                                          #      #             #13     #
;                                          #      ###############       #
;                   #                      #         9   #              #
;                   #                      #             #              #
;                   #                      #             #14            #
;                   #                      #       3     #              #
;                   #2                     ##############################
;                   #
;                   #
;
;         Front/Aft Cross Hairs                Attack Computer Display
;
; LOCAL VARIABLES
DOWN            = $80
RIGHT           = $00

DRAWLINESTAB    .byte 80,40,DOWN|7                    ; Line 1
                .byte 80,54,DOWN|7                    ; Line 2

                .byte 119,70,RIGHT|30                 ; Line 3
                .byte 119,86,RIGHT|30                 ; Line 4
                .byte 119,70,DOWN|17                  ; Line 5
                .byte 148,70,DOWN|17                  ; Line 6
                .byte 120,78,RIGHT|6                  ; Line 7
                .byte 126,75,RIGHT|15                 ; Line 8
                .byte 126,81,RIGHT|15                 ; Line 9
                .byte 141,78,RIGHT|7                  ; Line 10
                .byte 133,71,DOWN|4                   ; Line 11
                .byte 126,76,DOWN|5                   ; Line 12
                .byte 140,76,DOWN|5                   ; Line 13
                .byte 133,82,DOWN|4                   ; Line 14

                .byte 62,50,RIGHT|15                  ; Line 15
                .byte 84,50,RIGHT|15                  ; Line 16
                .byte $FE                             ; End marker

;*** 3-byte elements to draw our starship's shape in Long-Range Scan view ******
;
;   Line  17 18 19 20 21
;               ##
;               ##
;            ## ## ##
;         ## ## ## ## ##
;         ##    ##    ##

                .byte 78,53,DOWN|2                    ; Line 17
                .byte 79,52,DOWN|2                    ; Line 18
                .byte 80,50,DOWN|5                    ; Line 19
                .byte 81,52,DOWN|2                    ; Line 20
                .byte 82,53,DOWN|2                    ; Line 21
                .byte $FE                             ; End marker

;*** Initial x and y coordinates of a star during hyperwarp ********************
; The following two tables are used to determine the initial x and y coordinates
; (high byte) of a star during hyperwarp. An index in 0..3 picks both the x and
; y coordinate, thus 4 pairs of coordinates are possible:
;
; Y           +-------+----------------------------+----------------------------+
; ^           | Index |        x-coordinate        |        y-coordinate        |
; |           +-------+----------------------------+----------------------------+
; |.32.       |   0   | +1024..+1279 (+$04**) <KM> |  +512..+767  (+$02**) <KM> |
; |...1       |   1   | +1024..+1279 (+$04**) <KM> |  +768..+1023 (+$03**) <KM> |
; |...0       |   2   |  +768..+1023 (+$03**) <KM> | +1024..+1279 (+$04**) <KM> |
; |....       |   3   |  +512..+767  (+$02**) <KM> | +1024..+1279 (+$04**) <KM> |
; 0----->X    +-------+----------------------------+----------------------------+

;*** Initial x-coordinate (high byte) of star in hyperwarp *********************
WARPSTARXTAB    .byte $04                             ; +1024..+1279 (+$04**) <KM>
                .byte $04                             ; +1024..+1279 (+$04**) <KM>
                .byte $03                             ;  +768..+1023 (+$03**) <KM>
                .byte $02                             ;  +512..+767  (+$02**) <KM>

;*** Initial y-coordinate (high byte) of star in hyperwarp *********************
WARPSTARYTAB    .byte $02                             ;  +512..+767  (+$02**) <KM>
                .byte $03                             ;  +768..+1023 (+$03**) <KM>
                .byte $04                             ; +1024..+1279 (+$04**) <KM>
                .byte $04                             ; +1024..+1279 (+$04**) <KM>

;*** Text of Control Panel Display (encoded in custom character set) ***********
; Row 1: "V:00 K:00 E:9999 T:0"
; Row 2: " O:-00 O:-00 R:-000 "

PANELTXTTAB     .byte CCS_V
                .byte CCS_COLON
                .byte CCS_0
                .byte CCS_0
                .byte CCS_SPC
                .byte CCS_COL1|CCS_K
                .byte CCS_COL1|CCS_COLON
                .byte CCS_COL1|CCS_0
                .byte CCS_COL1|CCS_0
                .byte CCS_SPC
                .byte CCS_COL2|CCS_E
                .byte CCS_COL2|CCS_COLON
                .byte CCS_COL2|CCS_9
                .byte CCS_COL2|CCS_9
                .byte CCS_COL2|CCS_9
                .byte CCS_COL2|CCS_9
                .byte CCS_SPC
                .byte CCS_T
                .byte CCS_COLON
                .byte CCS_0

                .byte CCS_SPC
                .byte CCS_THETA
                .byte CCS_COLON
                .byte CCS_MINUS
                .byte CCS_0
                .byte CCS_0
                .byte CCS_SPC
                .byte CCS_COL1|CCS_PHI
                .byte CCS_COL1|CCS_COLON
                .byte CCS_MINUS
                .byte CCS_0
                .byte CCS_0
                .byte CCS_SPC
                .byte CCS_COL2|CCS_R
                .byte CCS_COL2|CCS_COLON
                .byte CCS_MINUS
                .byte CCS_0
                .byte CCS_0
                .byte CCS_0
                .byte CCS_SPC

;*** Text of Galactic Chart Panel Display **************************************
; Row 1: "WARP ENERGY:   0    "
; Row 2: "TARGETS:  DC:PESCLR "
; Row 3: "STAR DATE:00.00     "

                .byte ROM_W
                .byte ROM_A
                .byte ROM_R
                .byte ROM_P
                .byte ROM_SPC
                .byte ROM_E
                .byte ROM_N
                .byte ROM_E
                .byte ROM_R
                .byte ROM_G
                .byte ROM_Y
                .byte ROM_COLON
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_0
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_SPC

                .byte CCS_COL2|ROM_T
                .byte CCS_COL2|ROM_A
                .byte CCS_COL2|ROM_R
                .byte CCS_COL2|ROM_G
                .byte CCS_COL2|ROM_E
                .byte CCS_COL2|ROM_T
                .byte CCS_COL2|ROM_S
                .byte CCS_COL2|ROM_COLON
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_D
                .byte ROM_C
                .byte ROM_COLON
                .byte ROM_P
                .byte ROM_E
                .byte ROM_S
                .byte ROM_C
                .byte ROM_L
                .byte ROM_R
                .byte ROM_SPC

                .byte CCS_COL3|ROM_S
                .byte CCS_COL3|ROM_T
                .byte CCS_COL3|ROM_A
                .byte CCS_COL3|ROM_R
                .byte ROM_SPC
                .byte CCS_COL3|ROM_D
                .byte CCS_COL3|ROM_A
                .byte CCS_COL3|ROM_T
                .byte CCS_COL3|ROM_E
                .byte CCS_COL3|ROM_COLON
                .byte CCS_COL3|ROM_0
                .byte CCS_COL3|ROM_0
                .byte CCS_COL3|ROM_DOT
                .byte CCS_COL3|ROM_0
                .byte CCS_COL3|ROM_0
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_SPC
                .byte ROM_SPC

;*** Galactic Chart sector type table ******************************************
SECTORTYPETAB   .byte $CF                             ; Starbase
                .byte $04                             ; 4 Zylon ships
                .byte $03                             ; 3 Zylon ships
                .byte $02                             ; 1 or 2 Zylon ships

;*** Phrase table **************************************************************
; Phrases consist of phrase tokens. These are bytes that encode words, segments
; (multiple words that fit into a single line of text), and how they are displayed.
;
; LOCAL VARIABLES
EOP             = $40                                 ; End of phrase
EOS             = $80                                 ; End of segment
LONG            = $C0                                 ; Display title phrase for a long time

                                                      ; Title Phrase Offset, Text
PHRASETAB       .byte $00                             ; (unused)
                .byte $05,$06,$02|EOP                 ; $01  "ATTACK COMPUTER ON"
                .byte $05,$06,$03|EOP                 ; $04  "ATTACK COMPUTER OFF"
                .byte $04,$02|EOP                     ; $07  "SHIELDS ON"
                .byte $04,$03|EOP                     ; $09  "SHIELDS OFF"
                .byte $06,$07,$02|EOP                 ; $0B  "COMPUTER TRACKING ON"
                .byte $07,$03|EOP                     ; $0E  "TRACKING OFF"
                .byte $08|EOP                         ; $10  "WHATS WRONG?"
                .byte $09,$0A|EOP                     ; $11  "HYPERWARP ENGAGED"
                .byte $0B,$0D|LONG                    ; $13  "STARBASE SURROUNDED"
                .byte $0B,$0C|LONG                    ; $15  "STARBASE DESTROYED"
                .byte $09,$0E|EOP                     ; $17  "HYPERWARP ABORTED"
                .byte $09,$0F|EOP                     ; $19  "HYPERWARP COMPLETE"
                .byte $10|LONG                        ; $1B  "HYPERSPACE"
                .byte $11,$12|EOS                     ; $1C  "ORBIT ESTABLISHED"
                .byte $16|EOP                         ; $1E  "STANDBY"
                .byte $13,$0E|EOP                     ; $1F  "DOCKING ABORTED"
                .byte $15,$0F|EOP                     ; $21  "TRANSFER COMPLETE"
                .byte $38|EOS                         ; $23  " "
                .byte $17|EOS                         ; $24  "STAR FLEET TO"
                .byte $19|EOS                         ; $25  "ALL UNITS"
                .byte $18|EOS                         ; $26  "STAR CRUISER 7"
                .byte $0C|EOS                         ; $27  "DESTROYED"
                .byte $1D|EOS                         ; $28  "BY ZYLON FIRE"
                .byte $1E,$1F|EOS                     ; $29  "POSTHUMOUS RANK IS:"
                .byte $FD                             ; $2B  "<PLACEHOLDER FOR RANK>"
                .byte $25,$FC                         ; $2C  "CLASS <PLACEHOLDER FOR CLASS>"
                .byte $38|EOP                         ; $2E  " "
                .byte $1B|EOS                         ; $2F  "STAR RAIDERS"
                .byte $20|EOP                         ; $30  "COPYRIGHT ATARI 1979"
                .byte $38|EOS                         ; $31  " "
                .byte $17|EOS                         ; $32  "STAR FLEET TO"
                .byte $18|EOS                         ; $33  "STAR CRUISER 7"
                .byte $1A,$0E|EOS                     ; $34  "MISSION ABORTED"
                .byte $1C,$14|EOS                     ; $36  "ZERO ENERGY"
                .byte $24,$1F|EOS                     ; $38  "NEW RANK IS"
                .byte $FD                             ; $3A  "<PLACEHOLDER FOR RANK>"
                .byte $25,$FC                         ; $3B  "CLASS <PLACEHOLDER FOR CLASS>"
                .byte $27|EOS                         ; $3D  "REPORT TO BASE"
                .byte $28|EOP                         ; $3E  "FOR TRAINING"
                .byte $38|EOS                         ; $3F  " "
                .byte $17|EOS                         ; $40  "STAR FLEET TO"
                .byte $18|EOS                         ; $41  "STAR CRUISER 7"
                .byte $1A,$0F|EOS                     ; $42  "MISSION COMPLETE"
                .byte $24,$1F|EOS                     ; $44  "NEW RANK IS:"
                .byte $FD                             ; $46  "<PLACEHOLDER FOR RANK>"
                .byte $25,$FC                         ; $47  "CLASS <PLACEHOLDER FOR CLASS>"
                .byte $26|EOP                         ; $49  "CONGRATULATIONS"
                .byte $2C,$1A|EOP                     ; $4A  "NOVICE MISSION"
                .byte $2E,$1A|EOP                     ; $4C  "PILOT MISSION"
                .byte $31,$1A|EOP                     ; $4E  "WARRIOR MISSION"
                .byte $33,$1A|EOP                     ; $50  "COMMANDER MISSION"
                .byte $38|EOS                         ; $52  " "
                .byte $34,$36|EOP                     ; $53  "DAMAGE CONTROL"
                .byte $37,$35|EOS                     ; $55  "PHOTONS DAMAGED"
                .byte $38|EOP                         ; $57  " "
                .byte $37,$0C|EOS                     ; $58  "PHOTONS DESTROYED"
                .byte $38|EOP                         ; $5A  " "
                .byte $23,$35|EOS                     ; $5B  "ENGINES DAMAGED"
                .byte $38|EOP                         ; $5D  " "
                .byte $23,$0C|EOS                     ; $5E  "ENGINES DESTROYED"
                .byte $38|EOP                         ; $60  " "
                .byte $04,$35|EOS                     ; $61  "SHIELDS DAMAGED"
                .byte $38|EOP                         ; $63  " "
                .byte $04,$0C|EOS                     ; $64  "SHIELDS DESTROYED"
                .byte $38|EOP                         ; $66  " "
                .byte $06,$35|EOS                     ; $67  "COMPUTER DAMAGED"
                .byte $38|EOP                         ; $69  " "
                .byte $06,$0C|EOS                     ; $6A  "COMPUTER DESTROYED"
                .byte $38|EOP                         ; $6C  " "
                .byte $22|EOS                         ; $6D  "SECTOR SCAN"
                .byte $35|EOP                         ; $6E  "DAMAGED"
                .byte $22|EOS                         ; $6F  "SECTOR SCAN"
                .byte $0C|EOP                         ; $70  "DESTROYED"
                .byte $21|EOS                         ; $71  "SUB-SPACE RADIO"
                .byte $35|EOP                         ; $72  "DAMAGED"
                .byte $21|EOS                         ; $73  "SUB-SPACE RADIO"
                .byte $0C|EOP                         ; $74  "DESTROYED"
                .byte $01|LONG                        ; $75  "RED ALERT"
                .byte $38|EOS                         ; $76  " "
                .byte $17|EOS                         ; $77  "STAR FLEET TO"
                .byte $18|EOS                         ; $78  "STAR CRUISER 7"
                .byte $1A,$0E|EOS                     ; $79  "MISSION ABORTED"
                .byte $24,$1F|EOS                     ; $7B  "NEW RANK IS:"
                .byte $FD                             ; $7D  "<PLACEHOLDER FOR RANK>"
                .byte $25,$FC                         ; $7E  "CLASS <PLACEHOLDER FOR CLASS>"
                .byte $26|EOP                         ; $80  "CONGRATULATIONS"

;*** Word table ****************************************************************
; Bit B7 of the first byte of a word is the end-of-word marker of the preceding
; word
;
; LOCAL VARIABLES
EOW             = $80                                 ; End of word

WORDTAB         .text EOW|$20,"    RED ALERT"         ; Word $01
                .text EOW|'O',"N"                      ; Word $02
                .text EOW|'O',"FF"                     ; Word $03
                .text EOW|'S',"HIELDS"                 ; Word $04
                .text EOW|'A',"TTACK"                  ; Word $05
                .text EOW|'C',"OMPUTER"                ; Word $06
                .text EOW|'T',"RACKING"                ; Word $07
                .text EOW|'W',"HATS WRONG?"            ; Word $08
                .text EOW|'H',"YPERWARP"               ; Word $09
                .text EOW|'E',"NGAGED"                 ; Word $0A
                .text EOW|'S',"TARBASE"                ; Word $0B
                .text EOW|'D',"ESTROYED"               ; Word $0C
                .text EOW|'S',"URROUNDED"              ; Word $0D
                .text EOW|'A',"BORTED"                 ; Word $0E
                .text EOW|'C',"OMPLETE"                ; Word $0F
                .text EOW|'H',"YPERSPACE"              ; Word $10
                .text EOW|'O',"RBIT"                   ; Word $11
                .text EOW|'E',"STABLISHED"             ; Word $12
                .text EOW|'D',"OCKING"                 ; Word $13
                .text EOW|'E',"NERGY"                  ; Word $14
                .text EOW|'T',"RANSFER"                ; Word $15
                .text EOW|'S',"TANDBY"                 ; Word $16
                .text EOW|'S',"TAR FLEET TO"           ; Word $17
                .text EOW|'S',"TAR CRUISER 7"          ; Word $18
                .text EOW|'A',"LL UNITS"               ; Word $19
                .text EOW|'M',"ISSION"                 ; Word $1A
                .text EOW|$20,"   STAR RAIDERS"       ; Word $1B
                .text EOW|'Z',"ERO"                    ; Word $1C
                .text EOW|'B',"Y ZYLON FIRE"           ; Word $1D
                .text EOW|'P',"OSTHUMOUS"              ; Word $1E
                .text EOW|'R',"ANK IS:"                ; Word $1F
                .text EOW|'C',"OPYRIGHT ATARI 1979"    ; Word $20
                .text EOW|'S',"UB-SPACE RADIO"         ; Word $21
                .text EOW|'S',"ECTOR SCAN"             ; Word $22
                .text EOW|'E',"NGINES"                 ; Word $23
                .text EOW|'N',"EW"                     ; Word $24
                .text EOW|'C',"LASS"                   ; Word $25
                .text EOW|'C',"ONGRATULATIONS"         ; Word $26
                .text EOW|'R',"EPORT TO BASE"          ; Word $27
                .text EOW|'F',"OR TRAINING"            ; Word $28
                .text EOW|'G',"ALACTIC COOK"           ; Word $29
                .text EOW|'G',"ARBAGE SCOW CAPTAIN"    ; Word $2A
                .text EOW|'R',"OOKIE"                  ; Word $2B
                .text EOW|'N',"OVICE"                  ; Word $2C
                .text EOW|'E',"NSIGN"                  ; Word $2D
                .text EOW|'P',"ILOT"                   ; Word $2E
                .text EOW|'A',"CE"                     ; Word $2F
                .text EOW|'L',"IEUTENANT"              ; Word $30
                .text EOW|'W',"ARRIOR"                 ; Word $31
                .text EOW|'C',"APTAIN"                 ; Word $32
                .text EOW|'C',"OMMANDER"               ; Word $33
                .text EOW|'D',"AMAGE"                  ; Word $34
                .text EOW|'D',"AMAGED"                 ; Word $35
                .text EOW|'C',"ONTROL"                 ; Word $36
                .text EOW|'P',"HOTONS"                 ; Word $37
                .text EOW|$20                          ; Word $38
                .text EOW|'S',"TAR COMMANDER"          ; Word $39
                .byte EOW|$00

;*** View modes ****************************************************************
VIEWMODETAB     .byte $00                             ; Front view
                .byte $01                             ; Aft view
                .byte $40                             ; Long-Range Scan view
                .byte $80                             ; Galactic Chart view

;*** Title phrase offsets of "TRACKING OFF", "SHIELDS OFF", "COMPUTER OFF" *****
MSGOFFTAB       .byte $0E                             ; "TRACKING OFF"
                .byte $09                             ; "SHIELDS OFF"
                .byte $04                             ; "COMPUTER OFF"

;*** Masks to test if Tracking Computer, Shields, or Attack Computer are on ****
MSGBITTAB       .byte $FF                             ; Mask Tracking Computer
                .byte $08                             ; Mask Shields
                .byte $02                             ; Mask Attack Computer

;*** Title phrase offsets of "COMPUTER TRACKING ON", "SHIELDS ON", "COMPUTER ON"
MSGONTAB        .byte $0B                             ; "COMPUTER TRACKING ON"
                .byte $07                             ; "SHIELDS ON"
                .byte $01                             ; "COMPUTER ON"

;*** The following two tables encode the PLAYER shapes *************************
;
; PHOTON TORPEDO (shape type 0, data in shape table PLSHAP1TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $01..$10  $11..$1E  $1F..$2A  $2B..$34  $35..$3C  $3D..$42  $75..$76  $7A..$7B
; ...##...  ...#....  ...##...  ...#....  ...#....  ...#....  ...##...  ...#....
; ..####..  ..###...  ..####..  ..###...  ...##...  ..###...  ...##...  ...#....
; .######.  .#####..  ..####..  ..###...  ..####..  ..###...
; .######.  .#####..  .######.  .#####..  ..#.##..  ..#.#...
; .###.##.  #######.  .##.###.  .###.#..  ..####..  ..###...
; ####.###  ##.####.  .####.#.  .#####..  ..####..  ...#....
; ##.#####  ##.##.#.  .######.  .##.##..  ...##...
; ##.#####  #####.#.  .###.##.  ..###...  ....#...
; ########  ###.###.  .######.  ..###...
; ########  ###.###.  ..####..  ...#....
; ####.###  .#####..  ..####..
; .###.##.  .#####..  ...##...
; .######.  ..###...
; .######.  ...#....
; ..####..
; ...##...
;
; ZYLON FIGHTER (shape type 1, data in shape table PLSHAP2TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $01..$0C  $0D..$14  $15..$1A  $1B..$20  $21..$24  $25..$28  $29..$2A  $2B..$2C
; #......#  #.....#.  .#....#.  .#...#..  ..#..#..  ..#.#...  ...##...  ...#....
; #......#  #.....#.  .#.##.#.  .#.#.#..  ..####..  ..###...  ...##...  ...#....
; #......#  #.###.#.  .######.  .#####..  ..####..  ..###...
; #......#  #######.  .######.  .#####..  ..#..#..  ..#.#...
; #.####.#  #######.  .#.##.#.  .#.#.#..
; ########  #.###.#.  .#....#.  .#...#..
; ########  #.....#.
; #.####.#  #.....#.
; #......#
; #......#
; #......#
; #......#
;
; STARBASE RIGHT (shape type 2, data in shape table PLSHAP2TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $2D..$36  $38..$40  $41..$46  $36..$38  $36       $00       $00       $00
; ###.....  ##......  ##......  ##......  ##......  ........  ........  ........
; #####...  ####....  ##......  ####....
; #####...  ####....  ####....  ##......
; #######.  ######..  #.####..
;  #.#.###  #.#####.  ####....
; #######.  ######..  ##......
; #####...  ####....
; #####...  #.......
; ##......  #.......
; ##......
;
; STARBASE CENTER (shape type 3, data in shape table PLSHAP1TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $7E..$8D  $8E..$9C  $9D..$A9  $AA..$B3  $B4..$BB  $BC..$C0  $7B..$7D  $7A..$7B
; ...##...  ........  ........  ...##...  ........  ...##...  ...#....  ...#....
; .######.  ...##...  ...##...  ..####..  ...##...  ..####..  ..###...  ...#....
; ########  ..####..  ..####..  ########  ..####..  ########  ...#....
; ########  .######.  .######.  ########  ########  ..####..
; ########  ########  ########  ###..###  ########  ...##...
; ########  ########  ########  .##..##.  ########
; ########  ########  ###..###  ########  ..####..
; ###..###  ###..###  .##..##.  ########  ...##...
; ###..###  .##..##.  ########  .######.
; ########  ########  ########  ..####..
; ########  ########  ########
; ########  ########  ########
; ########  ########  ..####..
; ########  .######.
; .######.  .######.
; .######.
;
; STARBASE LEFT (shape type 4, data in shape table PLSHAP2TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $47..$50  $52..$5A  $5B..$60  $50..$52  $50       $00       $00       $00
; .....###  ......##  ......##  ......##  ......##  ........  ........  ........
; ...#####  ....####  ......##  ....####
; ...#####  ....####  ....####  ......##
; .#######  ..######  ..####.#
; ###.#.#.  .#####.#  ....####
; .#######  ..######  ......##
; ...#####  ....####
; ...#####  .......#
; ......##  .......#
; ......##
;
; TRANSFER VESSEL (shape type 5, data in shape table PLSHAP1TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $43..$52  $53..$60  $61..$6B  $6C..$74  $75..$79  $7A..$7D  $75..$76  $7A..$7B
; ..####..  ...###..  ...##...  ...#....  ...##...  ...#....  ...##...  ...#....
; ..####..  ...###..  ...##...  ...#....  ...##...  ...#....  ...##...  ...#....
; ..#..#..  ...#.#..  ..####..  ..###...  ..####..  ..###...
; ..####..  ..#####.  ..####..  ..###...  ...##...  ...#....
; .######.  ..#####.  ..####..  ..###...  ...##...
; .######.  ..#####.  ..####..  .#####..
; .######.  ..#.#.#.  .######.  ..#.#...
; .#.##.#.  .#######  ..#..#..  ..#.#...
; ########  .#######  ..#..#..  ..#.#...
; ########  ..#...#.  ..#..#..
; .#....#.  ..#...#.  ..#..#..
; .#....#.  ..#...#.
; .#....#.  ..#...#.
; .#....#.  ..#...#.
; .#....#.
; .#....#.
;
; METEOR (shape type 6, data in shape table PLSHAP1TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $01..$10  $11..$1E  $1F..$2A  $2B..$34  $35..$3C  $3D..$42  $75..$76  $7A..$7B
; ...##...  ...#....  ...##...  ...#....  ...#....  ...#....  ...##...  ...#....
; ..####..  ..###...  ..####..  ..###...  ...##...  ..###...  ...##...  ...#....
; .######.  .#####..  ..####..  ..###...  ..####..  ..###...
; .######.  .#####..  .######.  .#####..  ..#.##..  ..#.#...
; .###.##.  #######.  .##.###.  .###.#..  ..####..  ..###...
; ####.###  ##.####.  .####.#.  .#####..  ..####..  ...#....
; ##.#####  ##.##.#.  .######.  .##.##..  ...##...
; ##.#####  #####.#.  .###.##.  ..###...  ....#...
; ########  ###.###.  .######.  ..###...
; ########  ###.###.  ..####..  ...#....
; ####.###  .#####..  ..####..
; .###.##.  .#####..  ...##...
; .######.  ..###...
; .######.  ...#....
; ..####..
; ...##...
;
; ZYLON CRUISER (shape type 7, data in shape table PLSHAP2TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $61..$69  $6A..$71  $72..$78  $79..$7E  $7F..$82  $83..$85  $29..$2A  $2B..$2C
; ...##...  ...#....  ...##...  ...#....  ...##...  ...#....  ...##...  ...#....
; ..####..  ..###...  ..####..  ..###...  ..####..  ..###...  ...##...  ...#....
; .######.  .#####..  ..####..  ..###...  ..#..#..  ..#.#...
; .######.  .#####..  .##..##.  .##.##..  ..#..#..
; ##.##.##  ##.#.##.  .##..##.  .#...#..
; ##....##  ##...##.  .#....#.  .#...#..
; #......#  #.....#.  .#....#.
; #......#  #.....#.
; #......#
;
; ZYLON BASESTAR (shape type 8, data in shape table PLSHAP2TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $86..$8F  $90..$99  $9A..$A0  $A1..$A7  $A8..$AC  $AD..$B0  $29..$2A  $2B..$2C
; ...##...  ...#....  ...##...  ...#....  ...##...  ...#....  ...##...  ...#....
; ..####..  ..###...  ..####..  ..###...  ..####..  ..###...  ...##...  ...#....
; .######.  .#####..  .######.  .#####..  ...##...  ..###...
; ########  #######.  ...##...  ...#....  ..####..  ...#....
; ...##...  ..###...  .######.  .#####..  ...##...
; ...##...  ..###...  ..####..  ..###...
; ########  #######.  ...##...  ...#....
; .######.  .#####..
; ..####..  ..###...
; ...##...  ...#....
;
; HYPERWARP TARGET MARKER (shape type 9, data in shape table PLSHAP1TAB)
; Numbers at top indicate the shape table offset of the first and last shape row
;
; $C1..$CC  $C1..$CC  $C1..$CC  $C1..$CC  $C1..$CC  $C1..$CC  $75..$76  $C1..$CC
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ...##...  ..#.#...
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ...##...  ..#.#...
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...            ..#.#...
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...            ..#.#...
; ###.###.  ###.###.  ###.###.  ###.###.  ###.###.  ###.###.            ###.###.
; ........  ........  ........  ........  ........  ........            ........
; ........  ........  ........  ........  ........  ........            ........
; ###.###.  ###.###.  ###.###.  ###.###.  ###.###.  ###.###.            ###.###.
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...            ..#.#...
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...            ..#.#...
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...            ..#.#...
; ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...  ..#.#...            ..#.#...

;*** Shape type 0..9 offset table (10 shape cell offsets of shape type...) *****
PLSHAPOFFTAB    .byte $01,$11,$1F,$2B,$35,$3D,$75,$7A ; ...0 into PLSHAP1TAB
                .byte $01,$0D,$15,$1B,$21,$25,$29,$2B ; ...1 into PLSHAP2TAB
                .byte $2D,$38,$41,$36,$36,$00,$00,$00 ; ...2 into PLSHAP2TAB
                .byte $7E,$8E,$9D,$AA,$B4,$BC,$7B,$7A ; ...3 into PLSHAP1TAB
                .byte $47,$52,$5B,$50,$50,$00,$00,$00 ; ...4 into PLSHAP2TAB
                .byte $43,$53,$61,$6C,$75,$7A,$75,$7A ; ...5 into PLSHAP1TAB
                .byte $01,$11,$1F,$2B,$35,$3D,$75,$7A ; ...6 into PLSHAP1TAB
                .byte $61,$6A,$72,$79,$7F,$83,$29,$2B ; ...7 into PLSHAP2TAB
                .byte $86,$90,$9A,$A1,$A8,$AD,$29,$2B ; ...8 into PLSHAP2TAB
                .byte $C1,$C1,$C1,$C1,$C1,$C1,$75,$C1 ; ...9 into PLSHAP1TAB


;*** Shape type 0..9 height table (10 shape cell heights of shape type...) *****
PLSHAPHEIGHTTAB .byte $0F,$0D,$0B,$09,$07,$05,$01,$01 ; ...0
                .byte $0B,$07,$05,$05,$03,$03,$01,$01 ; ...1
                .byte $09,$08,$05,$02,$00,$00,$00,$00 ; ...2
                .byte $0F,$0E,$0C,$09,$07,$04,$02,$01 ; ...3
                .byte $09,$08,$05,$02,$00,$00,$00,$00 ; ...4
                .byte $0F,$0D,$0A,$08,$04,$03,$01,$01 ; ...5
                .byte $0F,$0D,$0B,$09,$07,$05,$01,$01 ; ...6
                .byte $08,$07,$06,$05,$03,$02,$01,$01 ; ...7
                .byte $09,$09,$06,$06,$04,$03,$01,$01 ; ...8
                .byte $0B,$0B,$0B,$0B,$0B,$0B,$01,$0B ; ...9


;*** Keyboard codes to switch to Front or Aft view when Tracking Computer is on 
TRACKKEYSTAB    .byte $F8                             ; 'F' - Front view
                .byte $FF                             ; 'A' - Aft view

;*** Galactic Chart sector character codes (encoded in custom character set) ***
SECTORCHARTAB   .byte CCS_BORDERSW                    ; Empty sector
                .byte CCS_2ZYLONS                     ; Sector contains 1 Zylon ship
                .byte CCS_2ZYLONS                     ; Sector contains 2 Zylon ships
                .byte CCS_3ZYLONS                     ; Sector contains 3 Zylon ships
                .byte CCS_4ZYLONS                     ; Sector contains 4 Zylon ships
                .byte CCS_STARBASE                    ; Sector contains starbase

;*** Mask to limit veer-off velocity of Hyperwarp Target Marker in hyperwarp ***
VEERMASKTAB     .byte NEG|31                          ;  -31..+31  <KM/H> (unused)
                .byte NEG|63                          ;  -63..+63  <KM/H> PILOT mission
                .byte NEG|95                          ;  -95..+95  <KM/H> WARRIOR mission
                .byte NEG|127                         ; -127..+127 <KM/H> COMMANDER mission

;*** Horizontal PLAYER offsets for PLAYER0..1 (STARBASE LEFT, STARBASE RIGHT) **
PLSTARBAOFFTAB  .char -8                              ; -8 Player/Missile pixels
                .byte 8                               ; +8 Player/Missile pixels

;*** Mission bonus table *******************************************************
BONUSTAB        .byte 80                              ; Mission complete   NOVICE mission
                .byte 76                              ; Mission complete   PILOT mission
                .byte 60                              ; Mission complete   WARRIOR mission
                .byte 111                             ; Mission complete   COMMANDER mission

                .byte 60                              ; Mission aborted    NOVICE mission
                .byte 60                              ; Mission aborted    PILOT mission
                .byte 50                              ; Mission aborted    WARRIOR mission
                .byte 100                             ; Mission aborted    COMMANDER mission

                .byte 40                              ; Starship destroyed NOVICE mission
                .byte 50                              ; Starship destroyed PILOT mission
                .byte 40                              ; Starship destroyed WARRIOR mission
                .byte 90                              ; Starship destroyed COMMANDER mission

;*** Title phrase offsets of scored class rank *********************************
RANKTAB         .byte $29|EOS                         ; "GALACTIC COOK"
                .byte $2A|EOS                         ; "GARBAGE SCOW CAPTAIN"
                .byte $2A|EOS                         ; "GARBAGE SCOW CAPTAIN"
                .byte $2B|EOS                         ; "ROOKIE"
                .byte $2B|EOS                         ; "ROOKIE"
                .byte $2C|EOS                         ; "NOVICE"
                .byte $2C|EOS                         ; "NOVICE"
                .byte $2D|EOS                         ; "ENSIGN"
                .byte $2D|EOS                         ; "ENSIGN"
                .byte $2E|EOS                         ; "PILOT"
                .byte $2E|EOS                         ; "PILOT"
                .byte $2F|EOS                         ; "ACE"
                .byte $30|EOS                         ; "LIEUTENANT"
                .byte $31|EOS                         ; "WARRIOR"
                .byte $32|EOS                         ; "CAPTAIN"
                .byte $33|EOS                         ; "COMMANDER"
                .byte $33|EOS                         ; "COMMANDER"
                .byte $39|EOS                         ; "STAR COMMANDER"
                .byte $39|EOS                         ; "STAR COMMANDER"

;*** Scored class number table *************************************************
CLASSTAB        .byte CCS_COL2|ROM_5                  ; Class 5
                .byte CCS_COL2|ROM_5                  ; Class 5
                .byte CCS_COL2|ROM_5                  ; Class 5
                .byte CCS_COL2|ROM_4                  ; Class 4
                .byte CCS_COL2|ROM_4                  ; Class 4
                .byte CCS_COL2|ROM_4                  ; Class 4
                .byte CCS_COL2|ROM_4                  ; Class 4
                .byte CCS_COL2|ROM_3                  ; Class 3
                .byte CCS_COL2|ROM_3                  ; Class 3
                .byte CCS_COL2|ROM_3                  ; Class 3
                .byte CCS_COL2|ROM_2                  ; Class 2
                .byte CCS_COL2|ROM_2                  ; Class 2
                .byte CCS_COL2|ROM_2                  ; Class 2
                .byte CCS_COL2|ROM_1                  ; Class 1
                .byte CCS_COL2|ROM_1                  ; Class 1
                .byte CCS_COL2|ROM_1                  ; Class 1

;*** Title phrase offsets of mission level *************************************
MISSIONPHRTAB   .byte $4A                             ; "NOVICE MISSION"
                .byte $4C                             ; "PILOT MISSION"
                .byte $4E                             ; "WARRIOR MISSION"
                .byte $50                             ; "COMMANDER MISSION"

;*** Damage probability of subsystems depending on mission level ***************
DAMAGEPROBTAB   .byte 0                               ;  0% (  0:256) NOVICE mission
                .byte 80                              ; 31% ( 80:256) PILOT mission
                .byte 180                             ; 70% (180:256) WARRIOR mission
                .byte 254                             ; 99% (254:256) COMMANDER mission

;*** Title phrase offsets of damaged subsystems ********************************
DAMAGEPHRTAB    .byte $55                             ; "PHOTON TORPEDOS DAMAGED"
                .byte $5B                             ; "ENGINES DAMAGED"
                .byte $61                             ; "SHIELDS DAMAGED"
                .byte $67                             ; "COMPUTER DAMAGED"
                .byte $6D                             ; "LONG RANGE SCAN DAMAGED"
                .byte $71                             ; "SUB-SPACE RADIO DAMAGED"

;*** Title phrase offsets of destroyed subsystems ******************************
DESTROYPHRTAB   .byte $58                             ; "PHOTON TORPEDOS DESTROYED"
                .byte $5E                             ; "ENGINES DESTROYED"
                .byte $64                             ; "SHIELDS DESTROYED"
                .byte $6A                             ; "COMPUTER DESTROYED"
                .byte $6F                             ; "LONG RANGE SCAN DESTROYED"
                .byte $73                             ; "SUB-SPACE RADIO DESTROYED"

;*** 3 x 10-byte noise sound patterns (bytes 0..7 stored in reverse order) *****
;
; (9) AUDCTL        ($D208) POKEY: Audio control
; (8) AUDF3         ($D204) POKEY: Audio channel 3 frequency
; (7) NOISETORPTIM  ($DA)   Timer for PHOTON TORPEDO LAUNCHED noise sound patterns
; (6) NOISEEXPLTIM  ($DB)   Timer for SHIELD and ZYLON EXPLOSION noise sound patterns
; (5) NOISEAUDC2    ($DC)   Audio channel 1/2 control shadow register
; (4) NOISEAUDC3    ($DD)   Audio channel 3   control shadow register
; (3) NOISEAUDF1    ($DE)   Audio channel 1 frequency shadow register
; (2) NOISEAUDF2    ($DF)   Audio channel 2 frequency shadow register
; (1) NOISEFRQINC   ($E0)   Audio channel 1/2 frequency increment
; (0) NOISELIFE     ($E1)   Noise sound pattern lifetime
;
;                     (0),(1),(2),(3),(4),(5),(6),(7),(8),(9)
NOISEPATTAB     .byte $18,$FF,$02,$00,$8A,$A0,$00,$08,$50,$00; PHOTON TORPEDO LAUNCHED
                .byte $40,$40,$01,$03,$88,$AF,$08,$00,$50,$04; SHIELD EXPLOSION
                .byte $30,$40,$01,$03,$84,$A8,$04,$00,$50,$04; ZYLON EXPLOSION



;*** 5 x 6-byte beeper sound patterns (bytes 0..5 stored in reverse order) *****
;
; (5) BEEPFRQIND    ($D2) Running index into frequency table BEEPFRQTAB ($BF5C)
; (4) BEEPREPEAT    ($D3) Number of times the beeper sound pattern sequence is repeated - 1
; (3) BEEPTONELIFE  ($D4) Lifetime of tone in TICKs - 1
; (2) BEEPPAUSELIFE ($D5) Lifetime of pause in TICKs - 1 ($FF -> No pause)
; (1) BEEPPRIORITY  ($D6) Beeper sound pattern priority. A playing beeper sound pattern is
;                         stopped if a beeper sound pattern of higher priority is about to be
;                         played. A value of 0 indicates that no beeper sound pattern is
;                         playing at the moment.
; (0) BEEPFRQSTART  ($D7) Index to first byte of the beeper sound pattern frequency in table
;                         BEEPFRQTAB ($BF5C)
;
; Frequency-over-TICKs diagrams for all beeper sound patterns:
;
; HYPERWARP TRANSIT
;
;      FRQ
;       |
;   $18 |-4--
;       |
;   $00 |    -3-
;       +-------> TICKS
;        <13 x >
;
; RED ALERT
;
;      FRQ
;       |
;   $60 |                 --------17-------
;       |
;   $40 |--------17-------
;       |
;       +----------------------------------> TICKS
;        <-------------- 8 x ------------->
;
; ACKNOWLEDGE
;
;      FRQ
;       |
;   $10 |-3-   -3-   -3-
;       |
;   $00 |   -3-   -3-   -3-
;       +------------------> TICKS
;        <------ 1 x ----->
;
; DAMAGE REPORT (not to scale)
;
;      FRQ
;       |
;   $40 |------------33-------------
;       |
;   $20 |                           ------------33-------------
;       |
;       +------------------------------------------------------> TICKS
;        <------------------------ 3 x ----------------------->
;
; MESSAGE FROM STARBASE (not to scale)
;
;      FRQ
;       |
;   $51 |                                  -----33-----
;   $48 |-----33-----
;   $40 |                 -----33-----
;       |
;   $00 |            --9--            --9--            --9--
;       +----------------------------------------------------> TICKS
;        <---------------------- 1 x ---------------------->
;
;                     (0),(1),(2),(3),(4),(5)
BEEPPATTAB      .byte $02,$02,$02,$03,$0C,$02         ; HYPERWARP TRANSIT

                .byte $04,$03,$FF,$10,$07,$04         ; RED ALERT

                .byte $07,$04,$02,$02,$00,$07         ; ACKNOWLEDGE

                .byte $0B,$05,$FF,$20,$02,$0B         ; DAMAGE REPORT

                .byte $0E,$06,$08,$20,$00,$0E         ; MESSAGE FROM STARBASE


;*** Beeper sound pattern frequency table **************************************
BEEPFRQTAB      .byte $10,$FF                         ; (unused) (!)
                .byte $18,$FF                         ; HYPERWARP TRANSIT
                .byte $40,$60,$FF                     ; RED ALERT
                .byte $10,$10,$10,$FF                 ; ACKNOWLEDGE
                .byte $40,$20,$FF                     ; DAMAGE REPORT
                .byte $48,$40,$51,$FF                 ; MESSAGE FROM STARBASE

;*** Shape of blip in Attack Computer Display **********************************
BLIPSHAPTAB     .byte $84                             ; #....#..
                .byte $B4                             ; #.##.#..
                .byte $FC                             ; ######..
                .byte $B4                             ; #.##.#..
                .byte $84                             ; #....#..

;*** Initial x-coordinate (high byte) of our starship's photon torpedo *********
BARRELXTAB      .byte $FF                             ; Left barrel  = -256 (-$FF00) <KM>
                .byte $01                             ; Right barrel = +256 (+$0100) <KM>

;*** Maximum photon torpedo hit z-coordinate (high byte) ***********************
HITMAXZTAB      .byte $0C                             ; < 3328 ($0C**) <KM>
                .byte $0C                             ; < 3328 ($0C**) <KM>
                .byte $0C                             ; < 3328 ($0C**) <KM>
                .byte $0C                             ; < 3328 ($0C**) <KM>
                .byte $0E                             ; < 3840 ($0E**) <KM>
                .byte $0E                             ; < 3840 ($0E**) <KM>
                .byte $0E                             ; < 3840 ($0E**) <KM>
                .byte $20                             ; < 8448 ($20**) <KM>

;*** Minimum photon torpedo hit z-coordinate (high byte) ***********************
HITMINZTAB      .byte $00                             ; >=    0 ($00**) <KM>
                .byte $00                             ; >=    0 ($00**) <KM>
                .byte $00                             ; >=    0 ($00**) <KM>
                .byte $02                             ; >=  512 ($02**) <KM>
                .byte $04                             ; >= 1024 ($04**) <KM>
                .byte $06                             ; >= 1536 ($06**) <KM>
                .byte $08                             ; >= 2048 ($08**) <KM>
                .byte $0C                             ; >= 3072 ($0C**) <KM>

;*** Velocity of homing Zylon photon torpedo ***********************************
ZYLONHOMVELTAB  .byte NEG|1                           ;  -1 <KM/H> NOVICE mission
                .byte NEG|4                           ;  -4 <KM/H> PILOT mission
                .byte NEG|8                           ;  -8 <KM/H> WARRIOR mission
                .byte NEG|20                          ; -20 <KM/H> COMMANDER mission

;*** Zylon shape type table ****************************************************
ZYLONSHAPTAB    .byte SHAP_ZBASESTAR                  ; ZYLON BASESTAR
                .byte SHAP_ZFIGHTER                   ; ZYLON FIGHTER
                .byte SHAP_ZFIGHTER                   ; ZYLON FIGHTER
                .byte SHAP_ZFIGHTER                   ; ZYLON FIGHTER
                .byte SHAP_ZCRUISER                   ; ZYLON CRUISER
                .byte SHAP_ZCRUISER                   ; ZYLON CRUISER
                .byte SHAP_ZCRUISER                   ; ZYLON CRUISER
                .byte SHAP_ZFIGHTER                   ; ZYLON FIGHTER

;*** Zylon flight pattern table ************************************************
ZYLONFLPATTAB   .byte 4                               ; Flight pattern 4
                .byte 4                               ; Flight pattern 4
                .byte 0                               ; Attack Flight Pattern 0
                .byte 0                               ; Attack Flight Pattern 0
                .byte 0                               ; Attack Flight Pattern 0
                .byte 1                               ; Flight pattern 1
                .byte 0                               ; Attack Flight Pattern 0
                .byte 0                               ; Attack Flight Pattern 0

;*** Zylon velocity table ******************************************************
ZYLONVELTAB     .byte 62                              ; +62 <KM/H>
                .byte 30                              ; +30 <KM/H>
                .byte 16                              ; +16 <KM/H>
                .byte 8                               ;  +8 <KM/H>
                .byte 4                               ;  +4 <KM/H>
                .byte 2                               ;  +2 <KM/H>
                .byte 1                               ;  +1 <KM/H>
                .byte 0                               ;   0 <KM/H>
                .byte 0                               ;   0 <KM/H>
                .byte NEG|1                           ;  -1 <KM/H>
                .byte NEG|2                           ;  -2 <KM/H>
                .byte NEG|4                           ;  -4 <KM/H>
                .byte NEG|8                           ;  -8 <KM/H>
                .byte NEG|16                          ; -16 <KM/H>
                .byte NEG|30                          ; -30 <KM/H>
                .byte NEG|62                          ; -62 <KM/H>

;*** PLAYFIELD colors (including PLAYFIELD colors during DLI) ******************
PFCOLORTAB      .byte $A6                             ; PF0COLOR    = {GREEN}
                .byte $AA                             ; PF1COLOR    = {LIGHT GREEN}
                .byte $AF                             ; PF2COLOR    = {VERY LIGHT GREEN}
                .byte $00                             ; PF3COLOR    = {BLACK}
                .byte $00                             ; BGRCOLOR    = {BLACK}
                .byte $B8                             ; PF0COLORDLI = {LIGHT MINT}
                .byte $5A                             ; PF1COLORDLI = {MEDIUM PINK}
                .byte $FC                             ; PF2COLORDLI = {LIGHT ORANGE}
                .byte $5E                             ; PF3COLORDLI = {LIGHT PINK}
                .byte $90                             ; BGRCOLORDLI = {DARK BLUE}

;*** Vicinity mask table. Confines coordinates of space objects in sector ******
VICINITYMASKTAB .byte $FF                             ; <= 65535 ($FF**) <KM>
                .byte $FF                             ; <= 65535 ($FF**) <KM>
                .byte $3F                             ; <= 16383 ($3F**) <KM>
                .byte $0F                             ; <=  4095 ($0F**) <KM>
                .byte $3F                             ; <= 16383 ($3F**) <KM>
                .byte $7F                             ; <= 32767 ($7F**) <KM>
                .byte $FF                             ; <= 65535 ($FF**) <KM>
                .byte $FF                             ; <= 65535 ($FF**) <KM>

;*** Movement probability of sector types in Galactic Chart ********************
MOVEPROBTAB     .byte 0                               ; Empty sector    0% (  0:256)
                .byte 255                             ; 1 Zylon ship  100% (255:256)
                .byte 255                             ; 2 Zylon ships 100% (255:256)
                .byte 192                             ; 3 Zylon ships  75% (192:256)
                .byte 32                              ; 4 Zylon ships  13% ( 32:256)

;*** Galactic Chart sector offset to adjacent sector ***************************
COMPASSOFFTAB   .char -16                             ; NORTH
                .char -17                             ; NORTHWEST
                .char -1                              ; WEST
                .byte 15                              ; SOUTHWEST
                .byte 16                              ; SOUTH
                .byte 17                              ; SOUTHEAST
                .byte 1                               ; EAST
                .char -15                             ; NORTHEAST
                .byte 0                               ; CENTER

;*** Homing velocities of photon torpedoes 0..1 depending on distance to target 
HOMVELTAB       .byte 0                               ;  +0 <KM/H>
                .byte 8                               ;  +8 <KM/H>
                .byte 16                              ; +16 <KM/H>
                .byte 24                              ; +24 <KM/H>
                .byte 40                              ; +40 <KM/H>
                .byte 48                              ; +48 <KM/H>
                .byte 56                              ; +56 <KM/H>
                .byte 64                              ; +64 <KM/H>

;*** PLAYER shape color table (bits B7..4 of color/brightness) *****************
PLSHAPCOLORTAB  .byte $50                             ; PHOTON TORPEDO          = {PURPLE}
                .byte $00                             ; ZYLON FIGHTER           = {GRAY}
                .byte $20                             ; STARBASE RIGHT          = {ORANGE}
                .byte $20                             ; STARBASE CENTER         = {ORANGE}
                .byte $20                             ; STARBASE LEFT           = {ORANGE}
                .byte $00                             ; TRANSFER VESSEL         = {GRAY}
                .byte $A0                             ; METEOR                  = {GREEN}
                .byte $00                             ; ZYLON CRUISER           = {GRAY}
                .byte $00                             ; ZYLON BASESTAR          = {GRAY}
                .byte $9F                             ; HYPERWARP TARGET MARKER = {SKY BLUE}

;*** PLAYER shape brightness table (bits B3..0 of color/brightness) ************
PLSHAPBRITTAB   .byte $0E                             ; ##############..
                .byte $0E                             ; ##############..
                .byte $0E                             ; ##############..
                .byte $0C                             ; ############....
                .byte $0C                             ; ############....
                .byte $0C                             ; ############....
                .byte $0A                             ; ##########......
                .byte $0A                             ; ##########......
                .byte $0A                             ; ##########......
                .byte $08                             ; ########........
                .byte $08                             ; ########........
                .byte $08                             ; ########........
                .byte $06                             ; ######..........
                .byte $06                             ; ######..........
                .byte $04                             ; ####............
                .byte $04                             ; ####............

;*** PHOTON TORPEDO LAUNCHED noise bit and volume (stored in reverse order) ****
NOISETORPVOLTAB .byte $8A                             ; ##########.....
                .byte $8F                             ; ###############
                .byte $8D                             ; #############..
                .byte $8B                             ; ###########....
                .byte $89                             ; #########......
                .byte $87                             ; #######........
                .byte $85                             ; ######.........
                .byte $83                             ; ###............

;*** PHOTON TORPEDO LAUNCHED noise frequency table (stored in reverse order) ***
NOISETORPFRQTAB .byte $00                             ;
                .byte $04                             ;
                .byte $01                             ;
                .byte $04                             ;
                .byte $01                             ;
                .byte $04                             ;
                .byte $01                             ;
                .byte $04                             ;

                .byte $07                             ; (unused)

                .byte $00                             ; Always 0 for cartridges
                .byte $80                             ; On SYSTEM RESET jump to INITCOLD via
                .word INITCOLD                        ; Cartridge Initialization Address
