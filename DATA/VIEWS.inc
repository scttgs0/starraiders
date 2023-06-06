;*** Header text of Long-Range Scan view (shares spaces with following header) *
LRSHEADER       .byte $00,$00,$6C,$6F,$6E,$67,$00,$72 ; "  LONG RANGE SCAN"

                .byte $61,$6E,$67,$65,$00,$73,$63,$61

                .byte $6E

;*** Header text of Aft view (shares spaces with following header) *************
AFTHEADER       .byte $00,$00,$00,$00,$00,$00,$61,$66 ; "      AFT VIEW   "

                .byte $74,$00,$76,$69,$65,$77,$00,$00

                .byte $00

;*** Header text of Galactic Chart view ****************************************
GCHEADER        .byte $00,$00,$00,$67,$61,$6C,$61,$63 ; "   GALACTIC CHART   "

                .byte $74,$69,$63,$00,$63,$68,$61,$72

                .byte $74,$00,$00,$00

;*** Display List of Galactic Chart view ***************************************
DLSTGC          .byte $60                             ; BLK7
                .byte $46,<GCHEADER,>GCHEADER         ; GR1 @ GCHEADER
                .byte $F0                             ; BLK8 + DLI
                .byte $47,<GCPFMEM,>GCPFMEM           ; GR2 @ GCPFMEM
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $80                             ; BLK1 + DLI
                .byte $46,<TITLETXT,>TITLETXT         ; GR1 @ TITLETXT
                .byte $46,<GCTXT,>GCTXT               ; GR1 @ GCTXT
                .byte $06                             ; GR1
                .byte $06                             ; GR1
                .byte $41,<DSPLST,>DSPLST             ; jmp @ DSPLST
