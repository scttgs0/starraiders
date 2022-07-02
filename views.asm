;*** Header text of Long-Range Scan view (shares spaces with following header) *
LRSHEADER       .text "  LONG RANGE SCAN"

;*** Header text of Aft view (shares spaces with following header) *************
AFTHEADER       .text "      AFT VIEW   "

;*** Header text of Galactic Chart view ****************************************
GCHEADER        .text "   GALACTIC CHART   "

;*** Display List of Galactic Chart view ***************************************
DLSTGC          .byte AEMPTY7
                .byte $06+ALMS
                    .word GCHEADER                    ; GR1 @ GCHEADER
                .byte AEMPTY8+ADLI
                .byte $07+ALMS
                    .word gcPfMem                     ; GR2 @ gcPfMem
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte $07                             ; GR2
                .byte AEMPTY1+ADLI
                .byte $06+ALMS
                    .word TITLETXT                    ; GR1 @ TITLETXT
                .byte $06+ALMS
                    .word GCTXT                       ; GR1 @ GCTXT
                .byte $06                             ; GR1
                .byte $06                             ; GR1
                .byte AVB+AJMP
                    .word DSPLST
