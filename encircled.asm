;*******************************************************************************
;*                                                                             *
;*                                IsSurrounded                                 *
;*                                                                             *
;*               Check if a sector is surrounded by Zylon units                *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Checks if a sector of the Galactic Chart is surrounded by Zylon units in the
; adjacent NORTH, EAST, SOUTH, and WEST sectors.
;
; INPUT
;
;   X = Sector of Galactic Chart. Used values are: $00..$7F with, for example,
;     $00 -> NORTHWEST corner sector
;     $0F -> NORTHEAST corner sector
;     $70 -> SOUTHWEST corner sector
;     $7F -> SOUTHWEST corner sector
;
; OUTPUT
;
;   A = Returns if the sector is surrounded by Zylon units in the adjacent
;       NORTH, EAST, SOUTH, and WEST sectors.
;       0 -> Sector is not surrounded
;     > 0 -> Sector is surrounded


;======================================
; Check if a sector is surrounded by
; Zylon units
;======================================
IsSurrounded    .proc
                lda gcMemMap-1,X        ; Check WEST sector
                beq _XIT                ;

                lda gcMemMap+1,X        ; Check EAST sector
                beq _XIT                ;

                lda gcMemMap-16,X       ; Check NORTH sector
                beq _XIT                ;

                lda gcMemMap+16,X       ; Check SOUTH sector
_XIT            rts
                .endproc
