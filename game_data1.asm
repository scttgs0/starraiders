;*******************************************************************************
;*                                                                             *
;*                G A M E   D A T A   ( P A R T   1   O F   2 )                *
;*                                                                             *
;*******************************************************************************

;*** Number of space objects ***************************************************

NUMSPCOBJ_PL    = 5                               ; Number of PLAYER space objects
NUMSPCOBJ_STARS = 12                              ; Number of PLAYFIELD space objects (stars)
NUMSPCOBJ_NORM  = NUMSPCOBJ_PL+NUMSPCOBJ_STARS    ; Normal number of space objects
NUMSPCOBJ_ALL   = 49                              ; Maximum number of space objects

;*** PLAYER shape data offsets *************************************************

shapeTORPEDO    = $00                   ; Photon torpedo
shapeZFIGHTER   = $10                   ; Zylon fighter
shapeSTARBASEL  = $20                   ; Starbase (left part)
shapeSTARBASEC  = $30                   ; Starbase (center part)
shapeSTARBASER  = $40                   ; Starbase (right part)
shapeTRANSVSSL  = $50                   ; Transfer vessel
shapeMETEOR     = $60                   ; Meteor
shapeZCRUISER   = $70                   ; Zylon cruiser
shapeZBASESTAR  = $80                   ; Zylon basestar
shapeHYPERWARP  = $90                   ; Hyperwarp Target Marker

;*** ROM character set constants ***********************************************
rom_Spc         = $00                   ; ROM character ' '
rom_Dot         = $0E                   ; ROM character '.'
rom_0           = $10                   ; ROM character '0'
rom_1           = $11                   ; ROM character '1'
rom_2           = $12                   ; ROM character '2'
rom_3           = $13                   ; ROM character '3'
rom_4           = $14                   ; ROM character '4'
rom_5           = $15                   ; ROM character '5'
rom_9           = $19                   ; ROM character '9'
rom_Colon       = $1A                   ; ROM character ':'
rom_A           = $21                   ; ROM character 'A'
rom_C           = $23                   ; ROM character 'C'
rom_D           = $24                   ; ROM character 'D'
rom_E           = $25                   ; ROM character 'E'
rom_G           = $27                   ; ROM character 'G'
rom_L           = $2C                   ; ROM character 'L'
rom_N           = $2E                   ; ROM character 'N'
rom_P           = $30                   ; ROM character 'P'
rom_R           = $32                   ; ROM character 'R'
rom_S           = $33                   ; ROM character 'S'
rom_T           = $34                   ; ROM character 'T'
rom_W           = $37                   ; ROM character 'W'
rom_Y           = $39                   ; ROM character 'Y'

;*** Custom character set constants ********************************************
ccs_Col1        = $40                   ; COLOR1 bits for text in GR1/2 text mode
ccs_Col2        = $80                   ; COLOR2 bits for text in GR1/2 text mode
ccs_Col3        = $C0                   ; COLOR3 bits for text in GR1/2 text mode

ccs_0           = 0                     ; Custom character '0'
ccs_1           = 1                     ; Custom character '1'
ccs_2           = 2                     ; Custom character '2'
ccs_3           = 3                     ; Custom character '3'
ccs_4           = 4                     ; Custom character '4'
ccs_5           = 5                     ; Custom character '5'
ccs_6           = 6                     ; Custom character '6'
ccs_7           = 7                     ; Custom character '7'
ccs_8           = 8                     ; Custom character '8'
ccs_9           = 9                     ; Custom character '9'
ccs_Spc         = 10                    ; Custom character ' '
ccs_Colon       = 11                    ; Custom character ':'
ccs_BorderSW    = 12                    ; Custom character 'BORDER SOUTHWEST'
ccs_E           = 13                    ; Custom character 'E'
ccs_Inf         = 14                    ; Custom character 'INFINITY'
ccs_Minus       = 15                    ; Custom character '-'
ccs_Plus        = 16                    ; Custom character '+'
CCS_PHI         = 17                    ; Custom character 'PHI'
ccs_V           = 18                    ; Custom character 'V'
ccs_R           = 19                    ; Custom character 'R'
ccs_Theta       = 20                    ; Custom character 'THETA'
ccs_K           = 21                    ; Custom character 'K'
ccs_T           = 22                    ; Custom character 'T'
ccs_C           = 23                    ; Custom character 'C'
ccs_BorderS     = 24                    ; Custom character 'BORDER SOUTH'
ccs_BorderW     = 25                    ; Custom character 'BORDER WEST'
ccs_CornerSW    = 26                    ; Custom character 'CORNER SOUTHWEST'
ccs_Starbase    = 27                    ; Custom character 'STARBASE SECTOR'
ccs_4Zylons     = 28                    ; Custom character '4-ZYLON SECTOR'
ccs_3Zylons     = 29                    ; Custom character '3-ZYLON SECTOR'
ccs_2Zylons     = 30                    ; Custom character '2-ZYLON SECTOR'
