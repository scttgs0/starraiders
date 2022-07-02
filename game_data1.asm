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

SHAP_TORPEDO    = $00                   ; Photon torpedo
SHAP_ZFIGHTER   = $10                   ; Zylon fighter
SHAP_STARBASEL  = $20                   ; Starbase (left part)
SHAP_STARBASEC  = $30                   ; Starbase (center part)
SHAP_STARBASER  = $40                   ; Starbase (right part)
SHAP_TRANSVSSL  = $50                   ; Transfer vessel
SHAP_METEOR     = $60                   ; Meteor
SHAP_ZCRUISER   = $70                   ; Zylon cruiser
SHAP_ZBASESTAR  = $80                   ; Zylon basestar
SHAP_HYPERWARP  = $90                   ; Hyperwarp Target Marker

;*** ROM character set constants ***********************************************
ROM_SPC         = $00                   ; ROM character ' '
ROM_DOT         = $0E                   ; ROM character '.'
ROM_0           = $10                   ; ROM character '0'
ROM_1           = $11                   ; ROM character '1'
ROM_2           = $12                   ; ROM character '2'
ROM_3           = $13                   ; ROM character '3'
ROM_4           = $14                   ; ROM character '4'
ROM_5           = $15                   ; ROM character '5'
ROM_9           = $19                   ; ROM character '9'
ROM_COLON       = $1A                   ; ROM character ':'
ROM_A           = $21                   ; ROM character 'A'
ROM_C           = $23                   ; ROM character 'C'
ROM_D           = $24                   ; ROM character 'D'
ROM_E           = $25                   ; ROM character 'E'
ROM_G           = $27                   ; ROM character 'G'
ROM_L           = $2C                   ; ROM character 'L'
ROM_N           = $2E                   ; ROM character 'N'
ROM_P           = $30                   ; ROM character 'P'
ROM_R           = $32                   ; ROM character 'R'
ROM_S           = $33                   ; ROM character 'S'
ROM_T           = $34                   ; ROM character 'T'
ROM_W           = $37                   ; ROM character 'W'
ROM_Y           = $39                   ; ROM character 'Y'

;*** Custom character set constants ********************************************
CCS_COL1        = $40                   ; COLOR1 bits for text in GR1/2 text mode
CCS_COL2        = $80                   ; COLOR2 bits for text in GR1/2 text mode
CCS_COL3        = $C0                   ; COLOR3 bits for text in GR1/2 text mode

CCS_0           = 0                     ; Custom character '0'
CCS_1           = 1                     ; Custom character '1'
CCS_2           = 2                     ; Custom character '2'
CCS_3           = 3                     ; Custom character '3'
CCS_4           = 4                     ; Custom character '4'
CCS_5           = 5                     ; Custom character '5'
CCS_6           = 6                     ; Custom character '6'
CCS_7           = 7                     ; Custom character '7'
CCS_8           = 8                     ; Custom character '8'
CCS_9           = 9                     ; Custom character '9'
CCS_SPC         = 10                    ; Custom character ' '
CCS_COLON       = 11                    ; Custom character ':'
CCS_BORDERSW    = 12                    ; Custom character 'BORDER SOUTHWEST'
CCS_E           = 13                    ; Custom character 'E'
CCS_INF         = 14                    ; Custom character 'INFINITY'
CCS_MINUS       = 15                    ; Custom character '-'
CCS_PLUS        = 16                    ; Custom character '+'
CCS_PHI         = 17                    ; Custom character 'PHI'
CCS_V           = 18                    ; Custom character 'V'
CCS_R           = 19                    ; Custom character 'R'
CCS_THETA       = 20                    ; Custom character 'THETA'
CCS_K           = 21                    ; Custom character 'K'
CCS_T           = 22                    ; Custom character 'T'
CCS_C           = 23                    ; Custom character 'C'
CCS_BORDERS     = 24                    ; Custom character 'BORDER SOUTH'
CCS_BORDERW     = 25                    ; Custom character 'BORDER WEST'
CCS_CORNERSW    = 26                    ; Custom character 'CORNER SOUTHWEST'
CCS_STARBASE    = 27                    ; Custom character 'STARBASE SECTOR'
CCS_4ZYLONS     = 28                    ; Custom character '4-ZYLON SECTOR'
CCS_3ZYLONS     = 29                    ; Custom character '3-ZYLON SECTOR'
CCS_2ZYLONS     = 30                    ; Custom character '2-ZYLON SECTOR'
