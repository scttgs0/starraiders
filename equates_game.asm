;*******************************************************************************
;*                                                                             *
;*                           G A M E   S Y M B O L S                           *
;*                                                                             *
;*******************************************************************************

MISSIONLEVEL    = $62
FKEYCODE        = $63
ISDEMOMODE      = $64
NEWTITLEPHR     = $65
IDLECNTHI       = $66
ISVBISYNC       = $67
MEMPTR          = $68

DIVIDEND        = $6A
JOYSTICKDELTA   = $6D

VELOCITYLO      = $70
NEWVELOCITY     = $71
COUNT8          = $72
EXPLLIFE        = $73
CLOCKTIM        = $74
DOCKSTATE       = $75
COUNT256        = $76
IDLECNTLO       = $77
ZYLONUNITTIM    = $78
MAXSPCOBJIND    = $79
OLDMAXSPCOBJIND = $7A
ISSTARBASESECT  = $7B
ISTRACKCOMPON   = $7C
DRAINSHIELDS    = $7D
DRAINATTCOMP    = $7E
ENERGYCNT       = $7F
DRAINENGINES    = $80
SHIELDSCOLOR    = $81
PL3HIT          = $82
PL4HIT          = $83
OLDTRIG0        = $84

ISTRACKING      = $86
BARRELNR        = $87
LOCKONLIFE      = $88
PLTRACKED       = $89
HITBADNESS      = $8A
REDALERTLIFE    = $8B
vWarpDeprRow    = $8C
vWarpDeprColumn = $8D
vWarpArrvRow    = $8E
vWarpArrvColumn = $8F
vCurrentSector  = $90
WARPENERGY      = $91
ARRVSECTOR      = $92
HUNTSECTOR      = $93
HUNTSECTCOLUMN  = $94
HUNTSECTROW     = $95
NEWZYLONDIST    = $96
OLDZYLONDIST    = $9E
HUNTTIM         = $9F
BLIPCOLUMN      = $A0
BLIPROW         = $A1
BLIPCYCLECNT    = $A2
ISINLOCKON      = $A3
DIRLEN          = $A4
PENROW          = $A5
PENCOLUMN       = $A6
CTRLDZYLON      = $A7
ZYLONFLPAT0     = $A8
ZYLONFLPAT1     = $A9
MILESTTIM0      = $AA
MILESTTIM1      = $AB
MILESTVELINDZ0  = $AC
MILESTVELINDZ1  = $AD
MILESTVELINDX0  = $AE
MILESTVELINDX1  = $AF
MILESTVELINDY0  = $B0
MILESTVELINDY1  = $B1
ZYLONVELINDZ0   = $B2
ZYLONVELINDZ1   = $B3
ZYLONVELINDX0   = $B4
ZYLONVELINDX1   = $B5
ZYLONVELINDY0   = $B6
ZYLONVELINDY1   = $B7
ISBACKATTACK0   = $B8
ISBACKATTACK1   = $B9
ZYLONTIMX0      = $BA
ZYLONTIMX1      = $BB
ZYLONTIMY0      = $BC
ZYLONTIMY1      = $BD
TORPEDODELAY    = $BE
ZYLONATTACKER   = $BF
WARPSTATE       = $C0
VELOCITYHI      = $C1
TRAILDELAY      = $C2
TRAILIND        = $C3
WARPTEMPCOLUMN  = $C4
WARPTEMPROW     = $C5
VEERMASK        = $C6
VICINITYMASK    = $C7
JOYSTICKX       = $C8
JOYSTICKY       = $C9
KEYCODE         = $CA
SCORE           = $CB
SCOREDRANKIND   = $CD
SCOREDCLASSIND  = $CE
TITLELIFE       = $CF
SHIPVIEW        = $D0
TITLEPHR        = $D1
BEEPFRQIND      = $D2
BEEPREPEAT      = $D3
BEEPTONELIFE    = $D4
BEEPPAUSELIFE   = $D5
BEEPPRIORITY    = $D6
BEEPFRQSTART    = $D7
BEEPLIFE        = $D8
BEEPTOGGLE      = $D9
NOISETORPTIM    = $DA
NOISEEXPLTIM    = $DB
NOISEAUDC2      = $DC
NOISEAUDC3      = $DD
NOISEAUDF1      = $DE
NOISEAUDF2      = $DF
NOISEFRQINC     = $E0
NOISELIFE       = $E1
NOISEZYLONTIM   = $E2
NOISEHITLIFE    = $E3
PL0SHAPOFF      = $E4
PL1SHAPOFF      = $E5
PL2SHAPOFF      = $E6
PL3SHAPOFF      = $E7
PL4SHAPOFF      = $E8
PL0LIFE         = $E9
PL1LIFE         = $EA
PL2LIFE         = $EB
PL3LIFE         = $EC
PL4LIFE         = $ED
PL0COLOR        = $EE
PL1COLOR        = $EF
PL2COLOR        = $F0
PL3COLOR        = $F1
PF0COLOR        = $F2
PF1COLOR        = $F3
PF2COLOR        = $F4
PF3COLOR        = $F5
BGRCOLOR        = $F6
PF0COLORDLI     = $F7
PF1COLORDLI     = $F8
PF2COLORDLI     = $F9
PF3COLORDLI     = $FA
BGRCOLORDLI     = $FB

DSPLST          = $0280
PL4DATA         = $0300
PL0DATA         = $0400
PL1DATA         = $0500
PL2DATA         = $0600
PL3DATA         = $0700
PFMEMROWLO      = $0800
PFMEMROWHI      = $0864
gcMemMap        = $08C9
PANELTXT        = $0949
VELOCD1         = $094B
KILLCNTD1       = $0950
ENERGYD1        = $0955
TRACKC1         = $095A
TRACKDIGIT      = $095C
THETAC1         = $0960
PHIC1           = $0966
RANGEC1         = $096C
GCTXT           = $0971
GCWARPD1        = $097D
GCTRGCNT        = $098D
GCSTATPHO       = $0992
GCSTATENG       = $0993
GCSTATSHL       = $0994
GCSTATCOM       = $0995
GCSTATLRS       = $0996
GCSTATRAD       = $0997
GCSTARDAT       = $09A3
ZPOSSIGN        = $09AD
PL2ZPOSSIGN     = $09AF
PL3ZPOSSIGN     = $09B0
PL4ZPOSSIGN     = $09B1
XPOSSIGN        = $09DE
PL2XPOSSIGN     = $09E0
PL3XPOSSIGN     = $09E1
PL4XPOSSIGN     = $09E2
YPOSSIGN        = $0A0F
PL2YPOSSIGN     = $0A11
PL3YPOSSIGN     = $0A12
PL4YPOSSIGN     = $0A13
ZPOSHI          = $0A40
PL0ZPOSHI       = $0A40
PL2ZPOSHI       = $0A42
PL3ZPOSHI       = $0A43
PL4ZPOSHI       = $0A44
XPOSHI          = $0A71
PL2XPOSHI       = $0A73
PL3XPOSHI       = $0A74
PL4XPOSHI       = $0A75
YPOSHI          = $0AA2
PL2YPOSHI       = $0AA4
PL3YPOSHI       = $0AA5
PL4YPOSHI       = $0AA6
ZPOSLO          = $0AD3
PL2ZPOSLO       = $0AD5
PL3ZPOSLO       = $0AD6
PL4ZPOSLO       = $0AD7
XPOSLO          = $0B04
PL2XPOSLO       = $0B06
PL3XPOSLO       = $0B07
PL4XPOSLO       = $0B08
YPOSLO          = $0B35
PL2YPOSLO       = $0B37
PL3YPOSLO       = $0B38
PL4YPOSLO       = $0B39
ZVEL            = $0B66
PL0ZVEL         = $0B66
PL1ZVEL         = $0B67
PL2ZVEL         = $0B68
PL3ZVEL         = $0B69
PL4ZVEL         = $0B6A
XVEL            = $0B97
PL0XVEL         = $0B97
PL1XVEL         = $0B98
PL2XVEL         = $0B99
PL3XVEL         = $0B9A
PL4XVEL         = $0B9B
YVEL            = $0BC8
PL0YVEL         = $0BC8
PL1YVEL         = $0BC9
PL2YVEL         = $0BCA
PL3YVEL         = $0BCB
PL4YVEL         = $0BCC
PIXELROWNEW     = $0BF9
PL0ROWNEW       = $0BF9
PL1ROWNEW       = $0BFA
PL2ROWNEW       = $0BFB
PL3ROWNEW       = $0BFC
PL4ROWNEW       = $0BFD
PIXELCOLUMN     = $0C2A
PL0COLUMN       = $0C2A
PL1COLUMN       = $0C2B
PL2COLUMN       = $0C2C
PL3COLUMN       = $0C2D
PL4COLUMN       = $0C2E
PIXELROW        = $0C5B
PL0ROW          = $0C5B
PL1ROW          = $0C5C
PL2ROW          = $0C5D
PL3ROW          = $0C5E
PL4ROW          = $0C5F
PIXELBYTEOFF    = $0C8C
PL0SHAPTYPE     = $0C8C
PL1SHAPTYPE     = $0C8D
PL2SHAPTYPE     = $0C8E
PL3SHAPTYPE     = $0C8F
PL4SHAPTYPE     = $0C90
PIXELSAVE       = $0CBD
PL0HEIGHT       = $0CBD
PL1HEIGHT       = $0CBE
PL2HEIGHT       = $0CBF
PL3HEIGHT       = $0CC0
PL4HEIGHT       = $0CC1
PIXELBYTE       = $0CEE
PL0HEIGHTNEW    = $0CEE
PL1HEIGHTNEW    = $0CEF
PL2HEIGHTNEW    = $0CF0
PL3HEIGHTNEW    = $0CF1
PL4HEIGHTNEW    = $0CF2
TITLETXT        = $0D1F
gcPfMem         = $0D35
MAPTO80         = $0DE9
MAPTOBCD99      = $0EE9
PFMEM           = $1000
