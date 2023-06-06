
;*******************************************************************************
;*                                                                             *
;*                                  GAMEOVER                                   *
;*                                                                             *
;*                              Handle game over                               *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Handles game over, including calculating the scored rank and class.
;
; This subroutine has two entry points: 
;
; (1)  GAMEOVER ($B10A) is entered at the end of a failed mission (mission
;      aborted, zero energy, or starship destroyed by Zylon fire), essentially
;      shutting down our starship. Code execution continues into GAMEOVER2
;      ($B121) below.
;
; (2)  GAMEOVER2 ($B121) is entered at the end of a successful mission (all
;      Zylon ships destroyed). It puts the game in demo mode, enqueues the
;      corresponding game over message, and calculates the scored rank and
;      class.
;
;      The scored rank and class are based on the total score. This is the score
;      accumulated during the game plus a mission bonus, which depends on the
;      mission level and on how the mission ended (mission complete, mission
;      aborted, or starship destroyed by Zylon fire). The mission bonus is
;      picked from table BONUSTAB ($BEDD).
;
;      The scored rank index is taken from bits B8..4 of the total score and
;      limited to values of 0..18. It indexes table RANKTAB ($BEE9) for the rank
;      string. The rank string is displayed in subroutine SETTITLE ($B223).
;
;      The scored class index is taken from bits B3..0 (for rank indices 0,
;      11..14) and computed from bits B4..1 (for rank indices 1..10 and 15..18).
;      It takes values of 0..15. It indexes table CLASSTAB ($BEFC) for the class
;      digit. The class digit is displayed in subroutine SETTITLE ($B223).
;
;      For quick lookup, the following table lists rank and class from the total
;      score. Use the table as follows: Pick the cell with the closest value
;      less or equal to your score then read the rank and class off the left and
;      the top of the table, respectively.
;
;      For example: A score of 90 results in a ranking of "Novice Class 4", a
;      score of 161 results in a ranking of "Pilot Class 3".
;
; +------------------------------+---------------------------------------------------------------+
; |     Minimum Total Score      |                        Class Index                            |
; |                              |  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15|
; +-------+----------------------+---------------------------------------------------------------+
; | Rank  |                      |                           Class                               |
; | Index |         Rank         |  5   5   5   4   4   4   4   3   3   3   2   2   2   1   1   1|
; +-------+----------------------+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
; |     0 | Galactic Cook        |  0|  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12| 13| 14| 15|
; |     1 | Garbage Scow Captain | 16| 18| 20| 22| 24| 26| 28| 30|   |   |   |   |   |   |   |   |
; |     2 | Garbage Scow Captain |   |   |   |   |   |   |   |   | 32| 34| 36| 38| 40| 42| 44| 46|
; |     3 | Rookie               | 48| 50| 52| 54| 56| 58| 60| 62|   |   |   |   |   |   |   |   |
; |     4 | Rookie               |   |   |   |   |   |   |   |   | 64| 66| 68| 70| 72| 74| 76| 78|
; |     5 | Novice               | 80| 82| 84| 86| 88| 90| 92| 94|   |   |   |   |   |   |   |   |
; |     6 | Novice               |   |   |   |   |   |   |   |   | 96| 98|100|102|104|106|108|110|
; |     7 | Ensign               |112|114|116|118|120|122|124|126|   |   |   |   |   |   |   |   |
; |     8 | Ensign               |   |   |   |   |   |   |   |   |128|130|132|134|136|138|140|142|
; |     9 | Pilot                |144|146|148|150|152|154|156|158|   |   |   |   |   |   |   |   |
; |    10 | Pilot                |   |   |   |   |   |   |   |   |160|162|164|166|168|170|172|174|
; |    11 | Ace                  |176|177|178|179|180|181|182|183|184|185|186|187|188|189|190|191|
; |    12 | Lieutenant           |192|193|194|195|196|197|198|199|200|201|202|203|204|205|206|207|
; |    13 | Warrior              |208|209|210|211|212|213|214|215|216|217|218|219|220|221|222|223|
; |    14 | Captain              |224|225|226|227|228|229|230|231|232|233|234|235|236|237|238|239|
; |    15 | Commander            |240|242|244|246|248|250|252|254|   |   |   |   |   |   |   |   |
; |    16 | Commander            |   |   |   |   |   |   |   |   |256|258|260|262|264|266|268|270|
; |    17 | Star Commander       |272|274|276|278|280|282|284|286|   |   |   |   |   |   |   |   |
; |    18 | Star Commander       |   |   |   |   |   |   |   |   |288|290|292|294|296|298|300|302|
; +-------+----------------------+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;
;      NOTE: This subroutine also clears the vertical and horizontal joystick
;      directions.
;
; INPUT
;
;   X = Offset to index table BONUSTAB ($BEDD) of mission bonus values. Used
;       values are:
;     $00 -> Mission complete
;     $04 -> Mission was aborted due to zero energy
;     $08 -> Our starship was destroyed by Zylon fire
;
;   Y = Title phrase offset. Used values are:
;     $3F -> "MISSION COMPLETE"
;     $31 -> "MISSION ABORTED ZERO ENERGY"
;     $23 -> "SHIP DESTROYED BY ZYLON FIRE"

;*** Game over (Mission failed) ************************************************


;======================================
; End of a failed mission
;======================================
GAMEOVER        .proc
                lda #0                  ;
                sta PL3LIFE             ; PLAYER3 lifetime := 0 game loops
                sta BEEPPRIORITY        ; Mute beeper
                sta TITLEPHR            ; Clear title line
                sta REDALERTLIFE        ; Red alert flash lifetime := 0 game loops
                ;--sta SID2_CTRL1               ; Mute audio channel 4
                sta NEWVELOCITY         ; Shut down Engines
                sta SHIELDSCOLOR        ; Set Shields color to {BLACK}
                sta DRAINSHIELDS        ; Switch off Shields
                sta WARPSTATE           ; Disengage hyperwarp
                sta VELOCITYHI          ; Turn off hyperwarp velocity

                .endproc

                ;[fall-through]


;*** Game over (Mission successful) ********************************************


;======================================
; End of a successful mission
;======================================
GAMEOVER2       .proc
                lda #$FF                ; Enter demo mode
                sta ISDEMOMODE          ;

                sty NEWTITLEPHR         ; Enqueue title phrase

;*** Calculate total score *****************************************************
                txa                     ;
                ora MISSIONLEVEL        ;
                tax                     ;
                lda BONUSTAB,X          ; Retrieve mission bonus
                clc                     ; Add mission bonus and game score
                adc SCORE               ;
                tax                     ;
                lda #0                  ;

                sta JOYSTICKY           ; Clear vertical joystick delta
                sta JOYSTICKX           ; Clear horizontal joystick delta

                adc SCORE+1             ;
                bmi _XIT                ; Return if total score < 0 (= total score of 0)

;*** Calculate scored rank *****************************************************
                lsr                     ;
                txa                     ;
                ror A                   ;
                lsr                     ;
                lsr                     ;
                lsr                     ; Use bits B8..4 of total score as rank index
                cmp #19                 ; Limit scored rank index to 0..18
                bcc _1                  ;

                lda #18                 ;
                ldx #15                 ; Prep class index of 15
_1              sta SCOREDRANKIND       ; Store scored rank index

;*** Calculate scored class ****************************************************
                tay                     ;
                txa                     ;
                cpy #0                  ;
                beq _3                  ;
                cpy #11                 ;
                bcc _2                  ;
                cpy #15                 ;
                bcc _3                  ;
_2              lsr                     ;
                eor #$08                ;
_3              and #$0F                ;
                sta SCOREDCLASSIND      ; Store scored class index, is 0..15

_XIT            rts
                .endproc
