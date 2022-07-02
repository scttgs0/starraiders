;*******************************************************************************
;*                                                                             *
;*                                  UPDTITLE                                   *
;*                                                                             *
;*                              Update title line                              *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Updates the title phrase displayed in the title line.
;
; If no title phrase has been set then fetch the offset of the next ("enqueued")
; title phrase to be displayed. If one has been set then code execution
; continues into subroutine SETTITLE ($B223), otherwise code execution returns.
;
; If a title phrase has been set then decrement the lifetime of the currently
; displayed title phrase segment. If its lifetime has reached a value of 0 then
; branch to subroutine SETTITLE ($B223) to display the next segment.

UPDTITLE        lda TITLEPHR            ; Skip if no title phrase set
                beq SKIP175             ;

                dec TITLELIFE           ; Decrement title phrase segment lifetime
                beq SKIP176             ; If lifetime expired show next title segment

SKIP174         rts                     ; Return

SKIP175         ldy NEWTITLEPHR         ; Prep enqueued new title phrase
                beq SKIP174             ; Return if not set

;*******************************************************************************
;*                                                                             *
;*                                  SETTITLE                                   *
;*                                                                             *
;*                       Set title phrase in title line                        *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Displays a title phrase in the title line. 
;
; INTRODUCTION
;
; Title phrases are picked from the title phrase table PHRASETAB ($BBAA). They
; consist of one or more phrase tokens. Each token is a byte representing a word
; in word table WORDTAB ($BC2B). Two special tokens are placeholders for the
; scored class string ($FC) and scored rank string ($FD).
;
; A title phrase is split up into one or more title phrase segments, each
; fitting into the title line. One title phrase segment is displayed after the
; other after a delay called the "title segment lifetime".
;
; Phrase tokens, except the tokens for the scored class ($FC) and for the scored
; rank ($FD), contain the number of a word in word table WORDTAB ($BC2B) and may
; contain an end-of-segment or end-of-phrase marker bit.
;
; DETAILS
;
; The Display List is modified by subroutine MODDLST ($ADF1) to display the
; title line. Then, the title line is cleared and the words of the title phrase
; are copied into it using the passed offset into title phrase table PHRASETAB
; ($BBAA). If the offset has a value of $FF the title line is hidden in
; subroutine MODDLST ($ADF1). 
;
; INPUT
;
;   Y = Offset into title phrase table PHRASETAB ($BBAA). Used values are:
;     $FF  -> Hide title line
;     else -> Offset into title phrase table PHRASETAB ($BBAA), with explicitly
;             used values:
;
;     $01 -> "COMPUTER ON"
;     $04 -> "COMPUTER OFF"
;     $07 -> "SHIELDS ON"
;     $09 -> "SHIELDS OFF"
;     $0B -> "COMPUTER TRACKING ON"
;     $0E -> "TRACKING OFF"
;     $13 -> "STARBASE SURROUNDED"
;     $15 -> "STARBASE DESTROYED"
;     $1F -> "DOCKING ABORTED"
;     $21 -> "TRANSFER COMPLETE"
;     $4A -> "NOVICE MISSION"
;     $4C -> "PILOT MISSION"
;     $4E -> "WARRIOR MISSION"
;     $50 -> "COMMANDER MISSION"
;     $52 -> "DAMAGE CONTROL..."
;     $75 -> "RED ALERT"

L_WORD          = $6A                   ; Saves word number of WORDTAB ($BC2A). Used values
                                        ; are $00..$3F.
L_COLUMNPOS     = $6B                   ; Saves cursor column position during copying text
                                        ; into title line
L_TOKEN         = $6C                   ; Saves title phrase token from PHRASETAB ($BBAA),
                                        ; contains bit-encoded information about one word in
                                        ; the title phrase:
                                        ; B7..6 = %00 -> Copy next word to title line
                                        ; B7..6 = %01 -> End-of-phrase reached, apply short
                                        ;                delay, then hide title line. Title
                                        ;                segment lifetime = 60 game loops.
                                        ; B7..6 = %10 -> End-of-segment reached. Title
                                        ;                segment lifetime = 60 game loops
                                        ; B7..6 = %11 -> End-of-phrase reached, apply long
                                        ;                delay, then hide title line. Title
                                        ;                segment lifetime = 254 game loops.
                                        ;                Used with title phrases
                                        ;                  "STARBASE SURROUNDED"
                                        ;                  "STARBASE DESTROYED"
                                        ;                  "HYPERSPACE"
                                        ;                  "RED ALERT"
                                        ; B5..0       -> Word number of WORDTAB ($BC2A)

SETTITLE        sty TITLEPHR            ; Save title phrase offset

                ldy #$23                ; Show title line
                ldx #$0F                ;
                lda #$07                ;
                jsr MODDLST             ;

;*** Init cursor column position and clear title line **************************
SKIP176         ldx #19                 ; There are 19(+1) characters to clear
                lda #0                  ;
                sta L_COLUMNPOS         ; Init cursor column position

LOOP055         sta TITLETXT,X          ; Clear character in title line
                dex                     ;
                bpl LOOP055             ;

;*** If title phrase offset = $FF then hide title line *************************
SKIP177         ldx TITLEPHR            ; Load title phrase offset
                inc TITLEPHR            ; Prepare title phrase offset for next word
                bne SKIP178             ; ...skip if it turned 0

                ldx #$0F                ; Remove title line and return
                ldy #$80                ;
                lda #$07                ;
                jmp MODDLST             ;

SKIP178         lda PHRASETAB,X         ; Get phrase token

;*** Display scored class? *****************************************************
                cmp #$FC                ; Skip if not "scored class" token
                bne SKIP179             ;

                ldy SCOREDCLASSIND      ; Get scored class index, is in 0..15
                lda CLASSTAB,Y          ; Load scored class number digit
                ldx L_COLUMNPOS         ; Load cursor position
                sta TITLETXT,X          ; Store class in title line
                lda #60                 ; Title segment lifetime := 60 game loops
                sta TITLELIFE           ;
                rts                     ; Return

;*** Display scored rank? ******************************************************
SKIP179         cmp #$FD                ; Skip if not "scored rank" token
                bne SKIP180             ;

                ldy SCOREDRANKIND       ; Get scored rank index, is in 0..18
                lda RANKTAB,Y           ; Load rank word number

;*** Search word of token in word table ****************************************
SKIP180         sta L_TOKEN             ; Save phrase token
                and #$3F                ; Strip bits B6..7 from phrase token
                sta L_WORD              ; Store word number (bits B5..0)

                lda #<(WORDTAB-1)       ; Point MEMPTR to WORDTAB-1
                sta MEMPTR              ;
                lda #>(WORDTAB-1)       ;
                sta MEMPTR+1            ;

LOOP056         inc MEMPTR              ; Increment MEMPTR
                bne SKIP181             ;
                inc MEMPTR+1            ;

SKIP181         ldy #0                  ;
                lda (MEMPTR),Y          ; Load character of word
                bpl LOOP056             ; Loop until end-of-word marker (bit B7) found
                dec L_WORD              ;
                bne LOOP056             ; Loop until word found

;*** Copy word to title line, add space ****************************************
LOOP057         and #$3F                ; Strip color bits B6..7 from character
                eor #ccs_Col2|$20       ; Merge COLOR2 bits and convert to ATASCII
                ldx L_COLUMNPOS         ; Copy character to title line
                inc L_COLUMNPOS         ; Increment cursor column position
                sta TITLETXT,X          ;
                iny                     ;
                lda (MEMPTR),Y          ; Load next character of word
                bpl LOOP057             ; Next character of word if no end-of-word marker
                inc L_COLUMNPOS         ; Word was copied. Add space after word.

;*** Decide to copy another word, etc. *****************************************
                lda #60                 ; SUMMARY:
                bit L_TOKEN             ; If bits B7..6 of phrase token...
                bpl SKIP182             ; %00 -> Copy next word to title line
                bvc SKIP183             ; %01 -> End-of-phrase, short delay, hide title line
                lda #254                ;        Title segment lifetime := 60 game loops
SKIP182         bvc SKIP177             ; %10 -> End-of-segment.
                ldy #$FF                ;        Title segment lifetime := 60 game loops
                sty TITLEPHR            ; %11 -> End-of-phrase, long delay, hide title line
SKIP183         sta TITLELIFE           ;        Title segment lifetime := 254 game loops
                rts                     ; Return
