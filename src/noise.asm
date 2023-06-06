
;*******************************************************************************
;*                                                                             *
;*                                    NOISE                                    *
;*                                                                             *
;*                          Copy noise sound pattern                           *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Copies a 10-byte noise sound pattern from table NOISEPATTAB ($BF20). The first
; 8 bytes are copied to the noise sound pattern area NOISETORPTIM
; ($DA)..NOISELIFE ($E1), the remaining 2 bytes are copied to audio registers
; AUDCTL ($D208) and AUDF3 ($D204). The noise sound pattern is automatically
; played in subroutine SOUND ($B2AB).
;
; NOTE: The first 8 bytes of each pattern in table NOISEPATTAB ($BF20) are
; copied in reverse order from memory. See subroutine SOUND ($B2AB) for details
; on the noise sound patterns stored in NOISEPATTAB ($BF20).
;
; Playing a SHIELD EXPLOSION or ZYLON EXPLOSION noise sound pattern overrides a
; currently playing PHOTON TORPEDO LAUNCHED noise sound pattern.
;
; Playing a PHOTON TORPEDO LAUNCHED noise sound pattern overrides a currently
; playing PHOTON TORPEDO LAUNCHED noise sound pattern if the latter has < 24
; TICKs to play.
;
; INPUT
;
;   X = Offset into table NOISEPATTAB ($BF20) to index noise sound patterns.
;       Used values are:
;     $00 -> PHOTON TORPEDO LAUNCHED
;     $0A -> SHIELD EXPLOSION (either our starship or a starbase explodes)
;     $14 -> ZYLON EXPLOSION


;======================================
; Copy noise sound pattern
;======================================
NOISE           .proc
                txa                     ; Skip if SHIELD EXPLOSION or ZYLON EXPLOSION playing
                bne _1                  ;

                lda NOISELIFE           ; Return if PHOTON TORPEDO LAUNCHED noise sound pat.
                cmp #24                 ; ...playing for yet more than 24 TICKs
                bcs _XIT                ;

_1              ldy #7                  ; Copy noise sound pattern (in reverse order)
_next1          lda NOISEPATTAB,X       ;
                sta NOISETORPTIM,Y      ;
                inx                     ;
                dey                     ;
                bpl _next1              ;

                lda NOISEPATTAB,X       ; Copy AUDCTL from noise sound pattern table
                ;--sta AUDCTL              ;
                lda NOISEPATTAB+1,X     ; Copy AUDF3 from noise sound pattern table
                ;--sta AUDF3               ;

_XIT            rts
                .endproc
