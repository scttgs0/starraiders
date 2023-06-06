
;*******************************************************************************
;*                                                                             *
;*                              DrawGalacticChart                              *
;*                                                                             *
;*                             Draw Galactic Chart                             *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Draws the content of the Galactic Chart memory map in GCMEMMAP ($08C9) to the
; Galactic Chart PLAYFIELD memory at GCPFMEM ($0D35). 
;
; NOTE: CPU register X indexes the Galactic Chart memory map GCMEMMAP ($08C9)
; (16 x 8 bytes). CPU register Y indexes the Galactic Chart PLAYFIELD memory
; GCPFMEM ($0D35) (20 x 9 bytes).
;
; NOTE: Sectors with 1 or 2 Zylon ships display the same symbol in the Galactic
; Chart.

L_GCMEMMAPIND   = $6A                   ; Saves Galactic Chart memory map index


;======================================
; Draw Galactic Chart
;======================================
DrawGalacticChart .proc
                ldy #0                  ; Clear Galactic Chart PLAYFIELD memory index
                sty L_GCMEMMAPIND       ; Clear Galactic Chart memory map index

_next1          ldx L_GCMEMMAPIND       ; Load sector of Galactic Chart memory map
                lda GCMEMMAP,X          ;
                bpl _1                  ; Skip if not a starbase sector

                lda #5                  ; Prep sector character index for starbase
_1              tax                     ; Load sector character index
                lda SECTORCHARTAB,X     ; Load custom character set code from table...
                sta GCPFMEM+22,Y        ; ...and store it in Galactic Chart PLAYFIELD memory
                iny                     ; Increment Galactic Chart PLAYFIELD memory index
                inc L_GCMEMMAPIND       ; Increment Galactic Chart memory map index
                lda L_GCMEMMAPIND       ;
                and #$0F                ;
                bne _next1              ; Next sector column until right border reached

                lda #CCS_BORDERW        ; Draw right border
                sta GCPFMEM+22,Y        ;

                iny                     ; Adjust Galactic Chart PLAYFIELD memory index
                iny                     ;
                iny                     ;
                iny                     ;
                cpy #$A0                ;
                bcc _next1              ; Next sector until bottom-right sector reached

                rts
                .endproc
