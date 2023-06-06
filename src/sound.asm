
;*******************************************************************************
;*                                                                             *
;*                                    SOUND                                    *
;*                                                                             *
;*                            Handle sound effects                             *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine handles the sound effects. It is called every vertical blank
; phase, that is, every TICK (1/60 s on an NTSC Atari 8-bit Home Computer
; system, 1/50 s on a PAL Atari 8-bit Home Computer system) from the Vertical
; Blank Interrupt handler VBIHNDLR ($A6D1).
;
; The game uses all of the available 4 audio channels: Audio channels 1, 2, and
; 3 are shared among the Engines sound effects and the "noise sound patterns"
; (explosion and photon torpedo sound effects), while audio channel 4 is used
; for "beeper sound patterns" (status report sound effects). The following
; sections explain the beeper sound patterns and the noise sound patterns:
;
; o   BEEPER SOUND PATTERNS
;
;     There are the following beeper sound patterns:
;
;     (1)  HYPERWARP TRANSIT
;     (2)  RED ALERT
;     (3)  ACKNOWLEDGE
;     (4)  DAMAGE REPORT
;     (5)  MESSAGE FROM STARBASE
;
;     They are encoded in table BEEPPATTAB ($BF3E) in 6-byte long "beeper sound
;     patterns". 
;
;     Another table, BEEPFRQTAB ($BF5C), stores the frequencies for the tones
;     of each beeper sound pattern, terminated by a marker byte ($FF).
;
;     BUG (at $BF5C): The pattern frequencies in table BEEPFRQTAB ($BF5C) at
;     offset $00 are unused. Suggested Fix: Remove from code.
;
;     Whenever the game calls subroutine BEEP ($B3A6), that subroutine sets up a
;     beeper sound pattern for playing by copying 6 bytes from the pattern table
;     BEEPPATTAB ($BF3E) to BEEPFRQIND ($D2)..BEEPFRQSTART ($D7). Subroutine
;     SOUND ($B2AB) detects the copied beeper sound pattern and plays the
;     encoded tones and pauses.
;
;     The relevant variables for playing a beeper sound pattern are the
;     following (see also figures at BEEPPATTAB ($BF3E)):
;
;     BEEPFRQIND    ($D2)   = Running index into table BEEPFRQTAB ($BF5C)
;     BEEPREPEAT    ($D3)   = Number of times that the beeper sound pattern is
;                             repeated - 1
;     BEEPTONELIFE  ($D4)   = Lifetime of tone in TICKs - 1
;     BEEPPAUSELIFE ($D5)   = Lifetime of pause in TICKs - 1 ($FF -> No pause)
;     BEEPPRIORITY  ($D6)   = Beeper sound pattern priority. A playing beeper
;                             sound pattern is stopped if a beeper sound pattern
;                             of higher priority is about to be played. A value
;                             of 0 indicates that no beeper sound pattern is
;                             playing at the moment.
;     BEEPFRQSTART  ($D7)   = Index to first byte of the beeper sound pattern in
;                             table BEEPFRQTAB ($BF5C)
;
;     BEEPLIFE      ($D8)   = Lifetime of the current tone or pause in TICKs
;     BEEPTOGGLE    ($D9)   = Indicates that either a tone (0) or a pause (not
;                             0) is currently playing.
;
; o   NOISE SOUND PATTERNS
;
;     There are the following noise sound patterns:
;
;     (1)  PHOTON TORPEDO LAUNCHED
;     (2)  SHIELD EXPLOSION
;     (3)  ZYLON EXPLOSION
;
;     They are encoded in table NOISEPATTAB ($BF20) in 10-byte long "noise sound
;     patterns". 
;
;     Whenever the game calls subroutine NOISE ($AEA8), that subroutine sets up
;     a noise sound pattern for being played by copying 10 bytes from the
;     pattern table NOISEPATTAB ($BF20) to NOISETORPTIM ($DA)..NOISELIFE ($E1)
;     and hardware sound registers AUDCTL ($D208) and AUDF3 ($D204).
;
;     The relevant variables for playing a noise sound pattern are the
;     following:
;
;     NOISETORPTIM  ($DA)   = Delay timer for PHOTON TORPEDO LAUNCHED noise
;                             sound pattern
;     NOISEEXPLTIM  ($DB)   = Delay timer for SHIELD EXPLOSION and ZYLON
;                             EXPLOSION noise sound patterns
;     NOISEAUDC2    ($DC)   = Audio channel 1/2 control shadow register
;     NOISEAUDC3    ($DD)   = Audio channel 3   control shadow register
;     NOISEAUDF1    ($DE)   = Audio channel 1 frequency shadow register
;     NOISEAUDF2    ($DF)   = Audio channel 2 frequency shadow register
;     NOISEFRQINC   ($E0)   = Audio channel 1/2 frequency increment
;     NOISELIFE     ($E1)   = Noise sound pattern lifetime
;
;     AUDCTL        ($D208) = POKEY: Audio control
;     AUDF3         ($D204) = POKEY: Audio channel 3 frequency audio register
;
;     There are two more variables that trigger noise effects. They are not part
;     of the noise sound pattern table:
;
;     NOISEZYLONTIM ($E2)   = Delay timer to trigger the ZYLON EXPLOSION noise
;                             sound pattern. It is set in subroutine Collision
;                             ($AF3D) when the impact of one of our starship's
;                             photon torpedoes with a target is imminent. The
;                             timer is decremented every TICK. When it reaches a
;                             value of 0 the ZYLON EXPLOSION noise sound pattern
;                             is played in subroutine SOUND ($B2AB). 
;     NOISEHITLIFE ($E3)    = Lifetime of the STARSHIP EXPLOSION noise when our
;                             starship was destroyed by a Zylon photon torpedo.
;                             It is set in GAMELOOP ($A1F3) to a value of 64
;                             TICKs. When it reaches a value of 0 the STARSHIP
;                             EXPLOSION noise is played in subroutine SOUND
;                             ($B2AB).
;
; SUBROUTINE DETAILS
;
; This subroutine executes the following steps:
;
; (1)  Play beeper sound pattern
;
;      The playing of a beeper sound pattern is started, continued, or stopped.
;
; (2)  Play ZYLON EXPLOSION noise sound pattern
;
;      If the explosion of a target space object is imminent (subroutine
;      Collision ($AF3D) has set NOISEZYLONTIM ($E2) to the number of game loop
;      iterations that will pass until our starship's photon torpedo will hit
;      the target), the timer NOISEZYLONTIM ($E2) is decremented every TICK. If
;      it reaches a value of 0, then the noise sound pattern ZYLON EXPLOSION is
;      played.
;
; (3)  Play starship's Engines sound
;
;      If the Engines are louder than the current noise sound pattern then the
;      noise sound pattern is terminated and the values for the audio channels
;      1..3 are updated: 
;
;      The velocity of our starship determines the pitch and the volume of the
;      Engines: the higher the velocity, the higher the pitch and the volume of
;      the Engines. The incremented value of VELOCITYLO ($70) is used as a "base
;      value" %abcdefgh.
;
;      Audio channels 1 and 2 are combined to a 16-bit audio channel 1/2,
;      clocked at 1.79 MHz. The inverted bits (represented by an overscore)
;      B7..0 of the base value form bits B12..5 of the 16-bit frequency value of
;      audio channel 1/2. Bits B7..4 of the base value form bits B3..0 of the
;      volume of audio channel 1/2, with noise distortion bit B7 set:
;                               ________
;      AUDF1/2 ($D202..3) := %000abcdefgh00000
;      AUDC2   ($D203)    := %1000abcd
;
;      Audio channel 3 is also clocked at 1.79 MHz. The inverted bits B7..0 of
;      the base value form bits B7..0 of the frequency value of audio channel 3.
;      Bits B6..4 of the base value form bits B3..0 of the volume of audio
;      channel 3, with noise distortion bit B7 set:
;                            ________
;      AUDF3   ($D204)    := %abcdefgh
;      AUDC3   ($D205)    := %10000bcd
;
;      Code execution returns at this point.
;
; (4)  Play ZYLON EXPLOSION or SHIELD EXPLOSION noise sound pattern
;
;      If the ZYLON EXPLOSION or SHIELD EXPLOSION noise sound pattern was set
;      up, the explosion noise timer NOISEEXPLTIM ($DB) is decremented every
;      TICK. It starts either with a value of 4 TICKs with a ZYLON EXPLOSION
;      noise sound pattern or with a value of 8 TICKs with a SHIELD EXPLOSION
;      noise sound pattern, set up in subroutine NOISE ($AEA8). If it reaches a
;      value of 0, then the shadow control register of audio channel 1/2
;      switches to "noise distortion" at maximum volume.
;
; (5)  Play PHOTON TORPEDO LAUNCHED noise sound pattern
;
;      If the PHOTON TORPEDO LAUNCHED noise sound pattern was set up, the photon
;      torpedo noise timer NOISETORPTIM ($DA) is decremented every TICK. It
;      starts with a value of 8 TICKs, set in subroutine TRIGGER ($AE29). The
;      noise distortion and volume for the shadow control register of audio
;      channel 3 is picked from table NOISETORPVOLTAB ($BFEB), the noise
;      frequency for audio channel 3 is picked from table NOISETORPFRQTAB
;      ($BFF3). If the photon torpedo noise timer reaches a value of 0, then the
;      shadow control registers of audio channel 1/2 switch to "tone distortion"
;      at maximum volume and a frequency of $0202.
;
;      NOTE: Using a real-time volume envelope stored in table NOISETORPVOLTAB
;      ($BFEB) for a launched photon torpedo results in producing the
;      distinctive "whooshing" photon torpedo sound.
;
; (6)  Play STARSHIP EXPLOSION noise
;
;      If our starship was hit by a Zylon photon torpedo then NOISEHITLIFE ($E3)
;      was set to 64 TICKs in routine GAMELOOP ($A1F3). While this value is
;      decremented every TICK, a random frequency value is stored to audio
;      channel 3 and the distortion bit of the shadow control register of audio
;      channel 3 is randomly toggled.
;
; (7)  Increase audio channels 1/2 frequency
;
;      The 16-bit frequency value of audio channels 1/2 (both shadow registers
;      and audio registers) is increased every TICK by an increment picked from
;      the currently playing noise sound pattern.
;
; (8)  Mute audio channels gradually
;
;      Toward the end of a noise sound pattern's lifetime all audio channels
;      gradually mute their volume every other TICK until completely silent. 


;======================================
; Play beeper sound pattern
;======================================
SOUND           .proc
                lda BEEPPRIORITY        ; Skip if beeper sound pattern not in use
                beq _2                  ;

                dec BEEPLIFE            ; Decrement beeper lifetime
                bpl _2                  ; Skip if beeper lifetime still counting down

                lda BEEPTOGGLE          ; Load tone/pause toggle
                beq _next1              ; Skip if a tone is playing or is to be played

                lda BEEPPAUSELIFE       ; Load pause lifetime
                bmi _next1              ; Skip if duration = $FF (no pause)
                sta BEEPLIFE            ; Store pause lifetime as beeper lifetime
                ldy #0                  ; Prep AUDC4 (zero volume)
                beq _1                  ; Skip unconditionally

_next1          lda BEEPTONELIFE        ; Load tone lifetime
                sta BEEPLIFE            ; Store tone lifetime as beeper lifetime
                ldx BEEPFRQIND          ; Load frequency index
                inc BEEPFRQIND          ; Increment frequency index
                lda BEEPFRQTAB,X        ; Store tone frequency from frequency table in AUDF4
                sta SID2_FREQ1          ;
                ldy #$A8                ; Prep AUDC4 (tone distortion + medium volume)
                cmp #$FF                ; Skip if frequency not $FF (there are more tones)
                bne _1                  ;

                lda BEEPFRQSTART        ; Rewind pattern frequency pointer
                sta BEEPFRQIND          ;
                dec BEEPREPEAT          ; Decrement sequence counter
                bpl _next1              ; Keep playing until sequence counter < 0

                ldy #0                  ; Prep AUDC4 with zero volume
                sty BEEPPRIORITY        ; Stop playing beeper sound pattern

_1              ;--sty SID2_CTRL1               ; Store in AUDC4
                sty BEEPTOGGLE          ; Store in BEEPTOGGLE

;*** Play ZYLON EXPLOSION noise sound pattern **********************************
_2              lda NOISEZYLONTIM       ; Skip if ZYLON EXPLOSION timer not in use
                beq _3                  ;

                dec NOISEZYLONTIM       ; Decrement ZYLON EXPLOSION timer
                bne _3                  ; Skip if ZYLON EXPLOSION timer still counting down

                ldx #$14                ; Play noise sound pattern ZYLON EXPLOSION
                jsr NOISE               ;

;*** Play our starship's Engines sound *****************************************
_3              ldx VELOCITYLO          ; Skip if Engines softer than noise sound pattern
                txa                     ;
                lsr                     ;
                lsr                     ;
                lsr                     ;
                lsr                     ;
                lsr                     ;
                cmp NOISELIFE           ;
                bcc _4                  ;

                lda #0                  ; Terminate noise sound pattern
                sta NOISELIFE           ;

                inx                     ;
                txa                     ; A := %abcdefgh = VELOCITYLO + 1
                eor #$FF                ;           ________
                sta SID1_FREQ3          ; AUDF3 := %abcdefgh

                tax                     ;                ________
                asl                     ; AUDF2/1 := %000abcdefgh00000
                asl                     ;
                asl                     ;
                asl                     ;
                asl                     ;
                sta SID1_FREQ1          ;

                txa                     ;
                lsr                     ;
                lsr                     ;
                lsr                     ;
                sta SID1_FREQ2          ;

                lsr                     ; AUDC2 := %1000abcd
                eor #$8F                ; (noise distortion + B7..B4 bits for volume)
                ;--sta SID1_CTRL2               ;

                and #$87                ; AUDC3 := %10000bcd
                ;--sta SID1_CTRL3               ; (noise distortion + B6..B4 bits for volume)

                lda #$70                ; Clock audio channel 1 and 3 @ 1.79 MHz and...
                ;--sta AUDCTL              ; ...combine audio channel 1/2 to 16-bit channel

                rts

;*** Play ZYLON EXPLOSION or SHIELD EXPLOSION noise ****************************
_4              lda NOISEEXPLTIM        ; Skip if explosion noise timer not in use
                beq _5                  ;

                dec NOISEEXPLTIM        ; Decrement explosion noise timer (4 or 8 TICKs long)
                bne _5                  ; Skip if explosion noise timer still counting down

                lda #$8F                ; Shadow register AUDC2 := (noise dist. + max volume)
                sta NOISEAUDC2          ;

;*** Play PHOTON TORPEDO LAUNCHED noise sound **********************************
_5              ldx NOISETORPTIM        ; Skip if photon torpedo noise timer not in use
                beq _7                  ;

                dec NOISETORPTIM        ; Decrement photon torpedo noise timer (8 TICKs long)
                bne _6                  ; Skip if torpedo noise timer still counting down

                lda #$AF                ; Shadow register AUDC2 := (tone dist. + max volume)
                sta NOISEAUDC2          ;
                lda #$02                ; Set frequency $0202 to AUDF1/2's shadow...
                sta NOISEAUDF1          ; ...registers
                sta NOISEAUDF2          ;

_6              lda NOISETORPVOLTAB-1,X ; Pick torpedo noise + volume shape (X in 1..8)...
                sta NOISEAUDC3          ; ...and store it in AUDC3's shadow register
                lda NOISETORPFRQTAB-1,X ; Pick photon torpedo noise frequency (X in 1..8)...
                sta SID1_FREQ3          ; ...and store it in AUDF3
                ;--sta STIMER              ; Reset POKEY audio timers

;*** Play STARSHIP EXPLOSION noise when our starship is hit ********************
_7              lda NOISEHITLIFE        ; Skip if STARSHIP EXPLOSION noise not in use
                beq _8                  ;

                dec NOISEHITLIFE        ; Decrement STARSHIP EXPLOSION noise lifetime
                .randomByte             ; Set random frequency to AUDF3
                sta SID1_FREQ3          ;
                and #$20                ; Toggle noise/tone dist. of AUDC3's shadow register
                eor NOISEAUDC3          ; ...randomly
                sta NOISEAUDC3          ;

;*** Increase 16-bit frequency of audio channels 1/2 (shadow registers also) ***
_8              clc                     ; Increase 16-bit frequency value of AUDF1/2...
                lda NOISEAUDF1          ; ...and its shadow register by...
                adc NOISEFRQINC         ; ...noise sound pattern frequency increment
                sta NOISEAUDF1          ; AUDF1/2 := NOISEAUDF1/2 := ...
                sta SID1_FREQ1          ; ...NOISEAUDF1/2 + NOISEFRQINC
                lda NOISEAUDF2          ;
                adc #0                  ;
                sta NOISEAUDF2          ;
                sta SID1_FREQ2          ;

;*** Gradually mute audio channels while noise sound pattern expires ***********
                ldx NOISEAUDC2          ; Prep AUDC2's shadow register value
                ldy NOISEAUDC3          ; Prep AUDC3's shadow register value

                lda COUNT8              ; Decrement volumes every other TICK
                lsr                     ;
                bcc _10                 ;

                lda NOISELIFE           ; Skip if noise sound pattern not in use
                beq _10                 ;

                dec NOISELIFE           ; Decrement noise sound pattern lifetime

                cmp #17                 ; Mute noise sound pattern only in...
                bcs _10                 ; ...the last 16 TICKs of its lifetime

                txa                     ; Decrement volume of AUDC2's shadow register
                and #$0F                ;
                beq _9                  ;

                dex                     ;
                stx NOISEAUDC2          ;

_9              tya                     ; Decrement volume of AUDC3's shadow register
                and #$0F                ;
                beq _10                 ;
                dey                     ;
                sty NOISEAUDC3          ;

_10             ;--stx SID1_CTRL2               ; Store shadow register values to audio registers
                ;--sty SID1_CTRL3               ;

                rts
                .endproc


;*******************************************************************************
;*                                                                             *
;*                                    BEEP                                     *
;*                                                                             *
;*                          Copy beeper sound pattern                          *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Copies a 6-byte beeper sound pattern from beeper sound pattern table
; BEEPPATTAB ($BF3E) to BEEPFRQIND ($D2)..BEEPFRQSTART ($D7), provided that no
; beeper sound pattern with higher priority is currently playing. The beeper
; sound pattern will then be automatically played in subroutine SOUND ($B2AB).
; See subroutine SOUND ($B2AB) for more information on beeper sound patterns. 
;
; NOTE: The bytes from table BEEPPATTAB ($BF3E) are copied in reverse order.
;
; INPUT
;
;   X = Offset to beeper sound pattern in table BEEPPATTAB ($BF3E). Used values
;       are:
;     $00 -> HYPERWARP TRANSIT
;     $06 -> RED ALERT
;     $0C -> ACKNOWLEDGE
;     $12 -> DAMAGE REPORT
;     $18 -> MESSAGE FROM STARBASE


;======================================
;
;======================================
BEEP            .proc
                lda BEEPPATTAB,X        ; Return if beeper sound pattern of...
                cmp BEEPPRIORITY        ; ...higher priority is playing
                bcc _XIT                ;

                ldy #5                  ; Copy 6-byte beeper sound pattern (in reverse order)
_next1          lda BEEPPATTAB,X        ;
                sta BEEPFRQIND,Y        ;
                inx                     ;
                dey                     ;
                bpl _next1              ;

_XIT            rts
                .endproc
