# Star Raiders Documentation

I wrote this document out of my own curiosity. When STAR RAIDERS was released in 1979 it became the killer app for the Atari 8-bit Home Computer System.
Since then I have always been wondering what made it tick and how its (at that time) spectacular 3D graphics worked, especially the rotating star field.
Impressed by "The Atari BASIC Source Book" I decided to reverse engineer the STAR RAIDERS 8KB ROM cartridge to recreate a fully documented assembly language source code file. I had no access to the original source code, so the only way to succeed was a combination of educated guesses, trial-and-error, and patience. Eventually, I made it.

Essential in preparing this document were three programs I wrote:

(1) A 6502-cross-assembler based on the syntax of the MAC/65 assembler for the
     Atari 8-bit Home Computer System to create the binary file that I verified
     against the binary of the original ROM cartridge.

(2) A text formatter to layout the source code file with its copious comment
     sections. This was a big time saver, because as the documentation grew the
     source code had to be reformatted over and over again.

(3) A symbol checker to verify that the ubiquitous symbol-value pairs in the documentation match the corresponding symbol values produced by the assembler.

This assembly language source code file is compatible with the MAC/65 assembler for the Atari 8-bit Home Computer System. I was able to assemble it on an emulated Atari running MAC/65, producing the identical binary of the ROM cartridge. 

Your feedback is welcome! Send feedback to lo.wiest(at)web.de.

Enjoy! -- Lorenz

## N O T A T I O N

### BITS AND BYTES

o   A "byte" consists of 8 bits. They are numbered B7..0. Bit B0 is the least-significant bit.

o   A "word" consists of 16 bits. They are numbered B15..B0. Bit B0 is the least significant bit. A word is stored in low-order then high-order byte order.

o   The high-order byte ("high byte") of a word consists of bits B15..8 of the word.

o   The low-order byte ("low byte") of a word consists of bits B7..0 of the word.

### NUMBERS

o   The dollar sign ($) prefixes hexadecimal numbers.

     Example: $101 is the decimal number 257.

o   The percent sign (%) prefixes binary numbers.

     Example: %101 is the decimal number 5.

o   The asterisk (*) is a wildcard character for a single hexadecimal or binary digit.

     Example: $0*00 is a placeholder for the numbers $0000, $0100, ..., $0F00.

o   The lowercase R (r) is a wildcard character for a single random hexadecimal or binary digit. The random digit r is chosen by a random number generator.

     Example: %00r0 is a placeholder for the numbers %0000 or %0010.

### OPERATORS

o   The exclamation mark (!) is the binary OR operator.

     Example: $01!$02 is $03.

o   The less-than sign (<) indicates bits B7..0 of a word.

     Example: <$1234 is $34.

o   The greater-than sign (>) indicates bits B15..8 of a word.

     Example: >$1234 is $12.

o   A pair of brackets ([]) groups mathematical expressions.

     Example: [3-1]*4 is 8.

### ASSEMBLY LANGUAGE

o   The uppercase A (A) indicates the accumulator register of the 6502 CPU.

o   The uppercase X (X) indicates the X register of the 6502 CPU.

o   The uppercase Y (Y) indicates the Y register of the 6502 CPU.

o   The prefix uppercase L and dot (L_) indicates a local variable, a memory
     location used temporarily in a subroutine.

### PSEUDO-FUNCTIONS

o   The function ABS(<num>) returns the absolute value of <num>.
     Example: ABS(3) returns 3.
     Example: ABS(-3) returns 3.

o   The function RND(<num1>..<num2>) returns a random integer in
     <num1>..<num2>.
     Example: RND(3..5) returns a random number out of 3, 4, or 5.

o   The function MAX(<num1>,<num2>) returns the larger number of <num1> and
     <num2>.
     Example: MAX(2,4) returns 4.

### VECTORS

o   The lowercase X (x) indicates the x-axis of the 3D coordinate system.

o   The lowercase Y (y) indicates the y-axis of the 3D coordinate system.

o   The lowercase Z (z) indicates the z-axis of the 3D coordinate system.

o   Components of a position vector (called "coordinates") have the arbitrary
     unit <KM> ("kilometers").

o   Components of a velocity vector have the arbitrary unit <KM/H>
     ("kilometers per hour").

o   A positive component of a position vector (coordinate) in hexadecimal
     notation is written in the form +$<hexNum> <KM>. <hexNum> is an unsigned
     integer value.
     Example: The starbase is +$1000 (or 4096) <KM> ahead of our starship.

o   A negative component of a position vector (coordinate) in hexadecimal
     notation is written in the form -($<hexNum>) <KM>. <hexNum> is an unsigned
     integer value. To calculate the actual bit pattern of this coordinate
     value compute the two's-complement of <hexNum>. See also "ON POSITION
     VECTORS".
     Example: The starbase is -($1000) (or -4096) <KM> behind our starship.

o   An absolute component of a position vector (coordinate) in hexadecimal
     notation is written in the form $<hexNum> <KM>. <hexNum> is an unsigned
     integer value.
     Example: The Zylon fighter fires when it is closer than $1000 (or 4096) <KM>. 

### DISPLAY LIST

o   The following notation is used for Display List instructions:

     BLK<n>           = Display <n> blank video lines (<n> in 1..8)
     GR1              = Display one GRAPHICS 1 row of 20 text characters
     GR2              = Display one GRAPHICS 2 row of 20 text characters
     GR7              = Display one GRAPHICS 7 row of 160 pixels
     DLI              = Trigger a Display List Interrupt
     ... @ <addr>     = Point to screen memory at address <addr>
     JMP @ <addr>     = Jump to next Display List instruction at address <addr>
     WAITJMP @ <addr> = Wait for vertical blank phase, then jump to next
                        Display List instruction at address <addr>

### MISCELLANEOUS

o   Probabilities are written in the form <percentage>% (<number of values out
     of the possible values>:<number of possible values>).
     Example: The probability to throw the number 3 with a die is 16% (1:6).

o   A "game loop iteration" (or "game loop") is a single execution of the game
     loop, the main program of the game.

o   A "TICK" is the time span it takes to update the TV screen (1/60 s on an
     NTSC TV system, 1/50 s on a PAL TV system).

o   A pair of braces ({}) encloses color names.
     Example: {BLACK}

o   A pair of parentheses enclosing a question mark ((?)) indicates code that
     is not well understood.

o   A pair of parentheses enclosing an exclamation mark ((!)) indicates a
     potential bug.

## O V E R V I E W

### ON POSITION VECTORS

 The game uses a 3D coordinate system with the position of our starship at the
 center of the coordinate system and the following coordinate axes:

o   The x-axis points to the right.
o   The y-axis points up.
o   The z-axis points in flight direction.

 By the way, this is called a "left-handed" coordinate system.

 The locations of all space objects (Zylon ships, meteors, photon torpedoes,
 starbase, transfer vessel, Hyperwarp Target Marker, stars, and explosion
 fragments) are described by a "position vector".

 A "position vector" is composed of an x, y, and z component. The values of the
 position vector components are called the x, y, and z "coordinates". They have
 the arbitrary unit <KM>.

 Each coordinate is a signed 17-bit integer number, which fits into 3 bytes:

     Sign     Mantissa
          B16 B15...B8 B7....B0
            | |      | |      |
     0000000* ******** ********

o   B16 contains the sign bit. Used values are:
       1 -> Positive sign
       0 -> Negative sign
o   B15..0 contain the coordinate value (or "mantissa") as a two's-complement
     integer number.

 The range of a position vector component is -65536..+65535 <KM>.

 Examples:

     00000001 11111111 11111111 = +65535 <KM>
     00000001 00010000 00000000 =  +4096 <KM>
     00000001 00001111 11111111 =  +4095 <KM>
     00000001 00000001 00000000 =   +256 <KM>
     00000001 00000000 11111111 =   +255 <KM>
     00000001 00000000 00010000 =    +16 <KM>
     00000001 00000000 00001111 =    +15 <KM>
     00000001 00000000 00000001 =     +1 <KM>
     00000001 00000000 00000000 =     +0 <KM>

     00000000 11111111 11111111 =     -1 <KM>
     00000000 11111111 11111110 =     -2 <KM>
     00000000 11111111 11110001 =    -15 <KM>
     00000000 11111111 11110000 =    -16 <KM>
     00000000 11111111 00000001 =   -255 <KM>
     00000000 11111111 00000000 =   -256 <KM>
     00000000 11110000 00000001 =  -4095 <KM>
     00000000 11110000 00000000 =  -4096 <KM>
     00000000 00000000 00000000 = -65536 <KM>

 The position vector for each space object is stored in 9 tables:

o   XPOSSIGN ($09DE..$0A0E), XPOSHI ($0A71..$0AA1), and XPOSLO ($0B04..$0B34)
o   YPOSSIGN ($0A0F..$0A3F), YPOSHI ($0AA2..$0AD2), and YPOSLO ($0B35..$0B65)
o   ZPOSSIGN ($09AD..$09DD), ZPOSHI ($0A40..$0A70), and ZPOSLO ($0AD3..$0B03)

 There are up to 49 space objects used in the game simultaneously, thus each
 table is 49 bytes long.

o   Position vectors 0..4 belong to space objects represented by PLAYERs
     (Zylon ships, meteors, photon torpedoes, starbase, transfer vessel, and
     Hyperwarp Target Marker).
o   Position vectors 5..48 belong to space objects represented by PLAYFIELD
     pixels. Position vectors 5..16 (stars, explosion fragments) are used for
     stars, position vectors 17..48 are used for explosion fragments and star
     trails.

 INFO: The x and y coordinates of space objects are converted and displayed by
 the THETA and PHI readouts of the Control Panel Display in "gradons". The
 z-coordinate is converted and displayed by the RANGE readout in "centrons".
 The conversion takes place in subroutine SHOWDIGITS ($B8CD) where the high
 byte of a coordinate (with values $00..$FF) is transformed with lookup table
 MAPTOBCD99 ($0EE9) into a BCD value of 00..99 in "gradons" or "centrons".

### ON VELOCITY VECTORS

 The velocities of all space objects are described by a "velocity vector". The
 velocity vector is relative to our starship.

 A "velocity vector" is composed of an x, y, and z component. The values of the
 velocity vector components are called the x, y, and z "velocities". They have
 the arbitrary unit <KM/H>.

 Each velocity vector component is an 8-bit integer number, which fits into 1
 byte:

     B7 Sign
     |
     |B6...B0 Mantissa
     ||     |
     ********

o   B7 contains the sign bit. Used values are:
     0 -> Positive sign, movement along the positive coordinate axis
          (x-velocity: right, y-velocity: up, z-velocity: in flight direction)
     1 -> Negative sign, movement along the negative coordinate axis
          (x-velocity: left, y-velocity: down, z-velocity: in reverse flight
          direction)
o   B6..B0 contain the velocity value (or "mantissa"). It is an unsigned
     number.

 The range of a velocity vector component is -127..+127 <KM/H>.

 Examples:

     01111111 = +127 <KM/H>
     00010000 =  +16 <KM/H>
     00001111 =  +15 <KM/H>
     00000001 =   +1 <KM/H>
     00000000 =   +0 <KM/H>

     10000000 =   -0 <KM/H>
     10000001 =   -1 <KM/H>
     10001111 =  +15 <KM/H>
     10010000 =  +16 <KM/H>
     11111111 = -127 <KM/H>

 The velocity vector for each space object stored in 3 tables:

o   XVEL ($0B97..$0BC7)
o   YVEL ($0BC8..$0BF8)
o   ZVEL ($0B66..$0B96)

 There are up to 49 space objects used in the game simultaneously, thus each
 table is 49 bytes long.

o   Velocity vectors 0..4 belong to space objects represented by PLAYERs
     (Zylon ships, meteors, photon torpedoes, starbase, transfer vessel, and
     Hyperwarp Target Marker).
o   Velocity vectors 5..48 belong to space objects represented by PLAYFIELD
     pixels. Velocity vectors 5..16 are used for stars, velocity vectors 17..48
     are used for explosion fragments and star trails.

 INFO: The velocity of our starship is converted and displayed by the VELOCITY
 readout of the Control Panel Display in "metrons per second" units. The
 conversion takes place in subroutine SHOWDIGITS ($B8CD) where our starship's
 velocity VELOCITYL ($70) (with values $00..$FF) is transformed with lookup
 table MAPTOBCD99 ($0EE9) into a BCD value of 00..99 in "metrons per second".  

## M E M O R Y   M A P

 The following variables are not changed by a SYSTEM RESET:

 $62      MISSIONLEVEL

          Mission level. Used values are:
            $00 -> NOVICE mission
            $01 -> PILOT mission
            $02 -> WARRIOR mission
            $03 -> COMMANDER mission

 $63      FKEYCODE

          Function key code. Used values are:
            $00 -> No function key pressed
            $01 -> START function key pressed
            $02 -> SELECT function key pressed

 $64      ISDEMOMODE

          Indicates whether the game is in game or in demo mode. Used values
          are:
            $00 -> Game mode
            $FF -> Demo mode

 $65      NEWTITLEPHR

          New title phrase offset for the text in the title line. The new title
          phrase is not immediately displayed in the title line but only after
          the display time of the currently displayed title phrase has expired.
          Thus, setting a value to NEWTITLEPHR ($65) "enqueues" the display of
          new title phrase. Used values are:
            $00..$7B -> Title phrase offset into PHRASETAB ($BBAA)
            $FF      -> Hide title line

          See also TITLEPHR ($D1).

 $66      IDLECNTHI

          Idle counter (high byte). Forms a 16-bit counter together with
          IDLECNTLO ($77), which is incremented during the execution of the
          Vertical Blank Interrupt handler VBIHNDLR ($A6D1). IDLECNTHI ($66) is
          reset to 0 when the joystick trigger or a keyboard key has been
          pressed, or to 1..3 when a function key has been pressed. When
          IDLECNTHI ($66) reaches a value of 128 (after about 10 min idle time)
          the game enters demo mode.

 The following variables are set to 0 after a SYSTEM RESET:

 $67      ISVBISYNC

          Indicates whether the Vertical Blank Interrupt handler VBIHNDLR
          ($A6D1) is executed. Used to synchronize the execution of a new game
          loop iteration in GAMELOOP ($A1F3) with the vertical blank phase.
          Used values are:
            $00 -> Halt execution at start of game loop and wait for VBI
            $FF -> Continue execution of game loop

 $68..$69 MEMPTR

          A 16-bit memory pointer.

          Also used as a local variable.

 $6A..$6B DIVIDEND

          A 16-bit dividend value passed in GAMELOOP ($A1F3) to subroutine
          PROJECTION ($AA21) to calculate a division.

          Also used as a local variable.

 $6C      Used as a local variable.

 $6D      JOYSTICKDELTA

          Used to pass joystick directions from GAMELOOP ($A1F3) to subroutine
          ROTATE ($B69B). Used values are:
            $01 -> Joystick pressed right or up
            $00 -> Joystick centered
            $FF -> Joystick pressed left or down

          Also used as a local variable.

 $6E      Used as a local variable.

 $70      VELOCITYLO

          Our starship's current velocity (low byte) in <KM/H>. Forms a 16-bit
          value together with VELOCITYHI ($C1). In subroutine UPDPANEL ($B804),
          VELOCITYLO ($70) is mapped to a BCD-value in 00..99 and displayed by
          the VELOCITY readout of the Control Panel Display. See also
          NEWVELOCITY ($71).

 $71      NEWVELOCITY

          Our starship's new velocity (low byte) in <KM/H>. It is set by
          pressing one of the speed keys '0'..'9'. A pressed speed key is
          mapped to the new velocity value with VELOCITYTAB ($BAB4).

 $72      COUNT8

          Wrap-around counter. Counts from 0..7, then starts over at 0. It is
          incremented every game loop iteration. It is used to change the
          brightness of stars and explosion fragments more randomly in GAMELOOP
          ($A1F3) and to slow down the movement of the hyperwarp markers of the
          Galactic Chart in subroutine SELECTWARP ($B162).

 $73      EXPLLIFE

          Explosion lifetime. It is decremented every game loop iteration. Used
          values are:
              $00 -> Explosion is over
            < $18 -> Number of explosion fragment space objects is decremented
            < $70 -> HITBADNESS ($8A) is reset
              $80 -> Initial value at start of explosion

 $74      CLOCKTIM

          Star date clock delay timer. Counts down from 40 to 0. It is
          decremented every game loop iteration. When the timer falls below 0
          the last digit of the star date of the Galactic Chart Panel Display
          is increased and the timer is reset to a value of 40. 

 $75      DOCKSTATE

          State of docking operation. Used values are:
            $00 -> NOT DOCKED
            $01 -> TRANSFER COMPLETE
            $81 -> RETURN TRANSFER VESSEL
            $FF -> ORBIT ESTABLISHED

 $76      COUNT256

          Wrap-around counter. Counts from 0..255, then starts over at 0. It is
          incremented every game loop iteration. It is used to make the
          starbase pulsate in brightness in GAMELOOP ($A1F3) and to decide on
          the creation of a meteor in subroutine MANEUVER ($AA79).

 $77      IDLECNTLO

          Idle counter (low byte). Forms a 16-bit counter together with
          IDLECNTHI ($66), which is incremented during the execution of the
          Vertical Blank Interrupt handler VBIHNDLR ($A6D1).

          NOTE: This variable is never properly initialized except at initial
          cartridge startup (cold start).

 $78      ZYLONUNITTIM

          Zylon unit movement timer. This delay timer triggers movement of
          Zylon units on the Galactic Chart. At the start of the game, the
          timer is initialized to a value of 100. It is decremented every 40
          game loop iterations. When the timer falls below 0 the Zylon units
          move on the Galactic Chart and the timer value is reset to 49. If a
          starbase is surrounded the timer is reset to 99 to buy you some extra
          time to destroy one of the surrounding Zylon units.

 $79      MAXSPCOBJIND

          Maximum index of used space objects in the current game loop
          iteration. Frequently used values are:
            $10 -> During regular cruise (5 PLAYER space objects + 12 PLAYFIELD
                   space objects (stars), counted $00..$10)
            $30 -> During explosion or hyperwarp (5 PLAYER space objects + 12
                   PLAYFIELD space objects (stars) + 32 PLAYFIELD space objects
                   (explosion fragments or stars of star trails), counted
                   $00..$30)

 $7A      OLDMAXSPCOBJIND

          Maximum index of used space objects in the previous game loop
          iteration. Frequently used values are:
            $10 -> During regular cruise (5 PLAYER space objects + 12 PLAYFIELD
                   space objects (stars), counted $00..$10)
            $30 -> During explosion or hyperwarp (5 PLAYER space objects + 12
                   PLAYFIELD space objects (stars) + 32 PLAYFIELD space objects
                   (explosion fragments or stars of star trails), counted
                   $00..$30)

 $7B      ISSTARBASESECT

          Indicates whether a starbase is in this sector. Used values are:
            $00 -> Sector contains no starbase
            $FF -> Sector contains starbase

 $7C      ISTRACKCOMPON

          Indicates whether the Tracking Computer is on or off. Used values
          are:
            $00 -> Tracking Computer is off
            $FF -> Tracking Computer is on

 $7D      DRAINSHIELDS

          Energy drain rate of the Shields per game loop iteration in energy
          subunits. See also subroutine UPDPANEL ($B804). Used values are:
            $00 -> Shields are off
            $08 -> Shields are on

 $7E      DRAINATTCOMP

          Energy drain rate of the Attack Computer per game loop iteration in
          energy subunits. See also subroutine UPDPANEL ($B804). Used values
          are:
            $00 -> Attack Computer off
            $02 -> Attack Computer on

 $7F      ENERGYCNT

          Running counter of consumed energy subunits (256 energy subunits = 1
          energy unit displayed by the 4-digit ENERGY readout of the Control
          Panel Display). Forms an invisible fractional or "decimals" part of
          the 4-digit ENERGY readout of the Control Panel Display. See also
          subroutine UPDPANEL ($B804).

 $80      DRAINENGINES

          Energy drain rate of our starship's Engines per game loop iteration
          in energy subunits (256 energy subunits = 1 energy unit displayed by
          the 4-digit ENERGY readout of the Control Panel Display). Values are
          picked from table DRAINRATETAB ($BAD3). See also subroutine UPDPANEL
          ($B804).

 $81      SHIELDSCOLOR

          Shields color. Used values are: 
            $00 -> {BLACK} (Shields are off)
            $A0 -> {DARK GREEN} (Shields are on)

 $82      PL3HIT

          Collision register of PLAYER3 (usually our starship's photon torpedo
          0) with other PLAYERs. Used values are:
              $00 -> No collision
            > $00 -> PLAYER3 has collided with another PLAYER space object. See
                     subroutine COLLISION ($AF3D) for details which PLAYER has
                     been hit by PLAYER3.

 $83      PL4HIT

          Collision register of PLAYER4 (usually our starship's photon torpedo
          1) with other PLAYERs. Used values are:
              $00 -> No collision
            > $00 -> PLAYER4 has collided with another PLAYER space object. See
                     subroutine COLLISION ($AF3D) for details which PLAYER has
                     been hit by PLAYER4.

 $84      OLDTRIG0

          Joystick trigger state. Used values are:
            $00 -> Joystick trigger was pressed
            $01 -> Joystick trigger was not pressed
            $AA -> Joystick trigger was "virtually" pressed (will launch
                   another of our starship's photon torpedoes, see subroutine
                   TRIGGER ($AE29).

 $86      ISTRACKING

          Indicates whether one of our starship's photon torpedoes is currently
          tracking (homing in on) the target space object. Used values are:
              $00 -> No target space object tracked. Our starship's photon
                     torpedoes will fly just straight ahead.
            > $00 -> Tracking a target space object. Our starship's photon
                     torpedoes will home in on the tracked space object.

 $87      BARRELNR

          Barrel from which our starship's next photon torpedo will be
          launched. Used values are:
            $00 -> Left barrel
            $01 -> Right barrel

 $88      LOCKONLIFE

          Lifetime of target lock-on. A target remains in lock-on while
          LOCKONLIFE ($88) counts down from 12 to 0. It is decremented every
          game loop iteration.

 $89      PLTRACKED

          Index of currently tracked PLAYER. It is copied in subroutine TRIGGER
          ($AE29) from TRACKDIGIT ($095C). Used values are:
            $00 -> Track Zylon ship 0
            $01 -> Track Zylon ship 1
            $02 -> Track starbase during docking operations
            $03 -> Track Hyperwarp Target Marker during hyperwarp

 $8A      HITBADNESS

          Severity of a Zylon photon torpedo hit. Used values are:
            $00 -> NO HIT
            $7F -> SHIELDS HIT
            $FF -> STARSHIP DESTROYED

 $8B      REDALERTLIFE

          Lifetime of red alert. It decreases from 255 to 0. It is decremented
          every game loop iteration.

 $8C      vWarpDeprRow

          Departure hyperwarp marker row number on the Galactic Chart. It is
          given in Player/Missile pixels relative to the top Galactic Chart
          border. It is initialized to a value of $47 (vertical center of
          Galactic Chart). Divide this value by 16 to get the departure sector
          row number. Used values are: $00..$7F.

 $8D      vWarpDeprColumn

          Departure hyperwarp marker column number on the Galactic Chart. It is
          given in Player/Missile pixels relative to the left Galactic Chart
          border and initialized to a value of $43 (horizontal center of
          Galactic Chart). Divide this value by 8 to get the departure sector
          column number. Used values are: $00..$7F.

 $8E      vWarpArrvRow

          Arrival hyperwarp marker row number on the Galactic Chart in
          Player/Missile pixels relative to top Galactic Chart border. It is
          initialized to a value of $47 (vertical center of Galactic Chart).
          Divide this value by 16 to get the arrival sector row number. Used
          values are: $00..$7F. 

 $8F      vWarpArrvColumn

          Arrival hyperwarp marker column number on the Galactic Chart in
          Player/Missile pixels relative to left Galactic Chart border. It is
          initialized to a value of $43 (horizontal center of Galactic Chart).
          Divide this value by 8 to get the arrival sector column number. Used
          values are: $00..$7F. 

 $90      vCurrentSector

          Galactic Chart sector of the current location of our starship. At the
          start of the game it is initialized to a value of $48. Used values
          are: $00..$7F with, for example,
            $00 -> NORTHWEST corner sector
            $0F -> NORTHEAST corner sector
            $70 -> SOUTHWEST corner sector
            $7F -> SOUTHWEST corner sector

          See also ARRVSECTOR ($92). 

 $91      WARPENERGY

          Energy required to hyperwarp between the departure and arrival
          hyperwarp marker locations on the Galactic Chart divided by 10.
          Values are picked from table WARPENERGYTAB ($BADD). Multiply this
          value by 10 to get the actual value in energy units displayed by the
          Galactic Chart Panel Display.

 $92      ARRVSECTOR

          Galactic Chart arrival sector of our starship after hyperwarp. Used
          values are: $00..$7F with, for example,
            $00 -> NORTHWEST corner sector
            $0F -> NORTHEAST corner sector
            $70 -> SOUTHWEST corner sector
            $7F -> SOUTHWEST corner sector

          See also vCurrentSector ($90). 

 $93      HUNTSECTOR

          Galactic Chart sector of the starbase toward which the Zylon units
          are currently moving. Used values are: $00..$7F with, for example,
            $00 -> NORTHWEST corner sector
            $0F -> NORTHEAST corner sector
            $70 -> SOUTHWEST corner sector
            $7F -> SOUTHWEST corner sector

 $94      HUNTSECTCOLUMN

          Galactic Chart sector column number of the starbase toward which the
          Zylon units are currently moving. Used values are: 0..15.

 $95      HUNTSECTROW

          Galactic Chart sector row number of the starbase toward which the
          Zylon units are currently moving. Used values are: 0..7.

 $96..$9E NEWZYLONDIST

          Table of distances between a Zylon unit and the hunted starbase when
          the Zylon unit is tentatively moved in one of the 9 possible
          directions NORTH, NORTHWEST, WEST, SOUTHWEST, SOUTH, SOUTHEAST, EAST,
          NORTHEAST, CENTER. Used to decide into which sector the Zylon unit
          should move.

 $9E      OLDZYLONDIST

          Current distance between the Zylon unit and the hunted starbase.

 $9F      HUNTTIM

          Delay timer for Zylon units to decide on which starbase to hunt. It
          counts down from 7. It is decremented every game loop iteration. When
          the timer falls below 0 the Zylon units re-decide toward which
          starbase to move.

 $A0      BLIPCOLUMN

          Top-left screen pixel column number of blip shape displayed in the
          Attack Computer Display. Used in subroutine UPDATTCOMP ($A7BF). Used
          values are: 120..142.

 $A1      BLIPROW

          Top-left screen pixel row number of blip shape displayed in the
          Attack Computer Display. Used in subroutine UPDATTCOMP ($A7BF). Used
          values are: 71..81.

 $A2      BLIPCYCLECNT

          Blip cycle counter. It controls drawing the blip shape in the Attack
          Computer Display. Its value is incremented every game loop iteration.
          Used in subroutine UPDATTCOMP ($A7BF). Used values are:
            $00..$04 -> Draw 0..4th row of blip shape
            $05..$09 -> Do not draw blip shape (delay)
            $0A      -> Recalculate blip shape position, erase Attack Computer
                        Display

 $A3      ISINLOCKON

          Indicates whether the tracked space object is currently in full
          lock-on (horizontally and vertically centered as well as in range) in
          the Attack Computer Display. If so, all lock-on markers show up on
          the Attack Computer Display and our starship's launched photon
          torpedoes will home in on the tracked space object. Used values are:
            $00 -> Not in lock-on
            $A0 -> In lock-on

 $A4      DIRLEN

          Used to pass the direction and length of a single line to be drawn in
          the PLAYFIELD. Used in subroutines DRAWLINES ($A76F), DRAWLINE
          ($A782), and UPDATTCOMP ($A7BF). Used values are:
            Bit B7 = 0 -> Draw right
            Bit B7 = 1 -> Draw down
            Bits B6..0 -> Length of line in pixels.

          See also PENROW ($A5) and PENCOLUMN ($A6).

 $A5      PENROW

          Used to pass the start screen pixel row number of the line to be
          drawn in the PLAYFIELD. Used in subroutines DRAWLINES ($A76F),
          DRAWLINE ($A782), and UPDATTCOMP ($A7BF).

 $A6      PENCOLUMN

          Used to pass the start screen pixel column number of the line to be
          drawn in the PLAYFIELD. Used in subroutines DRAWLINES ($A76F),
          DRAWLINE ($A782), and UPDATTCOMP ($A7BF).

 $A7      CTRLDZYLON

          Index of Zylon ship currently controlled by the game. Used in
          subroutine MANEUVER ($AA79). The value is toggled every other game
          loop iteration. Used values are:
            $00 -> Control Zylon ship 0.
            $01 -> Control Zylon ship 1.

 $A8      ZYLONFLPAT0

          Flight pattern of Zylon ship 0. Used in subroutine MANEUVER ($AA79).
          Used values are:
            $00 -> Attack flight pattern "0"
            $01 -> Flight pattern "1"
            $04 -> Flight pattern "4"

 $A9      ZYLONFLPAT1

          Flight pattern of Zylon ship 1. Compare ZYLONFLPAT0 ($A8).

 $AA      MILESTTIM0

          Delay timer of the milestone velocity indices of Zylon ship 0. Used
          in subroutine MANEUVER ($AA79).

          When Zylon ship 0 is active, this value is decremented every game
          loop iteration. If it falls below 0 then the milestone velocity
          indices of Zylon ship 0 are recalculated. When Zylon ship 0 is
          controlled by the computer for the first time, the timer is set to an
          initial value of 1, later to an initial value of 120.

 $AB      MILESTTIM1

          Delay timer of the milestone velocity index vector of Zylon ship 1.
          Compare MILESTTIM0 ($AA).

 $AC      MILESTVELINDZ0

          Milestone z-velocity index of Zylon ship 0. Used in subroutine
          MANEUVER ($AA79). The current z-velocity index of Zylon ship 0
          ZYLONVELINDZ0 ($B2) is compared with this index and gradually
          adjusted to it. Used values are: 0..15.

 $AD      MILESTVELINDZ1

          Milestone z-velocity index of Zylon ship 1. Compare MILESTVELINDZ0
          ($AC).

 $AE      MILESTVELINDX0

          Milestone x-velocity index of Zylon ship 0. Used in subroutine
          MANEUVER ($AA79). The current x-velocity index of Zylon ship 0
          ZYLONVELINDX0 ($B4) is compared with this index and gradually
          adjusted to it. Used values are: 0..15.

 $AF      MILESTVELINDX1

          Milestone x-velocity index of Zylon ship 1. Compare MILESTVELINDX0
          ($AE).

 $B0      MILESTVELINDY0

          Milestone y-velocity index of Zylon ship 0. Used in subroutine
          MANEUVER ($AA79). The current y-velocity index of Zylon ship 0
          ZYLONVELINDY0 ($B6) is compared with this index and gradually
          adjusted to it. Used values are: 0..15.

 $B1      MILESTVELINDY1

          Milestone y-velocity index of Zylon ship 1. Compare MILESTVELINDY0
          ($B0).

 $B2      ZYLONVELINDZ0

          Current z-velocity index of Zylon ship 0. Used in subroutine MANEUVER
          ($AA79). It indexes velocity values in ZYLONVELTAB ($BF99). Used
          values are: 0..15.

 $B3      ZYLONVELINDZ1

          Current z-velocity index of Zylon ship 1. Compare ZYLONVELINDZ0
          ($B2).

 $B4      ZYLONVELINDX0

          Current x-velocity index of Zylon ship 0. Compare ZYLONVELINDZ0
          ($B2).

 $B5      ZYLONVELINDX1

          Current x-velocity index of Zylon ship 1. Compare ZYLONVELINDZ0
          ($B2).

 $B6      ZYLONVELINDY0

          Current y-velocity index of Zylon ship 0. Compare ZYLONVELINDZ0
          ($B2).

 $B7      ZYLONVELINDY1

          Current y-velocity index of Zylon ship 1. Compare ZYLONVELINDZ0
          ($B2).

 $B8      ISBACKATTACK0

          Indicates whether Zylon ship 0 will attack our starship from the
          back. Used in subroutine MANEUVER ($AA79). Used values are:
            $00 -> Zylon ship 0 attacks from the front of our starship
            $01 -> Zylon ship 0 attacks from the front and back of our starship

 $B9      ISBACKATTACK1

          Indicates whether Zylon ship 1 will attack our starship from the
          back. Compare ISBACKATTACK0 ($B8).

 $BA      ZYLONTIMX0

          Delay timer of the x-velocity index of Zylon ship 0. Used in
          subroutine MANEUVER ($AA79). It is decremented every game loop
          iteration. When the timer value falls below 0 the current velocity
          index ZYLONVELINDX0 ($B4) is adjusted depending on the current
          joystick position. The new timer value is set depending on the
          resulting new x-velocity index. Used values are: 0, 2, 4, ..., 14.

 $BB      ZYLONTIMX1

          Delay timer of x-velocity index of Zylon ship 1. Compare ZYLONTIMX0
          ($BA).

 $BC      ZYLONTIMY0

          Delay timer of y-velocity index of Zylon ship 0. Compare ZYLONTIMX0
          ($BA).

 $BD      ZYLONTIMY1

          Delay timer of y-velocity index of Zylon ship 1. Compare ZYLONTIMX0
          ($BA).

 $BE      TORPEDODELAY

          After a Zylon photon torpedo has hit our starship this delay timer is
          initialized to a value of 2. It is decremented every game loop
          iteration and so delays the launch of the next Zylon photon torpedo
          for 2 game loop iterations.

 $BF      ZYLONATTACKER

          Index of the Zylon ship that launched the Zylon photon torpedo. It is
          used in GAMELOOP ($A1F3) to override the current tracking computer
          settings in order to track this Zylon ship first. Used values are:
            $00 -> Zylon photon torpedo was launched by Zylon ship 0
            $01 -> Zylon photon torpedo was launched by Zylon ship 1

 $C0      WARPSTATE

          Hyperwarp state. Used values are:
            $00 -> Hyperwarp not engaged
            $7F -> Hyperwarp engaged
            $FF -> In hyperspace

 $C1      VELOCITYHI

          Our starship's velocity (high byte) in <KM/H>. Used values are:
            $00 -> Not in hyperspace (regular cruise or accelerating to
                   hyperspace velocity)
            $01 -> Hyperspace velocity

          See also VELOCITYLO ($70). 

 $C2      TRAILDELAY

          Delay timer to create the next star trail. Its value is decremented
          from 3 to 0 every game loop iteration during the hyperwarp STAR TRAIL
          PHASE in subroutine INITTRAIL ($A9B4).

 $C3      TRAILIND

          Position vector index of the star trail's first star. Used in
          subroutine INITTRAIL ($A9B4) to initialize a star trail, which is
          then displayed during the hyperwarp STAR TRAIL PHASE. Used values
          are: 17..48 in wrap-around fashion.

 $C4      WARPTEMPCOLUMN

          Temporary arrival column number of our starship on the Galactic Chart
          at the beginning of hyperspace. It is given in Player/Missile pixels
          relative to the left Galactic Chart border. Divide this value by 8 to
          get the sector column number. Used values are: $00..$7F. See also
          vWarpArrvColumn ($8F).

 $C5      WARPTEMPROW

          Temporary arrival row number of our starship on the Galactic Chart at
          the beginning of hyperspace. It is given in Player/Missile pixels
          relative to top Galactic Chart border. Divide this value by 16 to get
          the sector row number.  Used values are: $00..$7F. See also
          vWarpArrvRow ($8E).

 $C6      VEERMASK

          Limits the veer-off velocity of the Hyperwarp Target Marker during
          the hyperwarp ACCELERATION PHASE in subroutine HYPERWARP ($A89B).
          Values are picked from table VEERMASKTAB ($BED7).

          Also used as a local variable.

 $C7      VICINITYMASK

          Mask to confine space objects' position vector components
          (coordinates) in a sector into a certain interval around our starship
          after its arrival from hyperspace. Values are picked from table
          VICINITYMASKTAB ($BFB3).

 $C8      JOYSTICKX

          Horizontal joystick direction. Values are picked from table
          STICKINCTAB ($BAF5). Used values are:
            $01 -> Right
            $00 -> Centered
            $FF -> Left

 $C9      JOYSTICKY

          Vertical joystick direction. Values are picked from table STICKINCTAB
          ($BAF5). Used values are:
            $01 -> Up
            $00 -> Centered 
            $FF -> Down

 $CA      KEYCODE

          Hardware keyboard code of the pressed key on the keyboard. Shift and
          Control key bits B7..6 are always set.

 $CB..$CC SCORE

          Internal 16-bit score of the game in low byte-high byte order

 $CD      SCOREDRANKIND

          Scored Rank Index. It is translated with table RANKTAB ($BEE9) to a
          title phrase offset pointing to the rank string. Used values are: 
            $00 -> GALACTIC COOK
            $01 -> GARBAGE SCOW CAPTAIN
            $02 -> GARBAGE SCOW CAPTAIN
            $03 -> ROOKIE
            $04 -> ROOKIE
            $05 -> NOVICE
            $06 -> NOVICE
            $07 -> ENSIGN
            $08 -> ENSIGN
            $09 -> PILOT
            $0A -> PILOT
            $0B -> ACE
            $0C -> LIEUTENANT
            $0D -> WARRIOR
            $0E -> CAPTAIN
            $0F -> COMMANDER
            $10 -> COMMANDER
            $11 -> STAR COMMANDER
            $12 -> STAR COMMANDER

 $CE      SCOREDCLASSIND

          Scored Class Index. It is translated into a class number with table
          CLASSTAB ($BEFC). Used values are:
            $00 -> Class 5
            $01 -> Class 5
            $02 -> Class 5
            $03 -> Class 4
            $04 -> Class 4
            $05 -> Class 4
            $06 -> Class 4
            $07 -> Class 3
            $08 -> Class 3
            $09 -> Class 3
            $0A -> Class 2
            $0B -> Class 2
            $0C -> Class 2
            $0D -> Class 1
            $0E -> Class 1
            $0F -> Class 1

 $CF      TITLELIFE

          Lifetime of title line. It is decremented every game loop iteration.
          Used initial values are:
            $3C -> When displaying regular title phrases
            $FE -> When displaying "STARBASE SURROUNDED", "STARBASE DESTOYED",
                   and "RED ALERT" messages
            $FF -> Hide title line

 $D0      SHIPVIEW

          Current view of our starship. Values are picked from table
          VIEWMODETAB ($BE22). Used values are:
            $00 -> Front view
            $01 -> Aft view
            $40 -> Long-Range Scan view
            $80 -> Galactic Chart view

 $D1      TITLEPHR

          Title phrase offset for text phrase in title line. Used values are:
            $00..$7B -> Title phrase offset into PHRASETAB ($BBAA)
            $FF      -> Hide title line

          See also NEWTITLEPHR ($65). 

 $D2      BEEPFRQIND

          Beeper sound pattern: Running index into frequency table BEEPFRQTAB
          ($BF5C). See also BEEPFRQSTART ($D7). See also subroutines BEEP
          ($B3A6) and SOUND ($B2AB).

 $D3      BEEPREPEAT

          Beeper sound pattern: Number of times the beeper sound pattern is
          repeated - 1. See also subroutines BEEP ($B3A6) and SOUND ($B2AB).

 $D4      BEEPTONELIFE

          Beeper sound pattern: Lifetime of tone in TICKs - 1. See also
          subroutines BEEP ($B3A6) and SOUND ($B2AB).

 $D5      BEEPPAUSELIFE

          Beeper sound pattern: Lifetime of pause in TICKs - 1. Used values
          are: 
            < $FF -> Number of TICKs - 1 to play
              $FF -> Skip playing pause

          See also subroutines BEEP ($B3A6) and SOUND ($B2AB).

 $D6      BEEPPRIORITY

          Beeper sound pattern: Pattern priority. Each beeper sound pattern has
          a priority. When a pattern of higher priority is about to be played
          the pattern that is currently playing is stopped. Used values are:
              $00 -> No pattern playing at the moment
            > $00 -> Pattern priority

          See also subroutines BEEP ($B3A6) and SOUND ($B2AB).

 $D7      BEEPFRQSTART

          Beeper sound pattern: Index to first byte of the pattern frequency in
          table BEEPFRQTAB ($BF5C). See also BEEPFRQIND ($D2). See also
          subroutines BEEP ($B3A6) and SOUND ($B2AB).

 $D8      BEEPLIFE

          Beeper sound pattern: Lifetime of the current tone or pause in TICKs.
          It is decremented every TICK. See also subroutines BEEP ($B3A6) and
          SOUND ($B2AB). 

 $D9      BEEPTOGGLE

          Beeper sound pattern: Indicates that either a tone or a pause is
          currently played. Used values are:
            $00 -> Tone
            $01 -> Pause

          See also subroutines BEEP ($B3A6) and SOUND ($B2AB).    

 $DA      NOISETORPTIM

          Noise sound pattern: Delay timer for PHOTON TORPEDO LAUNCHED noise
          sound pattern. It is decremented every TICK. See also subroutines
          NOISE ($AEA8) and SOUND ($B2AB).

 $DB      NOISEEXPLTIM

          Noise sound pattern: Delay timer for SHIELD EXPLOSION and ZYLON
          EXPLOSION noise sound pattern. It is decremented every TICK. See also
          subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $DC      NOISEAUDC2

          Noise sound pattern: Audio channel 1/2 control shadow register. See
          also subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $DD      NOISEAUDC3

          Noise sound pattern: Audio channel 3 control shadow register. See
          also subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $DE      NOISEAUDF1

          Noise sound pattern: Audio channel 1 frequency shadow register. See
          also subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $DF      NOISEAUDF2

          Noise sound pattern: Audio channel 2 frequency shadow register. See
          also subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $E0      NOISEFRQINC

          Noise sound pattern: Audio channel 1/2 frequency increment. See also
          subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $E1      NOISELIFE

          Noise sound pattern: Noise sound pattern lifetime. It is decremented
          every TICK. See also subroutines NOISE ($AEA8) and SOUND ($B2AB).

 $E2      NOISEZYLONTIM

          Delay timer to trigger the ZYLON EXPLOSION noise sound pattern. It is
          set in subroutine COLLISION ($AF3D) when an impact of one of our
          starship's photon torpedoes into a target is imminent. The timer is
          decremented every TICK during the execution of the Vertical Blank
          Interrupt handler VBIHNDLR ($A6D1). When the timer value reaches 0
          the ZYLON EXPLOSION noise sound pattern is played in subroutine SOUND
          ($B2AB). 

 $E3      NOISEHITLIFE

          Lifetime of STARSHIP EXPLOSION noise when our starship was destroyed
          by a Zylon photon torpedo. It is set in routine GAMELOOP ($A1F3) to a
          value of 64 TICKs. It is decremented every TICK during the execution
          of the Vertical Blank Interrupt handler VBIHNDLR ($A6D1).

 $E4      PL0SHAPOFF

          PLAYER0 offset into shape table PLSHAP2TAB ($B9B1)

 $E5      PL1SHAPOFF

          PLAYER1 offset into shape table PLSHAP2TAB ($B9B1)

 $E6      PL2SHAPOFF

          PLAYER2 offset into shape table PLSHAP1TAB ($B8E4)

 $E7      PL3SHAPOFF

          PLAYER3 offset into shape table PLSHAP1TAB ($B8E4)

 $E8      PL4SHAPOFF

          PLAYER4 offset into shape table PLSHAP1TAB ($B8E4)

 $E9      PL0LIFE

          Lifetime of the space object represented by PLAYER0 (usually Zylon
          ship 0). Any value other than $FF is decremented with every game loop
          iteration. Used values are:
            $00      -> Space object not alive (= not in use)
            $01..$FE -> Values during lifetime countdown
            $FF      -> Infinite lifetime (not counted down)

 $EA      PL1LIFE

          Lifetime of a space object represented by PLAYER1 (usually Zylon ship
          1). Compare PL0LIFE ($E9).

 $EB      PL2LIFE

          Lifetime of a space object represented by PLAYER2 (usually the Zylon
          photon torpedo). Compare PL0LIFE ($E9).

          If this PLAYER represents a photon torpedo, its lifetime is
          decremented from an initial value of $FF.

 $EC      PL3LIFE

          Lifetime of a space object represented by PLAYER3 (usually our
          starship's photon torpedo 0). Compare PL2LIFE ($EB).

          If this PLAYER represents a photon torpedo, its lifetime is
          decremented from an initial value of $FF.

 $ED      PL4LIFE

          Lifetime of a space object represented by PLAYER4 (usually our
          starship's photon torpedo 1). Compare PL2LIFE ($EB).

          If this PLAYER represents a photon torpedo, its lifetime is
          decremented from an initial value of $FF.

 $EE      PL0COLOR

          Color of PLAYER0

 $EF      PL1COLOR

          Color of PLAYER1

 $F0      PL2COLOR

          Color of PLAYER2

 $F1      PL3COLOR

          Color of PLAYER3

 $F2      PF0COLOR

          Color of PLAYFIELD0

 $F3      PF1COLOR

          Color of PLAYFIELD1

 $F4      PF2COLOR

          Color of PLAYFIELD2

 $F5      PF3COLOR

          Color of PLAYFIELD3

 $F6      BGRCOLOR

          Color of BACKGROUND

 $F7      PF0COLORDLI

          Color of PLAYFIELD0 after DLI

 $F8      PF1COLORDLI

          Color of PLAYFIELD1 after DLI

 $F9      PF2COLORDLI

          Color of PLAYFIELD2 after DLI

 $FA      PF3COLORDLI

          Color of PLAYFIELD3 after DLI

 $FB      BGRCOLORDLI

          Color of BACKGROUND after DLI

 $0280..$02E9 DSPLST

              Display List

 $0300..$03FF PL4DATA

              PLAYER4 data area

 $0400..$04FF PL0DATA

              PLAYER0 data area

 $0500..$05FF PL1DATA

              PLAYER1 data area

 $0600..$06FF PL2DATA

              PLAYER2 data area

 $0700..$07FF PL3DATA

              PLAYER3 data area

 $0800..$0863 PFMEMROWLO

              Lookup table of start addresses (low byte) for each row of
              PLAYFIELD memory, which is located at PFMEM ($1000). The table
              contains 100 bytes for 100 rows (of which only 99 are shown by
              the Display List, the PLAYFIELD is 160 x 99 pixels). The
              addresses grow in increments of 40 (40 bytes = 160 pixels in
              GRAPHICS7 mode = 1 PLAYFIELD row of pixels). See also PFMEMROWHI
              ($0864).

 $0864..$08C7 PFMEMROWHI

              Lookup table of start addresses (high byte) of each row of
              PLAYFIELD memory. See also PFMEMROWLO ($0800).

 $08C9..$0948 gcMemMap

              Galactic Chart memory map (16 columns x 8 rows = 128 bytes)

 $0949..$0970 PANELTXT

              Memory of Control Panel Display (bottom text window) in Front
              view, Aft view, and Long-Range Scan view (20 characters x 2 rows
              = 40 bytes).

 $094A        VELOCD1

              First digit (of 2) of the VELOCITY readout in Control Panel
              Display memory.

 $0950        KILLCNTD1

              First digit (of 2) of the KILL COUNTER readout in Control Panel
              Display memory.

 $0955        ENERGYD1

              First digit (of 4) of the ENERGY readout in Control Panel Display
              memory.

 $095A        TRACKC1

              Character of the TRACKING readout 'T' or 'C' in Control Panel
              Display memory.

 $095C        TRACKDIGIT

              Digit of the TRACKING readout in Control Panel Display memory. It
              is used to store the index of the currently tracked space object.
              Used values are:
                $00 -> Track Zylon ship 0
                $01 -> Track Zylon ship 1
                $02 -> Track starbase
                $03 -> Track Hyperwarp Target Marker

 $0960        THETAC1

              First character of the THETA readout in Control Panel Display
              memory.

 $0966        PHIC1

              First character of the PHI readout in Control Panel Display
              memory.

 $096C        RANGEC1

              First character of the RANGE readout in Control Panel Display
              memory.

 $0971..$09AC GCTXT

              Memory of Galactic Chart Panel Display (bottom text window) of
              Galactic Chart view (20 characters x 3 rows = 60 bytes).

 $097D        GCWARPD1

              First digit (of 4) of the HYPERWARP ENERGY readout in Galactic
              Chart Panel Display memory.

 $098D        GCTRGCNT

              First target counter digit (of 2) in Galactic Chart Panel Display
              memory.

 $0992        GCSTATPHO

              Photon Torpedo status letter in Galactic Chart Panel Display
              memory. Used values are:
                %00****** -> OK
                %10****** -> Destroyed
                %11****** -> Damaged

 $0993        GCSTATENG

              Engines status letter in Galactic Chart Panel Display memory.
              Used values are:
                %00****** -> OK
                %10****** -> Destroyed
                %11****** -> Damaged

 $0994        GCSTATSHL

              Shields status letter in Galactic Chart Panel Display memory.
              Used values are:
                %00****** -> OK
                %10****** -> Destroyed
                %11****** -> Damaged

 $0995        GCSTATCOM

              Attack Computer status letter in Galactic Chart Panel Display
              memory. Used values are:
                %00****** -> OK
                %10****** -> Destroyed
                %11****** -> Damaged

 $0996        GCSTATLRS

              Long-Range Scan status letter in Galactic Chart Panel Display
              memory. Used values are:
                %00****** -> OK
                %10****** -> Destroyed
                %11****** -> Damaged

 $0997        GCSTATRAD

              Subspace Radio status letter in Galactic Chart Panel Display
              memory. Used values are:
                %00****** -> OK
                %10****** -> Destroyed
                %11****** -> Damaged

 $09A3        GCSTARDAT

              First (of 5) digits of the star date clock in the Galactic Chart
              Panel Display memory.

 $09AD..$09DD ZPOSSIGN

              Table containing the sign bit (B16) of position vector
              z-components (z-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). Used values
              are:
                $00 -> Negative sign (behind our starship)
                $01 -> Positive sign (in front of our starship)

              See also "ON POSITION VECTORS".

 $09AD        PL0ZPOSSIGN

              Sign bit (B16) of position vector z-component (z-coordinate) of
              PLAYER0. Compare ZPOSSIGN ($09AD). See also "ON POSITION
              VECTORS".

 $09AE        PL1ZPOSSIGN

              Sign bit (B16) of position vector z-component (z-coordinate) of
              PLAYER1. Compare ZPOSSIGN ($09AD). See also "ON POSITION
              VECTORS".

 $09AF        PL2ZPOSSIGN

              Sign bit (B16) of position vector z-component (z-coordinate) of
              PLAYER2. Compare ZPOSSIGN ($09AD). See also "ON POSITION
              VECTORS".

 $09B0        PL3ZPOSSIGN

              Sign bit (B16) of position vector z-component (z-coordinate) of
              PLAYER3. Compare ZPOSSIGN ($09AD). See also "ON POSITION
              VECTORS".

 $09B1        PL4ZPOSSIGN

              Sign bit (B16) of position vector z-component (z-coordinate) of
              PLAYER4. Compare ZPOSSIGN ($09AD). See also "ON POSITION
              VECTORS".

 $09DE..$0A0E XPOSSIGN

              Table containing the sign bit (B16) of position vector
              x-components (x-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). Used values
              are:
                $00 -> Negative sign (left)
                $01 -> Positive sign (right)

              See also "ON POSITION VECTORS".

 $09DE        PL0XPOSSIGN

              Sign bit (B16) of position vector x-component (x-coordinate) of
              PLAYER0. Compare XPOSSIGN ($09DE). See also "ON POSITION
              VECTORS".

 $09DF        PL1XPOSSIGN

              Sign bit (B16) of position vector x-component (x-coordinate) of
              PLAYER1. Compare XPOSSIGN ($09DE). See also "ON POSITION
              VECTORS".

 $09E0        PL2XPOSSIGN

              Sign bit (B16) of position vector x-component (x-coordinate) of
              PLAYER2. Compare XPOSSIGN ($09DE). See also "ON POSITION
              VECTORS".

 $09E1        PL3XPOSSIGN

              Sign bit (B16) of position vector x-component (x-coordinate) of
              PLAYER3. Compare XPOSSIGN ($09DE). See also "ON POSITION
              VECTORS".

 $09E2        PL4XPOSSIGN

              Sign bit (B16) of position vector x-component (x-coordinate) of
              PLAYER4. Compare XPOSSIGN ($09DE). See also "ON POSITION
              VECTORS".

 $0A0F..$0A3F YPOSSIGN

              Table containing the sign bit (B16) of position vector
              y-components (y-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). Used values
              are:
                $00 -> Negative sign (down)
                $01 -> Positive sign (up)

              See also "ON POSITION VECTORS".

 $0A0F        PL0YPOSSIGN

              Sign bit (B16) of position vector y-component (y-coordinate) of
              PLAYER0. Compare YPOSSIGN ($0A0F). See also "ON POSITION
              VECTORS".

 $0A10        PL1YPOSSIGN

              Sign bit (B16) of position vector y-component (y-coordinate) of
              PLAYER1. Compare YPOSSIGN ($0A0F). See also "ON POSITION
              VECTORS".

 $0A11        PL2YPOSSIGN

              Sign bit (B16) of position vector y-component (y-coordinate) of
              PLAYER2. Compare YPOSSIGN ($0A0F). See also "ON POSITION
              VECTORS".

 $0A12        PL3YPOSSIGN

              Sign bit (B16) of position vector y-component (y-coordinate) of
              PLAYER3. Compare YPOSSIGN ($0A0F). See also "ON POSITION
              VECTORS".

 $0A13        PL4YPOSSIGN

              Sign bit (B16) of position vector y-component (y-coordinate) of
              PLAYER4. Compare YPOSSIGN ($0A0F). See also "ON POSITION
              VECTORS".

 $0A40..$0A70 ZPOSHI

              Table containing the high byte (B15..8) of position vector
              y-components (y-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). See also
              "ON POSITION VECTORS".

 $0A40        PL0ZPOSHI

              High byte (B15..8) of position vector z-component (z-coordinate)
              of PLAYER0. Compare ZPOSHI ($0A40). See also "ON POSITION
              VECTORS".

 $0A41        PL1ZPOSHI

              High byte (B15..8) of position vector z-component (z-coordinate)
              of PLAYER1. Compare ZPOSHI ($0A40). See also "ON POSITION
              VECTORS".

 $0A42        PL2ZPOSHI

              High byte (B15..8) of position vector z-component (z-coordinate)
              of PLAYER2. Compare ZPOSHI ($0A40). See also "ON POSITION
              VECTORS".

 $0A43        PL3ZPOSHI

              High byte (B15..8) of position vector z-component (z-coordinate)
              of PLAYER3. Compare ZPOSHI ($0A40). See also "ON POSITION
              VECTORS".

 $0A44        PL4ZPOSHI

              High byte (B15..8) of position vector z-component (z-coordinate)
              of PLAYER4. Compare ZPOSHI ($0A40). See also "ON POSITION
              VECTORS".

 $0A71..$0AA1 XPOSHI

              Table containing the high byte (B15..8) of position vector
              x-components (x-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). See also
              "ON POSITION VECTORS".

 $0A71        PL0XPOSHI

              High byte (B15..8) of position vector x-component (x-coordinate)
              of PLAYER0. Compare XPOSHI ($0A71). See also "ON POSITION
              VECTORS".

 $0A72        PL1XPOSHI

              High byte (B15..8) of position vector x-component (x-coordinate)
              of PLAYER1. Compare XPOSHI ($0A71). See also "ON POSITION
              VECTORS".

 $0A73        PL2XPOSHI

              High byte (B15..8) of position vector x-component (x-coordinate)
              of PLAYER2. Compare XPOSHI ($0A71). See also "ON POSITION
              VECTORS".

 $0A74        PL3XPOSHI

              High byte (B15..8) of position vector x-component (x-coordinate)
              of PLAYER3. Compare XPOSHI ($0A71). See also "ON POSITION
              VECTORS".

 $0A75        PL4XPOSHI

              High byte (B15..8) of position vector x-component (x-coordinate)
              of PLAYER4. Compare XPOSHI ($0A71). See also "ON POSITION
              VECTORS".

 $0AA2..$0AD2 YPOSHI

              Table containing the high byte (B15..8) of position vector
              y-components (y-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). See also
              "ON POSITION VECTORS".

 $0AA2        PL0YPOSHI

              High byte (B15..8) of position vector y-component (y-coordinate)
              of PLAYER0. Compare YPOSHI ($0AA2). See also "ON POSITION
              VECTORS".

 $0AA3        PL1YPOSHI

              High byte (B15..8) of position vector y-component (y-coordinate)
              of PLAYER1. Compare YPOSHI ($0AA2). See also "ON POSITION
              VECTORS".

 $0AA4        PL2YPOSHI

              High byte (B15..8) of position vector y-component (y-coordinate)
              of PLAYER2. Compare YPOSHI ($0AA2). See also "ON POSITION
              VECTORS".

 $0AA5        PL3YPOSHI

              High byte (B15..8) of position vector y-component (y-coordinate)
              of PLAYER3. Compare YPOSHI ($0AA2). See also "ON POSITION
              VECTORS".

 $0AA6        PL4YPOSHI

              High byte (B15..8) of position vector y-component (y-coordinate)
              of PLAYER4. Compare YPOSHI ($0AA2). See also "ON POSITION
              VECTORS".

 $0AD3..$0B03 ZPOSLO

              Table containing the low byte (B7..0) of position vector
              z-components (z-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). See also
              "ON POSITION VECTORS".

 $0AD3        PL0ZPOSLO

              Low byte (B7..0) of position vector z-component (z-coordinate) of
              PLAYER0. Compare ZPOSLO ($0AD3). See also "ON POSITION VECTORS".

 $0AD4        PL1ZPOSLO

              Low byte (B7..0) of position vector z-component (z-coordinate) of
              PLAYER1. Compare ZPOSLO ($0AD3). See also "ON POSITION VECTORS".

 $0AD5        PL2ZPOSLO

              Low byte (B7..0) of position vector z-component (z-coordinate) of
              PLAYER2. Compare ZPOSLO ($0AD3). See also "ON POSITION VECTORS".

 $0AD6        PL3ZPOSLO

              Low byte (B7..0) of position vector z-component (z-coordinate) of
              PLAYER3. Compare ZPOSLO ($0AD3). See also "ON POSITION VECTORS".

 $0AD7        PL4ZPOSLO

              Low byte (B7..0) of position vector z-component (z-coordinate) of
              PLAYER4. Compare ZPOSLO ($0AD3). See also "ON POSITION VECTORS".

 $0B04..$0B34 XPOSLO

              Table containing the low byte (B7..0) of position vector
              x-components (x-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). See also
              "ON POSITION VECTORS".

 $0B04        PL0XPOSLO

              Low byte (B7..0) of position vector x-component (x-coordinate) of
              PLAYER0. Compare XPOSLO ($0B04). See also "ON POSITION VECTORS".

 $0B05        PL1XPOSLO

              Low byte (B7..0) of position vector x-component (x-coordinate) of
              PLAYER1. Compare XPOSLO ($0B04). See also "ON POSITION VECTORS".

 $0B06        PL2XPOSLO

              Low byte (B7..0) of position vector x-component (x-coordinate) of
              PLAYER2. Compare XPOSLO ($0B04). See also "ON POSITION VECTORS".

 $0B07        PL3XPOSLO

              Low byte (B7..0) of position vector x-component (x-coordinate) of
              PLAYER3. Compare XPOSLO ($0B04). See also "ON POSITION VECTORS".

 $0B08        PL4XPOSLO

              Low byte (B7..0) of position vector x-component (x-coordinate) of
              PLAYER4. Compare XPOSLO ($0B04). See also "ON POSITION VECTORS".

 $0B35..$0B65 YPOSLO

              Table containing the low byte (B7..0) of position vector
              y-components (y-coordinate) (49 bytes). Bytes 0..4 belong to
              position vectors of PLAYER space objects (Zylon ships, photon
              torpedoes, etc.). Bytes 5..48 belong to position vectors of
              PLAYFIELD space objects (stars, explosion fragments). See also
              "ON POSITION VECTORS".

 $0B35        PL0YPOSLO

              Low byte (B7..0) of position vector y-component (y-coordinate) of
              PLAYER0. Compare YPOSLO ($0B35). See also "ON POSITION VECTORS". 

 $0B36        PL1YPOSLO

              Low byte (B7..0) of position vector y-component (y-coordinate) of
              PLAYER1. Compare YPOSLO ($0B35). See also "ON POSITION VECTORS".

 $0B37        PL2YPOSLO

              Low byte (B7..0) of position vector y-component (y-coordinate) of
              PLAYER2. Compare YPOSLO ($0B35). See also "ON POSITION VECTORS".

 $0B38        PL3YPOSLO

              Low byte (B7..0) of position vector y-component (y-coordinate) of
              PLAYER3. Compare YPOSLO ($0B35). See also "ON POSITION VECTORS".

 $0B39        PL4YPOSLO

              Low byte (B7..0) of position vector y-component (y-coordinate) of
              PLAYER4. Compare YPOSLO ($0B35). See also "ON POSITION VECTORS".

 $0B66..$0B96 ZVEL

              Table containing velocity vector z-components (z-velocities) (49
              bytes). Bytes 0..4 belong to velocity vectors of PLAYER space
              objects (Zylon ships, photon torpedoes, etc.). Bytes 5..48 belong
              to velocity vectors of PLAYFIELD space objects (stars, explosion
              fragments). Each z-velocity is stored in the binary format
              %sxxxxxxx where
                %s = 0   -> Positive sign (moving in flight direction)
                %s = 1   -> Negative sign (moving in reverse flight direction)
                %xxxxxxx -> Unsigned 7-bit velocity value in <KM/H>

              See also "ON VELOCITY VECTORS".

 $0B66        PL0ZVEL

              Velocity vector z-component (z-velocity) of PLAYER0. Compare ZVEL
              ($0B66). See also "ON VELOCITY VECTORS".

 $0B67        PL1ZVEL

              Velocity vector z-component (z-velocity) of PLAYER1. Compare ZVEL
              ($0B66). See also "ON VELOCITY VECTORS".

 $0B68        PL2ZVEL

              Velocity vector z-component (z-velocity) of PLAYER2. Compare ZVEL
              ($0B66). See also "ON VELOCITY VECTORS".

 $0B69        PL3ZVEL

              Velocity vector z-component (z-velocity) of PLAYER3. Compare ZVEL
              ($0B66). See also "ON VELOCITY VECTORS".

 $0B6A        PL4ZVEL

              Velocity vector z-component (z-velocity) of PLAYER4. Compare ZVEL
              ($0B66). See also "ON VELOCITY VECTORS".

 $0B97..$0BC7 XVEL

              Table containing velocity vector x-components (x-velocities) (49
              bytes). Bytes 0..4 belong to velocity vectors of PLAYER space
              objects (Zylon ships, photon torpedoes, etc.). Bytes 5..48 belong
              to velocity vectors of PLAYFIELD space objects (stars, explosion
              fragments). Each x-velocity is stored in the binary format
              %sxxxxxxx where
                %s = 0   -> Positive sign (moving to the right)
                %s = 1   -> Negative sign (moving to the left)
                %xxxxxxx -> Unsigned 7-bit velocity value in <KM/H>

              See also "ON VELOCITY VECTORS".

 $0B97        PL0XVEL

              Velocity vector x-component (x-velocity) of PLAYER0. Compare XVEL
              ($0B97). See also "ON VELOCITY VECTORS". 

 $0B98        PL1XVEL

              Velocity vector x-component (x-velocity) of PLAYER1. Compare XVEL
              ($0B97). See also "ON VELOCITY VECTORS".

 $0B99        PL2XVEL

              Velocity vector x-component (x-velocity) of PLAYER2. Compare XVEL
              ($0B97). See also "ON VELOCITY VECTORS".

 $0B9A        PL3XVEL

              Velocity vector x-component (x-velocity) of PLAYER3. Compare XVEL
              ($0B97). See also "ON VELOCITY VECTORS".

 $0B9B        PL4XVEL

              Velocity vector x-component (x-velocity) of PLAYER4. Compare XVEL
              ($0B97). See also "ON VELOCITY VECTORS".

 $0BC8..$0BF8 YVEL

              Table containing velocity vector y-components (y-velocities) (49
              bytes). Bytes 0..4 belong to velocity vectors of PLAYER space
              objects (Zylon ships, photon torpedoes, etc.). Bytes 5..48 belong
              to velocity vectors of PLAYFIELD space objects (stars, explosion
              fragments). Each y-velocity is stored in the binary format
              %sxxxxxxx where
                %s = 0   -> Positive sign (moving up)
                %s = 1   -> Negative sign (moving down)
                %xxxxxxx -> Unsigned 7-bit velocity value in <KM/H>

              See also "ON VELOCITY VECTORS".

 $0BC8        PL0YVEL

              Velocity vector y-component (y-velocity) of PLAYER0. Compare YVEL
              ($0BC8). See also "ON VELOCITY VECTORS". 

 $0BC9        PL1YVEL

              Velocity vector y-component (y-velocity) of PLAYER1. Compare YVEL
              ($0BC8). See also "ON VELOCITY VECTORS".

 $0BCA        PL2YVEL

              Velocity vector y-component (y-velocity) of PLAYER2. Compare YVEL
              ($0BC8). See also "ON VELOCITY VECTORS".

 $0BCB        PL3YVEL

              Velocity vector y-component (y-velocity) of PLAYER3. Compare YVEL
              ($0BC8). See also "ON VELOCITY VECTORS".

 $0BCC        PL4YVEL

              Velocity vector y-component (y-velocity) of PLAYER4. Compare YVEL
              ($0BC8). See also "ON VELOCITY VECTORS".

 $0BF9..$0C29 PIXELROWNEW

              Table containing the new pixel row number of space objects (49
              bytes). Bytes 0..4 belong to PLAYER space objects and contain
              Player/Missile (PM) pixel row numbers. They are counted from
              vertical PM position 0, which is offscreen. Bytes 5..48 belong to
              PLAYFIELD space objects (stars, explosion fragments) and contain
              PLAYFIELD pixel row numbers. They are counted from the top border
              of the PLAYFIELD and have values of 0..99. See also PIXELROW
              ($0C5B).

 $0BF9        PL0ROWNEW

              New pixel row number of PLAYER0 in Player/Missile pixels. See
              also PIXELROWNEW ($0BF9).

 $0BFA        PL1ROWNEW

              New pixel row number of PLAYER1 in Player/Missile pixels. See
              also PIXELROWNEW ($0BF9).

 $0BFB        PL2ROWNEW

              New pixel row number of PLAYER2 in Player/Missile pixels. See
              also PIXELROWNEW ($0BF9).

 $0BFC        PL3ROWNEW

              New pixel row number of PLAYER3 in Player/Missile pixels. See
              also PIXELROWNEW ($0BF9).

 $0BFD        PL4ROWNEW

              New pixel row number of PLAYER4 in Player/Missile pixels. See
              also PIXELROWNEW ($0BF9).

 $0C2A..$0C5A PIXELCOLUMN

              Table containing the pixel column number of space objects (49
              bytes). Bytes 0..4 belong to PLAYER space objects and contain
              Player/Missile (PM) pixel column numbers. They are counted from
              horizontal PM position 0, which is offscreen. Bytes 5..48 belong
              to PLAYFIELD space objects (stars, explosion fragments) and
              contain PLAYFIELD pixel column numbers. They are counted from the
              left border of the PLAYFIELD and have values of 0..159.

 $0C2A        PL0COLUMN

              Pixel column number of PLAYER0 in Player/Missile pixels. See also
              PIXELCOLUMN ($0C2A).

 $0C2B        PL1COLUMN

              Pixel column number of PLAYER1 in Player/Missile pixels. See also
              PIXELCOLUMN ($0C2A).

 $0C2C        PL2COLUMN

              Pixel column number of PLAYER2 in Player/Missile pixels. See also
              PIXELCOLUMN ($0C2A).

 $0C2D        PL3COLUMN

              Pixel column number of PLAYER3 in Player/Missile pixels. See also
              PIXELCOLUMN ($0C2A).

 $0C2E        PL4COLUMN

              Pixel column number of PLAYER4 in Player/Missile pixels. See also
              PIXELCOLUMN ($0C2A).

 $0C5B..$0C8B PIXELROW

              Table containing the pixel row number of space objects (49
              bytes). Bytes 0..4 belong to PLAYER space objects and contain
              Player/Missile (PM) pixel row numbers. They are counted from
              vertical PM position 0, which is offscreen. Bytes 5..48 belong to
              PLAYFIELD space objects (stars, explosion fragments) and contain
              PLAYFIELD pixel row numbers. They are counted from the top border
              of the PLAYFIELD and have values of 0..99. See also PIXELROWNEW
              ($0BF9).

 $0C5B        PL0ROW

              Pixel row number of PLAYER0 in Player/Missile pixels. See also
              PIXELROW ($0C5B).

 $0C5C        PL1ROW

              Pixel row number of PLAYER1 in Player/Missile pixels. See also
              PIXELROW ($0C5B).

 $0C5D        PL2ROW

              Pixel row number of PLAYER2 in Player/Missile pixels. See also
              PIXELROW ($0C5B).

 $0C5E        PL3ROW

              Pixel row number of PLAYER3 in Player/Missile pixels. See also
              PIXELROW ($0C5B).

 $0C5F        PL4ROW

              Pixel row number of PLAYER4 in Player/Missile pixels. See also
              PIXELROW ($0C5B).

 $0C8C..$0CBC PIXELBYTEOFF

              Table containing a byte offset into PLAYFIELD memory for each
              PLAYFIELD space object (stars, explosion fragments) (49 bytes):
              the number of bytes from the start of the PLAYFIELD row to the
              byte containing the space object pixel in the same PLAYFIELD row.
              In other words, the pixel column modulo 4 (1 byte = 4 GRAPHICS7
              pixels).

              NOTE: Only bytes 5..48 are used for PLAYFIELD space objects in
              this way. Bytes 0..4 are used differently. See PL0SHAPTYPE
              ($0C8C)..PL4SHAPTYPE ($0C90).

 $0C8C        PL0SHAPTYPE

              Shape type of PLAYER0. Used to index the PLAYER's set of shape
              cells in tables PLSHAPOFFTAB ($BE2F) and PLSHAPHEIGHTTAB ($BE7F).
              Used values are:
                $00 -> PHOTON TORPEDO
                $10 -> ZYLON FIGHTER
                $20 -> STARBASE RIGHT
                $30 -> STARBASE CENTER
                $40 -> STARBASE LEFT
                $50 -> TRANSFER VESSEL
                $60 -> METEOR
                $70 -> ZYLON CRUISER
                $80 -> ZYLON BASESTAR
                $90 -> HYPERWARP TARGET MARKER

 $0C8D        PL1SHAPTYPE

              Shape type of PLAYER1. Compare PL0SHAPTYPE ($0C8C).

 $0C8E        PL2SHAPTYPE

              Shape type of PLAYER2. Compare PL0SHAPTYPE ($0C8C).

 $0C8F        PL3SHAPTYPE

              Shape type of PLAYER3. Compare PL0SHAPTYPE ($0C8C).

 $0C90        PL4SHAPTYPE

              Shape type of PLAYER4. Compare PL0SHAPTYPE ($0C8C).

 $0CBD..$0CED PIXELSAVE

              Table containing the byte of PLAYFIELD memory before drawing the
              PLAYFIELD space object pixel into it (star, explosion fragments),
              for each PLAYFIELD space object (49 bytes). 

              NOTE: Only bytes 5..48 are used for PLAYFIELD space objects in
              this way. Bytes 0..4 are used differently. See PL0HEIGHT
              ($0CBD)..PL4HEIGHT ($0CC1).

 $0CBD        PL0HEIGHT

              Shape height of PLAYER0

 $0CBE        PL1HEIGHT

              Shape height of PLAYER1

 $0CBF        PL2HEIGHT

              Shape height of PLAYER2

 $0CC0        PL3HEIGHT

              Shape height of PLAYER3

 $0CC1        PL4HEIGHT

              Shape height of PLAYER4

 $0CEE..$0D1E PIXELBYTE

              Table containing a 1-byte bit pattern for 4 pixels in the color
              of the space object's pixel, for each PLAYFIELD space object (49
              bytes). 

              NOTE: Only bytes 5..48 are used for PLAYFIELD space objects in
              this way. Bytes 0..4 are used differently. See PL0HEIGHTNEW
              ($0CEE)..PL4HEIGHTNEW ($0CF2).

 $0CEE        PL0HEIGHTNEW

              New shape height of PLAYER0

 $0CEF        PL1HEIGHTNEW

              New shape height of PLAYER1

 $0CF0        PL2HEIGHTNEW

              New shape height of PLAYER2

 $0CF1        PL3HEIGHTNEW

              New shape height of PLAYER3

 $0CF2        PL4HEIGHTNEW

              New shape height of PLAYER4

 $0D1F..$0D32 TITLETXT

              Title text line, contains ATASCII-coded characters (20 bytes)

 $0D35..$0DE8 gcPfMem

              Galactic Chart PLAYFIELD memory (20 characters x 9 rows = 180
              bytes)

 $0DE9..$0EE8 MAPTO80

              Lookup table to convert values in $00..$FF to values of 0..80
              (255 bytes). Used to map position vector components (coordinates)
              to pixel row or column numbers relative to the PLAYFIELD center. 

 $0EE9..$0FE8 MAPTOBCD99

              Lookup table to convert values in $00..$FF to BCD-values of
              00..99 (255 bytes). Used in subroutines UPDPANEL ($B804) and
              SHOWDIGITS ($B8CD) to convert values to a 2-digit decimal readout
              value of the Control Panel Display.

 $1000..$1F77 PFMEM

              PLAYFIELD graphics memory (40 bytes x 100 rows = 4000 bytes, 1
              byte stores 4 pixels, 40 bytes = 160 pixels in GRAPHICS7 mode = 1
              PLAYFIELD row of pixels).

              NOTE: The Display List displays only PLAYFIELD rows 0..98.
