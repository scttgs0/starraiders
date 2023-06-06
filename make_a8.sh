
64tass  --m65xx \
        --atari-xex \
        -o sraiders.xex \
        -D MEDIA=0 \
        --list=sraiders_a8_exe.lst \
        --labels=sraiders_a8_exe.lbl \
        sraiders.asm

64tass  --m65xx \
        --atari-xex \
        -b \
        -o sraiders.rom \
        -D MEDIA=1 \
        --list=sraiders_a8_rom.lst \
        --labels=sraiders_a8_rom.lbl \
        sraiders.asm
