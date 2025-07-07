
mkdir -p obj/

# -------------------------------------

64tass  --m65c02 \
        --flat \
        --nostart \
        -D PGX=1 \
        -o obj/sraiders.pgx \
        --list=obj/sraiders.lst \
        --labels=obj/sraiders.lbl \
        sraiders.asm
