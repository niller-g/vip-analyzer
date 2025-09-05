class a

domains
    dom01 = [1..2].
    dom02 = [3..4] bitsize 5.
    dom03 = [6..7] digits 8.
    dom04 = bitsize 9.
    dom05 = bitsize 10 [11..12].
    dom06 = digits 13.
    dom07 = digits 14 [15..16].
    dom08 = [1..2] [inline].
    dom09 = [3..4] bitsize 5 [inline].
    dom10 = [6..7] digits 8 [inline].
    dom11 = bitsize 9 [inline].
    dom12 = bitsize 10 [11..12] [inline].
    dom13 = digits 13 [inline].
    dom14 = digits 14 [15..16] [inline].

end class