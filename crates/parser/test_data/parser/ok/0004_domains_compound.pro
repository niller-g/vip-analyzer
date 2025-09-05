class a

domains
    dom01 = align 1 b.
    dom02 = align 1 b().
    dom03 = align 1 b [inline].
    dom04 = align 1 b() [inline].
    dom05 = b().
    dom06 = b() [inline].

domains
    dom07 = align 1 b; tail.
    dom08 = align 1 b() ; tail.
    dom09 = align 1 b [inline]; tail.
    dom09 = align 1 b() [inline]; tail.
    dom010 = b(); tail.
    dom11 = b() [inline]; tail.

domains
    dom12 = align 1 b; tail [tail_attrib1].
    dom13 = align 1 b() ; tail [tail_attrib1].
    dom14 = align 1 b [inline]; tail [tail_attrib1].
    dom14 = align 1 b() [inline]; tail [tail_attrib1].
    dom15 = b(); tail [tail_attrib1].
    dom16 = b() [inline]; tail [tail_attrib1].

domains
    dom17 = align 1 b; tail(string).
    dom18 = align 1 b() ; tail(string).
    dom19 = align 1 b [inline]; tail(string).
    dom20 = align 1 b() [inline]; tail(string).
    dom21 = b(); tail(string).
    dom22 = b() [inline]; tail(string).

domains
    dom23 = align 1 b; tail(string) [tail_attrib2].
    dom24 = align 1 b() ; tail(string) [tail_attrib2].
    dom25 = align 1 b [inline]; tail(string) [tail_attrib2].
    dom26 = align 1 b() [inline]; tail(string) [tail_attrib2].
    dom27 = b(); tail(string) [tail_attrib2].
    dom28 = b() [inline]; tail(string) [tail_attrib2].

end class