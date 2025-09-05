implement precedence

clauses
    p() :-
        e and f,
        _ = 1 + 2,
        _ = 3 ^ 4,
        _ = 5 - 6,
        _ = true -- false,
        _ = false ++ true,
        _ = 7 * 8,
        _ = true ** false,
        _ = ~~true,
        _ = 9 << 10,
        _ = 11 >> 12,
        _ = 13 / 14,
        _ = 15 div 16,
        _ = 17 mod 18,
        _ = 19 quot 20,
        _ = 21 rem 22,
        _ = a otherwise b,
        23 = 24,
        25 <> 26,
        27 > 28,
        29 >= 30,
        31 < 32,
        33 <= 34,
        35 == 36,
        c in d,
        m := (o:p),
        _ = 37 ^ 38,
        _ = 39 ^ 40.

end implement precedence
