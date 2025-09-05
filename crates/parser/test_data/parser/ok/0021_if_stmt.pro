implement a

clauses
    p() :-
        if a then
        end if,
        
        if b then
            b
        end if,
        
        if c then
            d
        elseif e then
            f
        end if,

        if g then
            h
        elseif i then
            j
        else
            k
        end if,

        a(if a then end if),
        a((if a then b else c end if, d)),
        a(if a then b else a, b end if).

end implement
