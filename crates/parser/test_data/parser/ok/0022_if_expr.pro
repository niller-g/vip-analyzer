implement a

clauses
    p() :-
        _ = if d then e else f end if,
        _ = if g then h elseif i then f else f end if,
        a(if a then b else "" end if),
        b(_ = if a then b else c end if).

end implement
