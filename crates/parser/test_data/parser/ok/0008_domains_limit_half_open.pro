class a

domains
    dom01 = [1..].
    dom01 = [..2].
    dom01 = [1..] bitsize 5.
    dom01 = [..2] bitsize 5.
    dom01 = [1..] digits 5.
    dom01 = [..2] digits 5.

domains
    dom01 = [1..] [inline].
    dom01 = [..2] [inline].
    dom01 = [1..] bitsize 5 [inline].
    dom01 = [..2] bitsize 5 [inline].
    dom01 = [1..] digits 5 [inline].
    dom01 = [..2] digits 5 [inline].

end class