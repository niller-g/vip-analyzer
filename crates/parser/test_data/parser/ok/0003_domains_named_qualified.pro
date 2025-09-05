class a

domains
    dom001 = b [1..2].
    dom002 = b [3..4] bitsize 5.
    dom003 = b [6..7] digits 8.
    dom004 = b bitsize 9.
    dom005 = b bitsize 10 [11..12].
    dom006 = b digits 13.
    dom007 = b digits 14 [15..16].
    dom008 = b [inline].
    dom009 = b [1..2] [inline].
    dom010 = b [3..4] bitsize 5 [inline].
    dom011 = b [6..7] digits 8 [inline].
    dom012 = b bitsize 9 [inline].
    dom013 = b bitsize 10 [11..12] [inline].
    dom014 = b digits 13 [inline].
    dom015 = b digits 14 [15..16] [inline].

domains
    dom053 = b* [inline].
    dom098 = b** [inline].
    dom053 = ns\b* [inline].
    dom098 = ns\b** [inline].
    dom053 = ns\b::c* [inline].
    dom098 = ns\b::c** [inline].
    dom008 = ns\b [inline].

domains
    dom001 = ns\b::c [1..2].
    dom002 = ns\b::c [3..4] bitsize 5.
    dom003 = ns\b::c [6..7] digits 8.
    dom004 = ns\b::c bitsize 9.
    dom005 = ns\b::c bitsize 10 [11..12].
    dom006 = ns\b::c digits 13.
    dom007 = ns\b::c digits 14 [15..16].
    dom008 = ns\b::c [inline].
    dom009 = ns\b::c [1..2] [inline].
    dom010 = ns\b::c [3..4] bitsize 5 [inline].
    dom011 = ns\b::c [6..7] digits 8 [inline].
    dom012 = ns\b::c bitsize 9 [inline].
    dom013 = ns\b::c bitsize 10 [11..12] [inline].
    dom014 = ns\b::c digits 13 [inline].
    dom015 = ns\b::c digits 14 [15..16] [inline].

end class