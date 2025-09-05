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
    dom023 = C [inline].
    dom038 = @D [inline].
    dom053 = b* [inline].
    dom068 = C* [inline].
    dom083 = @D* [inline].
    dom098 = b** [inline].
    dom113 = C** [inline].
    dom128 = @D** [inline].

end class