% Copyright

goal
    console::runUtf8(main::run).

implement main
    open core, vip\, yTree, pfc\syntax\syntax

constants
    successExitCode : programControl::errorCode = 0.
    errorExitCode : programControl::errorCode = 1.
    exceptionExitCode :: programControl::errorCode = 2.

class facts
    error : boolean := false.
    error1 :: boolean := false.

clauses
    run() :-
        vipSyntaxSem::relaxedComma := true,
        vipSyntaxSem::parserError := parserErrorCallback,
        Buf = outputStream_string::new(),
        try
            console::inputStream:streamTo(Buf),
            Source = Buf:getString(),
            Buf:close(),
            tuple(SyntaxTree, ParseErrors) = vipParse(Source),
            console::outputStream:write(format(SyntaxTree, ParseErrors):getSome(Source)),
            ExitCode = if ParseErrors = false then successExitCode else errorExitCode end if,
            programControl::setApplicationExitCode(ExitCode)
        catch E do
            console::errorStream:write(exceptionDump::dumpToString(E)),
            console::errorStream:nl(),
            programControl::setApplicationExitCode(exceptionExitCode)
        end try.

class predicates
    format : (yTree::ySyntaxTree SyntaxTree, boolean ParseErrors) -> optional{string Formatted}.
clauses
    format(SyntaxTree, false) = some(Formatted) :-
        not(noFormat(SyntaxTree)),
        !,
        Buf = outputStream_string::new(),
        YTreePP = vip\yTreePP::new(Buf),
        YTreePP:pp_ySyntaxTree(SyntaxTree),
        Formatted = Buf:getString(),
        Buf:close().
    format(_, _) = none.

class predicates
    vipParse : (string Source) -> tuple{yTree::ySyntaxTree SyntaxTree, boolean Errors}.
clauses
    vipParse(Source) = tuple(YTree, error) :-
        error := false,
        VipParser = vipSyntax::new(parserErrorCallback),
        if YTree = VipParser:tryParse_prologUnit(Source) then
        else
            PC = VipParser:endCursor,
            YTree = vip\yTree::syntaxTree(cursor(shortCursor("<nofile>", PC, PC), [], []), [])
        end if.

class predicates
    noFormat : (ySyntaxTree) determ.
clauses
    noFormat(syntaxTree(_, [U | _])) :-
        cursor(_, PreComments, _) = getFirstCursor_yUnit(U),
        comment(_, Comment, _, _) in PreComments,
        _ = string::search(Comment, "@noformat", string::caseInsensitive),
        !.

class predicates
    getFirstCursor_yUnit : (yUnit) -> cursor.
clauses
    getFirstCursor_yUnit(scope(SK, _, _, _, _, _, _, _, _)) = getFirstCursor_yScopeKind(SK).
    getFirstCursor_yUnit(prologGoal(Cursor, _, _)) = Cursor.
    getFirstCursor_yUnit(directiveOutsideScope(D)) = getFirstCursor_yCompilerDirective(D).
    getFirstCursor_yUnit(conditionalUnit(Cursor, _, _, _)) = Cursor.
    getFirstCursor_yUnit(namespaceDeclaration(Cursor, _)) = Cursor.

class predicates
    getFirstCursor_yScopeKind : (yScopeKind) -> cursor.
clauses
    getFirstCursor_yScopeKind(interfaceKind(Cursor, _)) = Cursor.
    getFirstCursor_yScopeKind(classKind(Cursor, _)) = Cursor.
    getFirstCursor_yScopeKind(implementKind(Cursor)) = Cursor.

class predicates
    getFirstCursor_yCompilerDirective : (yCompilerDirective) -> cursor.
clauses
    getFirstCursor_yCompilerDirective(directiveGrammarGenerate(Cursor, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveInclude(Cursor, _, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveRequire(Cursor, _, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveError(Cursor, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveMessage(Cursor, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveExport(Cursor, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveExternally(Cursor, _)) = Cursor.
    getFirstCursor_yCompilerDirective(directiveOptions(Cursor, _)) = Cursor.

class predicates
    parserErrorCallback : pfc\syntax\syntax::errorCallBack.
clauses
    parserErrorCallback(Cursor, ErrorCode, Message, AdditionalInfo) :-
        error := true,
        cursor(shortCursor(SourceFile, StartCursor, _EndCursor), _, _) = Cursor,
        unpackCursor(StartCursor, Line, Pos),
        console::errorStream:writef("%s(%d,%d) : error c%d: %s", SourceFile, Line, Pos, convertErrorCode(ErrorCode), Message),
        if "" <> AdditionalInfo then
            stdio::errorStream:writef(" : %s\n", AdditionalInfo)
        end if,
        console::errorStream:nl().

class predicates
    convertErrorCode : (unsigned ParserCode) -> unsigned CompilerCode.
clauses
    convertErrorCode(errorCode_syntaxError) = 150 :-
        !.
    convertErrorCode(errorCode_unexpectedEndOfText) = 173 :-
        !.
    convertErrorCode(errorCode_invalidIntegralLiteral) = 110 :-
        !.
    convertErrorCode(errorCode_invalidFloatingPointLiteral) = 111 :-
        !.
    convertErrorCode(errorCode_invalidEscSequence) = 113 :-
        !.
    convertErrorCode(errorCode_unterminatedLiteral) = 115 :-
        !.
    convertErrorCode(errorCode_unterminatedComment) = 116 :-
        !.
    convertErrorCode(errorCode_unexpectedCharacter) = 117 :-
        !.
    convertErrorCode(errorCode_lowercaseIdentificatorExpected) = 155 :-
        !.
    convertErrorCode(errorCode_uppercaseIdentificatorExpected) = 200 :-
        !.
    convertErrorCode(errorCode_tokenExpected) = 161 :-
        !.
    convertErrorCode(errorCode_unterminatedEmbeddedSyntax) = errorCode_unterminatedEmbeddedSyntax :-
        !.
    convertErrorCode(ParserCode) = ParserCode.

end implement main
