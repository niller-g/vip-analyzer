% Copyright

class main
    open core

predicates
    main : ().
    % @short Format the text from stdin and write it to stdout.
    % @end

predicates
    bench : (string FixtureDirectory).
    % @short Benchmark the AST generation on all VIP files in `FixtureDirectory`.
    % @end

end class main
