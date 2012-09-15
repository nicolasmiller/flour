describe("Flour", function() {
    describe("treeify", function() {
        var just_tokens = "   foo\nbar   quux\tbuzz";
        var treeified_just_tokens = ['foo', 'bar', 'quux', 'buzz'];

        var non_nested_s_exp = "(define foo 42)";
        var treeified_non_nested_s_exp = [['define', 'foo', '42']];

        var series_of_non_nested = "(define foo 34)" 
            + "\n(define bar 11)"
            + "\nfoo"
            + "\nbar"
            + "\n(+ foo bar)";
        var treeified_series_non_nested = [['define', 'foo', '32'], 'foo', 'bar', ['+', 'foo', 'bar']]';

        it("should give an array of tokens from a string of wspace-delimited tokens", function() {
            expect(Flour.treeify(just_tokens)).toBe(treeified_just_tokens);
        });

        it("should give a nested array of tokens for just a flat s-exp", function() {
            expect(Flour.treeify(non_nested_s_exp)).toBe(treeified_non_nested_s_exp);
        });

        it("should give an array of tokens and nested arrays for a series of tokens and s_exp", function() {
            expect(Flour.treeify(series_of_non_nested)).toBe(treeified_series_non_nested);
        });
    });
});
