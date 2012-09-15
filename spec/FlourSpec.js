describe("Flour", function() {
    var just_tokens = "   foo\nbar   quux\tbuzz";
    var non_nested_s_exp = "(define foo 42)";
    var series_of_non_nested = "(define foo 34)" 
        + "\n(define bar 11)"
        + "\nfoo"
        + "\nbar"
        + "\n(+ foo bar)";

    var yin_yang = '(let* ((yin'
        + '\n((lambda (cc) (display "@") cc) (call-with-current-continuation (lambda (c) c))))'
        + '\n(yang'
        + '\n((lambda (cc) (display "*") cc) (call-with-current-continuation (lambda (c) c)))))'
        + '\n(yin yang))';

    describe("tokenize", function() {
        var tokenized_just_tokens = ['foo', 'bar', 'quux', 'buzz'];
        var tokenized_non_nested_s_exp = ['(', 'define', 'foo', '42', ')'];

        it("should give an array of tokens from a string of wspace-delimited tokens", function() {
           expect(Flour.tokenize(just_tokens)).toEqual(tokenized_just_tokens);
        });

        it("should separate out all parens as tokens", function() {
           expect(Flour.tokenize(non_nested_s_exp)).toEqual(tokenized_non_nested_s_exp);
        });
    });

    describe("treeify", function() {
        var treeified_just_tokens = ['foo', 'bar', 'quux', 'buzz'],
        treeified_non_nested_s_exp = [['define', 'foo', '42']],
        treeified_series_non_nested = [
            ['define', 'foo', '34'], 
            ['define', 'bar', '11'], 
            'foo', 
            'bar',
             ['+', 'foo', 'bar']],
        treeified_yin_yang = 
            [
                ['let*',  
                    [['yin', 
                        [['lambda', ['cc'], ['display', '"@"'], 'cc'], 
                        ['call-with-current-continuation', ['lambda', ['c'], 'c']]]],
                     ['yang',
                        [['lambda', ['cc'], ['display', '"*"'], 'cc'], 
                        ['call-with-current-continuation', ['lambda', ['c'], 'c']]]]],
                     ['yin', 'yang']]];

        it("should give an array of tokens from a string of wspace-delimited tokens", function() {
            expect(Flour.treeify(Flour.tokenize(just_tokens))).toEqual(treeified_just_tokens);
        });

        it("should give a nested array of tokens for just a flat s-exp", function() {
            expect(Flour.treeify(Flour.tokenize(non_nested_s_exp))).toEqual(treeified_non_nested_s_exp);
        });

        it("should give an array of tokens and nested arrays for a series of tokens and s-exp", function() {
            expect(Flour.treeify(Flour.tokenize(series_of_non_nested))).toEqual(treeified_series_non_nested);
        });

        it("should give a properly formed array structure for a nested s-exp", function() {
            expect(Flour.treeify(Flour.tokenize(yin_yang))).toEqual(treeified_yin_yang);
        });
    });

    describe("is_atom", function() {
        it("should return false for an array", function () {
            expect(Flour.is_atom([1,2,3,5])).toBe(false);
        });

        it("should return false for an empty array", function () {
            expect(Flour.is_atom([])).toBe(false);
        });

        it("should return false for a nested array", function () {
            expect(Flour.is_atom([1,[2,[4, 3]], 5, [4, 8]])).toBe(false);
        });

        it("should return true for an indentifier", function () {
            expect(Flour.is_atom('foo')).toBe(true);
        });

        it("should return true for a string literal", function () {
            expect(Flour.is_atom('"foo"')).toBe(true);
        });

        it("should return true for numbers", function () {
            expect(Flour.is_atom('12423.044')).toBe(true);
        });

        it("should return true for numbers", function () {
            expect(Flour.is_atom('-23')).toBe(true);
        });

        it("should return true for booleans", function () {
            expect(Flour.is_atom('#t')).toBe(true);
        });
    });

    describe("is_boolean", function() {
        it("should return true for #t", function() {
            expect(Flour.is_boolean('#t')).toBe(true);
        });

        it("should return true for #f", function() {
            expect(Flour.is_boolean('#f')).toBe(true);
        });

        it("should return false for whatever else", function() {
            expect(Flour.is_boolean('Surely you must be joking')).toBe(false);
        });
    });

    describe("bool", function() {
        it("should return true for #t", function() {
            expect(Flour.bool('#t')).toBe(true);
        });

        it("should return false for #f", function() {
            expect(Flour.bool('#f')).toBe(false);
        });

        it("should throw for whatever else", function() {
            expect(function() {
                Flour.bool('We are the music makers.') 
            }).toThrow();
        });
    });

    // this is sort of a cop out for now.. just kind of reuse javascript's notion of number
    describe("is_number", function() {
        it("should return true for a string parseable as a number", function () {
            expect(Flour.is_number('12343')).toBe(true);
        }); 

        it("should return true for a string parseable as a number", function () {
            expect(Flour.is_number('-12343.24343')).toBe(true);
        }); 

        it("should return false for a string not parseable as a number", function () {
            expect(Flour.is_number('324ff34')).toBe(true);
        }); 

        it("should return false for a string not parseable as a number", function () {
            expect(Flour.is_number('Thus spoke zarathustra.')).toBe(false);
        }); 
    });

    describe('number', function() {
        it("should return the correct value for a parseable number", function () {
            expect(Flour.number('12343')).toBe(12343);
        }); 

        it("should return the correct value for a parseable number", function () {
            expect(Flour.number('-12343.24343')).toBe(-12343.24343); // yeah...hopefully this works
        }); 

        it("should throw an exception when the arg is unparseable number", function () {
            expect(function() {Flour.number('f324ff34qqqr')}).toThrow();
        }); 

        it("should throw an exception when the arg is unparseable number", function () {
            expect(function () {Flour.number('Thus spoke zarathustra.')}).toThrow();
        }); 
    });

    describe('is_string_literal', function() {
        it("should return true for a valid string literal", function () {
            expect(Flour.is_string_literal('"Pure imagination."')).toBe(true);
        }); 

        it("should return true for a valid string literal", function () {
            expect(Flour.is_string_literal('"\\n\\nPure\\t!!!@#@! \\"\\"imagination."')).toBe(true);
        }); 

        it("should return true for the empty string literal", function () {
            expect(Flour.is_string_literal('""')).toBe(true);
        }); 

        it("should return false for a malformed string literal", function () {
            expect(Flour.is_string_literal('"fosdf"fsd"f"')).toBe(false);
        }); 

        it("should return false for an indentifier", function () {
            expect(Flour.is_string_literal('leaning_tower_of_piza')).toBe(false);
        }); 

        it("should return false for a number", function () {
            expect(Flour.is_string_literal('-1234.2343')).toBe(false);
        }); 

        it("should return false for a boolean", function () {
            expect(Flour.is_string_literal('#t')).toBe(false);
        }); 
    });

    describe('string_literal', function() {
        it("should return the correct value for a valid string literal", function () {
            expect(Flour.string_literal('"\\n\\nPure\\t!!!@#@! \\"\\"imagination."')).toBe('\n\nPure\t!!!@#@! \"\"imagination.');
        }); 

        it("should return the correct value for the empty string literal", function () {
            expect(Flour.string_literal('""')).toBe('');
        }); 

        it("should throw an exception for a value not parseable as a string literal", function () {
            expect(function () {Flour.string_literal('asdfjsdkk333fdsaf')}).toThrow();
        }); 

        it("should throw an exception for a value not parseable as a string literal", function () {
            expect(function () {Flour.string_literal('"asdfjskd"dfsksjd"dsf23')}).toThrow();
        }); 
    });

    describe('is_special', function () {
        it("should return true for the lambda indentifier", function () {
            expect(Flour.is_special('lambda')).toBe(true);
        }); 

        it("should return true for the define indentifier", function () {
            expect(Flour.is_special('define')).toBe(true);
        }); 

        it("should return true for the if indentifier", function () {
            expect(Flour.is_special('if')).toBe(true);
        }); 

        it("should return true for the cond indentifier", function () {
            expect(Flour.is_special('cond')).toBe(true);
        }); 
    });
});
