describe("Flour", function() {
    var just_tokens = "   foo\nbar   quux\tbuzz";
    var non_nested_s_exp = "(define foo 42)";
    var series_of_non_nested = "(define foo 34)" 
        + "\n(define bar 11)"
        + "\nfoo"
        + "\nbar"
        + "\n(+ foo bar)";

    var arithmetic_expression = '(* 2 (+ 12 (/ 100 2 (* 2 1)) (- 52 (+ 1 1 23))))';

    var yin_yang = '(let* ((yin'
        + '\n((lambda (cc) (display "@") cc) (call-with-current-continuation (lambda (c) c))))'
        + '\n(yang'
        + '\n((lambda (cc) (display "*") cc) (call-with-current-continuation (lambda (c) c)))))'
        + '\n(yin yang))';

    describe("tokenize", function() {
        var tokenized_just_tokens = ['foo', 'bar', 'quux', 'buzz'];
        var tokenized_non_nested_s_exp = ['(', 'define', 'foo', '42', ')'];

        it("returns an array of tokens from a string of wspace-delimited tokens", function() {
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

        it("returns an array of tokens from a string of wspace-delimited tokens", function() {
            expect(Flour.treeify(Flour.tokenize(just_tokens))).toEqual(treeified_just_tokens);
        });

        it("returns a nested array of tokens for a flat s-exp", function() {
            expect(Flour.treeify(Flour.tokenize(non_nested_s_exp))).toEqual(treeified_non_nested_s_exp);
        });

        it("returns an array of tokens and nested arrays for a series of tokens and s-expressions", function() {
            expect(Flour.treeify(Flour.tokenize(series_of_non_nested))).toEqual(treeified_series_non_nested);
        });

        it("returns a properly nested array structure for a nested s-exp", function() {
            expect(Flour.treeify(Flour.tokenize(yin_yang))).toEqual(treeified_yin_yang);
        });
    });

    describe("is_atom", function() {
        it("returns false for an array", function () {
            expect(Flour.is_atom([1,2,3,5])).toBe(false);
        });

        it("returns false for an empty array", function () {
            expect(Flour.is_atom([])).toBe(false);
        });

        it("returns false for a nested array", function () {
            expect(Flour.is_atom([1,[2,[4, 3]], 5, [4, 8]])).toBe(false);
        });

        it("returns true for an indentifier", function () {
            expect(Flour.is_atom('foo')).toBe(true);
        });

        it("returns true for a string literal", function () {
            expect(Flour.is_atom('"foo"')).toBe(true);
        });

        it("returns true for numbers", function () {
            expect(Flour.is_atom('12423.044')).toBe(true);
        });

        it("returns true for numbers", function () {
            expect(Flour.is_atom('-23')).toBe(true);
        });

        it("returns true for booleans", function () {
            expect(Flour.is_atom('#t')).toBe(true);
        });
    });

    describe("is_boolean", function() {
        it("returns true for #t", function() {
            expect(Flour.is_boolean('#t')).toBe(true);
        });

        it("returns true for #f", function() {
            expect(Flour.is_boolean('#f')).toBe(true);
        });

        it("returns false otherwise", function() {
            expect(Flour.is_boolean('Surely you must be joking')).toBe(false);
        });
    });

    describe("bool", function() {
        it("returns true for #t", function() {
            expect(Flour.bool('#t')).toBe(true);
        });

        it("returns false for #f", function() {
            expect(Flour.bool('#f')).toBe(false);
        });

        it("throws an exception otherwise", function() {
            expect(function() {
                Flour.bool('We are the music makers.') 
            }).toThrow();
        });
    });

    // this is sort of a cop out for now.. just kind of reuse javascript's notion of number
    describe("is_number", function() {
        it("returns true for a string parseable as a number", function () {
            expect(Flour.is_number('12343')).toBe(true);
        }); 

        it("returns true for a string parseable as a number", function () {
            expect(Flour.is_number('-12343.24343')).toBe(true);
        }); 

        it("returns false for a string not parseable as a number", function () {
            expect(Flour.is_number('324ff34')).toBe(true);
        }); 

        it("returns false for a string not parseable as a number", function () {
            expect(Flour.is_number('Thus spoke zarathustra.')).toBe(false);
        }); 
    });

    describe('number', function() {
        it("returns the correct value for a parseable number", function () {
            expect(Flour.number('12343')).toBe(12343);
        }); 

        it("returns the correct value for a parseable number", function () {
            expect(Flour.number('-12343.24343')).toBe(-12343.24343); // yeah...hopefully this works
        }); 

        it("throws an exception when the arg is unparseable as a number", function () {
            expect(function() {Flour.number('f324ff34qqqr')}).toThrow();
        }); 

        it("throws an exception when the arg is unparseable as a number", function () {
            expect(function () {Flour.number('Thus spoke zarathustra.')}).toThrow();
        }); 
    });

    describe('is_string_literal', function() {
        it("returns true for a valid string literal", function () {
            expect(Flour.is_string_literal('"Pure imagination."')).toBe(true);
        }); 

        it("returns true for a valid string literal", function () {
            expect(Flour.is_string_literal('"\\n\\nPure\\t!!!@#@! \\"\\"imagination."')).toBe(true);
        }); 

        it("returns true for the empty string literal", function () {
            expect(Flour.is_string_literal('""')).toBe(true);
        }); 

        // note to self, this is a copout for now
        // racket for example handles strings
        it("returns false for a malformed string literal", function () {
            expect(Flour.is_string_literal('"fosdf"fsd"f"')).toBe(false);
        }); 

        it("returns false for an indentifier", function () {
            expect(Flour.is_string_literal('leaning_tower_of_piza')).toBe(false);
        }); 

        it("returns false for a number", function () {
            expect(Flour.is_string_literal('-1234.2343')).toBe(false);
        }); 

        it("returns false for a boolean", function () {
            expect(Flour.is_string_literal('#t')).toBe(false);
        }); 
    });

    describe('string_literal', function() {
        it("returns the correct value for a valid string literal", function () {
            expect(Flour.string_literal('"\\n\\nPure\\t!!!@#@! \\"\\"imagination."')).toBe('\n\nPure\t!!!@#@! \"\"imagination.');
        }); 

        it("returns the correct value for the empty string literal", function () {
            expect(Flour.string_literal('""')).toBe('');
        }); 

        it("throws an exception for a value not parseable as a string literal", function () {
            expect(function () {Flour.string_literal('asdfjsdkk333fdsaf')}).toThrow();
        }); 

        it("throws an exception for a value not parseable as a string literal", function () {
            expect(function () {Flour.string_literal('"asdfjskd"dfsksjd"dsf23')}).toThrow();
        }); 
    });

    describe('is_special', function() {
        it("returns true for the lambda indentifier", function () {
            expect(Flour.is_special('lambda')).toBe(true);
        }); 

        it("returns true for the define indentifier", function () {
            expect(Flour.is_special('define')).toBe(true);
        }); 

        it("returns true for the if indentifier", function () {
            expect(Flour.is_special('if')).toBe(true);
        }); 

        it("returns true for the cond indentifier", function () {
            expect(Flour.is_special('cond')).toBe(true);
        }); 
    });

    describe('map_in_place', function() {
        it("applies a function to each argument of an array in place", function () {
            var data = [0, 1, 2, 3, 4];
            Flour.map_in_place(data, function(item) {
                return item + 1;
            });
            expect(data).toEqual([1, 2, 3, 4, 5]);
        }); 

        it("does nothing to an empty array", function () {
            var data = [];
            Flour.map_in_place(data, function(item) {
                return item + 1;
            });
            expect(data).toEqual([]);
        }); 

        it("throws an exception for an argument with an undefined .length", function () {
            expect(function() {Flour.map_in_place(42, function(item) {
                return item + 1;
            })}).toThrow();
        }); 
    });

    describe('eval_atom', function() {
        it("returns an integer's value for an integer", function () {
            expect(Flour.eval_atom('42')).toBe(42);
        });

        it("returns a float's value for a float", function () {
            expect(Flour.eval_atom('-42.14159')).toBe(-42.14159);
        });

        it("returns a string value for a string literal", function () {
            // think about escape chars...
            expect(Flour.eval_atom('"Do androids dream of electric sheep?"'))
            .toBe('Do androids dream of electric sheep?');
        });

        it("returns a boolean value for a boolean literal", function () {
            expect(Flour.eval_atom('#t')).toBe(true);
            expect(Flour.eval_atom('#f')).toBe(false);
        });
        
        it("returns a function for a special", function () {
            expect(typeof Flour.eval_atom('lambda')).toBe('function');
            expect(typeof Flour.eval_atom('define')).toBe('function');
            expect(typeof Flour.eval_atom('if')).toBe('function');
            expect(typeof Flour.eval_atom('cond')).toBe('function');
        });

        it("returns a value for an identifier", function () {
            expect(typeof Flour.eval_atom('+')).toBe('function');
            expect(Flour.eval_atom('foo')).toBe(true);
        });

        it("throws an exception for an undefined identifier", function () {
            expect(function() {Flour.eval_atom('replicant')}).toThrow();
        });
    });

    describe('f_eval', function() {
        var exp = ['*', '2', ['+', '12', [ '/', '100', '2', ['*', '2', '1']], ['-', '52', ['+', '1', '1', '23']]]];

        describe('arithmetic', function() {
            describe('+', function() {
                it("returns 0 for no args", function () {
                    expect(Flour.f_eval(['+'])).toBe(0);
                });

                it("supports n args", function () {
                    expect(Flour.f_eval(['+', '10', '20', '30', '50', '60', '100'])).toBe(270);
                });

                it("supports negative literals", function () {
                    expect(Flour.f_eval(['+', '10', '-10'])).toBe(0);
                });
            });

            describe('*', function() {
                it("returns 1 for no args", function () {
                    expect(Flour.f_eval(['*'])).toBe(1);
                });

                it("supports n args", function () {
                    expect(Flour.f_eval(['*', '2', '2', '2', '2', '2'])).toBe(32);
                });

                it("returns arg * 1 for one arg and supports negative literals", function () {
                    expect(Flour.f_eval(['*', '-10'])).toBe(-10);
                });
            });

            describe('-', function() {
                it("requires at least one argument", function () {
                    expect(function() {Flour.f_eval(['-'])}).toThrow();;
                });

                it("negates a single argument", function () {
                    expect(Flour.f_eval(['-', '100'])).toBe(-100);
                });

                it("supports n args", function () {
                    expect(Flour.f_eval(['-', '100', '20', '30', '10', '40'])).toBe(0);
                });

                it("supports negative literals", function () {
                    expect(Flour.f_eval(['-', '10', '-10'])).toBe(20);
                });
            });

            describe('/', function() {
                it("requires at least one argument", function () {
                    expect(function() {Flour.f_eval(['/'])}).toThrow();
                });
                
                it("returns the reciprocal of a single arg ", function () {
                    expect(Flour.f_eval(['/', '100'])).toBe(0.01);
                });

                it("supports n args", function () {
                    expect(Flour.f_eval(['/', '100', '2', '2', '5'])).toBe(5);
                });
            });

            describe('zero?', function() {
                it("returns true for 0", function () {
                    expect(Flour.f_eval(['zero?', '0'])).toBe(true);
                });

                it("returns true for 0.0", function () {
                    expect(Flour.f_eval(['zero?', '0.0'])).toBe(true);
                });

                it("returns false for a non-zero arg", function () {
                    expect(Flour.f_eval(['zero?', '42'])).toBe(false);
                });
            });

            describe('odd?', function() {
                it("returns true for odd arg", function () {
                    expect(Flour.f_eval(['odd?', '1'])).toBe(true);
                });

                it("returns false for even arg", function () {
                    expect(Flour.f_eval(['odd?', '2'])).toBe(false);
                });
            });

            describe('even?', function() {
                it("returns true for even arg", function () {
                    expect(Flour.f_eval(['even?', '2'])).toBe(true);
                });

                it("returns false for odd arg", function () {
                    expect(Flour.f_eval(['even?', '1'])).toBe(false);
                });
            });

            describe('max', function() {
                it("returns max of n args", function () {
                    expect(Flour.f_eval(['max', '-2', '0', '42', '5'])).toBe(42);
                });
            });

            describe('min', function() {
                it("returns min of n args", function () {
                    expect(Flour.f_eval(['min', '-2', '0', '42', '5'])).toBe(-2);
                });
            });

            describe('<', function() {
                it("throws with less than 2 args", function () {
                    expect(function() {Flour.f_eval(['<', '0'])}).toThrow();
                });

                it("returns true if first arg is less than rest of args", function () {
                    expect(Flour.f_eval(['<', '0', '1', '2', '3', '4'])).toBe(true);
                });

                it("returns false if first arg is not less than rest of args", function () {
                    expect(Flour.f_eval(['<', '42', '40', '42', '23', '12'])).toBe(false);
                });
            });

            describe('<=', function() {
                it("throws with less than 2 args", function () {
                    expect(function() {Flour.f_eval(['<=', '0'])}).toThrow();
                });

                it("returns true if first arg is less than or equal to rest of args", function () {
                    expect(Flour.f_eval(['<=', '0', '1', '0', '3', '4'])).toBe(true);
                });

                it("returns false if first arg is not less than or equal to  rest of args", function () {
                    expect(Flour.f_eval(['<=', '42', '40', '42', '74', '12'])).toBe(false);
                });
            });

/* having a weird problem with the > symbol
            describe('>', function() {
                it("throws with less than 2 args", function () {
                    expect(function() {Flour.f_eval(['>', '0'])}).toThrow();
                });

                it("returns true if first arg is greater than rest of args", function () {
                    expect(Flour.f_eval(['>', '100', '1', '0', '3', '4'])).toBe(true);
                });

                it("returns false if first arg is not greater than to  rest of args", function () {
                    expect(Flour.f_eval(['<=', '42', '40', '42', '74', '12'])).toBe(false);
                });
            });
            */

            describe('>=', function() {
                it("throws with less than 2 args", function () {
                    expect(function() {Flour.f_eval(['>=', '0'])}).toThrow();
                });

                it("returns true if first arg is greater than or equal to rest of args", function () {
                    expect(Flour.f_eval(['>=', '100', '1', '100', '3', '4'])).toBe(true);
                });

                it("returns false if first arg is not greater than or equal to rest of args", function () {
                    expect(Flour.f_eval(['>=', '42', '40', '42', '74', '12'])).toBe(false);
                });
            });

            it("supports nested arithmetic functions", function () {
                expect(Flour.f_eval(exp)).toBe(128);
            });
        });

        describe('special forms', function() {
            describe('if', function() {
                it("if cond true, evaluates true clause", function () {
                    expect(Flour.f_eval(['if', '#t', '1', '0'])).toBe(1);
                });

                it("if cond false, evaluates true clause", function () {
                    expect(Flour.f_eval(['if', '#f', '1', '0'])).toBe(0);
                });
            });

            describe('quote', function() {
                it("returns an unevaluated representation of the argument", function () {
                    expect(Flour.f_eval(['quote', '42'])).toBe('42');
                    expect(Flour.f_eval(['quote', '#t'])).toBe('#t');
                    expect(Flour.f_eval(['quote', '"foo"'])).toBe('"foo"');
                    expect(Flour.f_eval(['quote', ['lambda', ['x'], ['+', 'x', '1']]]))
                        .toEqual(['lambda', ['x'], ['+', 'x', '1']]);
                });
            });
        });

        describe('types', function() {
            describe('boolean?', function() {
                it("returns true for a boolean argument", function () {
                    expect(Flour.f_eval(['boolean?', '#t'])).toBe(true);
                    expect(Flour.f_eval(['boolean?', '#f'])).toBe(true);
                });

                it("returns false for a non-boolean argument", function () {
                    expect(Flour.f_eval(['boolean?', '1234'])).toBe(false);
                });
            });

            describe('symbol?', function() {
                it("returns true for a symbol argument", function () {
                });

                it("returns false for a non-symbol argument", function () {
                });
            });

            describe('char?', function() {
                it("returns true for a char argument", function () {
                });

                it("returns false for a non-char argument", function () {
                });
            });

            describe('vector?', function() {
                it("returns true for a vector argument", function () {
                });

                it("returns false for a non-vector argument", function () {
                });
            });

            describe('procedure?', function() {
                it("returns true for a procedure argument", function () {
                    expect(Flour.f_eval(['procedure?', 'boolean?'])).toBe(true);
                    expect(Flour.f_eval(['procedure?', 'procedure?'])).toBe(true); // nerd giggle
                    expect(Flour.f_eval(['procedure?', ['lambda', ['x']]])).toBe(true);
                });

                it("returns false for a non-procedure argument", function () {
                    expect(Flour.f_eval(['procedure?', '#t'])).toBe(false);
                    expect(Flour.f_eval(['procedure?', '1234'])).toBe(false);
                });
            });

            describe('pair?', function() {
                it("returns true for a pair argument", function () {
                    expect(Flour.f_eval(['pair?', ['cons', '1', '2']])).toBe(true);
                });

                it("returns true for a list argument", function () {
                    expect(Flour.f_eval(['pair?', ['list', '1', '2', '3', '4']])).toBe(true);
                    expect(Flour.f_eval(['pair?', ['quote', ['1', '2', '3', '4']]])).toBe(true);
                });
                
                describe('list', function() {
                    it('returns a list composed of its arguments evaluated', function() {
                        expect(Flour.f_eval(['list', '1', '2', '3', '4'])).toEqual([1, 2, 3, 4]);
                    });
                });

                describe('cons', function() {
                    it('for two atom args it returns the corresponding pair', function() {
                        expect(Flour.f_eval(['cons', '1', '2'])).toEqual({left: 1, right:2});
                        expect(Flour.f_eval(['pair?', ['cons', '1', '2']])).toEqual(true);
                    });

                    it('appends an atom onto the front of a list', function() {
                        expect(Flour.f_eval(['cons', '0', ['list', '1', '2']])).toEqual([0, 1, 2]);
                        expect(Flour.f_eval(['pair?', ['cons', '0', ['list', '1', '2']]])).toBe(true);
                    });

                    it('appends a list onto the front of a list', function() {
                        expect(Flour.f_eval(['cons', ['list', '1', '2'], ['list', '1', '2']])).toEqual([[1, 2], 1, 2]);
                    });

                    it('throws an exception for not exactly two arguments', function() {
                        expect(function() { Flour.f_eval(['cons'])}).toThrow();
                        //expect(function() { Flour.f_eval(['cons', '1', '2', '3'])}).toThrow();
                    });
                });
                
                describe('car', function() {
                    it('returns the left element of a pair', function() {
                        expect(Flour.f_eval(['car', ['cons', '1', '2']])).toBe(1);
                    });

                    it('returns the first element of a list', function() {
                        expect(Flour.f_eval(['car', ['list', '1', '2', '3', '4']])).toBe(1);
                    });

                    it('throws an exception for a non-list argument', function() {
                        expect(function() { Flour.f_eval(['car', '1'])}).toThrow();
                    });

                    it('throws an exception for not exactly one argument', function() {
                        expect(function() { Flour.f_eval(['car'])}).toThrow();
                        expect(function() { Flour.f_eval(['car', '1', '2', '3'])}).toThrow();
                    });
                });

                describe('cdr', function() {
                    // got a problem here:
                    // need to clarify the distinction between proper pairs and lists
                    it('returns the right element of a pair', function() {
                        expect(Flour.f_eval(['cdr', ['cons', '1', '2']])).toBe(2);
                    });

                    it('returns the rest of the list (a list without the first element)', function() {
                        expect(Flour.f_eval(['cdr', ['list', '1', '2', '3', '4']])).toEqual([2, 3, 4]);
                    });

                    it('throws an exception for a non-list argument', function() {
                        expect(function() { Flour.f_eval(['cdr', '1'])}).toThrow();
                    });

                    it('throws an exception for not exactly one argument', function() {
                        expect(function() { Flour.f_eval(['cdr'])}).toThrow();
                        expect(function() { Flour.f_eval(['cdr', '1', '2', '3'])}).toThrow();
                    });
                });

                it("returns false for a non-pair argument", function () {

                });
            });

            describe('number?', function() {
                it("returns true for a number argument", function () {
                });

                it("returns false for a non-number argument", function () {
                });
            });

            describe('string?', function() {
                it("returns true for a string argument", function () {
                });

                it("returns false for a non-string argument", function () {
                });
            });

            describe('port?', function() {
                it("returns true for a port argument", function () {
                });

                it("returns false for a non-port argument", function () {
                });
            });
        });
    });

    describe('eval', function() {
        /*
        it("evaluates an arithmetic scheme expression", function () {
            expect(Flour.eval(arithmetic_expression)).toBe(128);
        });
        */
    });
});
