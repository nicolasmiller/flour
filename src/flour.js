var Flour = (function() {
    function is_unary_numeric(args) {
        if(args.length > 1 && args.length != 0) {
            throw "Expected one argument";
        }
        if(typeof args[0] !== 'number') {
            throw "Numeric argument required";
        }
    }

    function die_if_non_numeric(x) {
        if(typeof x !== 'number') {
            throw "Non-numeric argument";
        }
    }

    function die_if_zero_args(args) {
        if(args.length < 1) {
            throw "Requires at least one argument";
        }
    }
    
    function die_if_atom(arg) {
        if(is_atom(arg)) {
            throw "Expected non-atom argument";
        }
    }

    function require_at_least(n, args) {
        if(args.length < n) {
            throw "Requires at least " + n + " argument(s)";
        }
    }

    function require_exactly(n, args) {
        if(args.length !== n) {
            throw "Requires at least " + n + " argument(s)";
        }
    }

    var data = {
        'cons': function() {
            require_exactly(2, arguments);
            var left = arguments[0],
                right = arguments[1];

            if(is_atom(right)) {
                return new Pair(left, right);
            }
            else {
                var head = [left];
                if(right instanceof Pair) {
                    return head.concat([right]);
                }
                return head.concat(right);
            }
         },
         'car': function() {
            require_exactly(1, arguments);
            die_if_atom(arguments[0]);
            if(arguments[0] instanceof Pair) {
                return arguments[0].left;
            }
            return arguments[0][0];
         },
         'cdr': function() {
            require_exactly(1, arguments);
            die_if_atom(arguments[0]);
            if(arguments[0] instanceof Pair) {
                return arguments[0].right;
            }
            return arguments[0].slice(1);
         }
    };

    var arithmetic = {
        '+': function () {
            var result = 0,
                i = 0;
            for( ; i < arguments.length; i++) {
                die_if_non_numeric(arguments[i]);
                result += arguments[i];
            }
            return result;
        }, 
        '-': function () {
            var result,
                i = 0;

            require_at_least(1, arguments);

            if(arguments.length === 1) {
                return -1 * arguments[0];
            }

            result = arguments[0];
            i++;

            for( ; i < arguments.length; i++) {
                die_if_non_numeric(arguments[i]);
                result -= arguments[i];
            }

            return result;
        },
        '*': function () {
            var result = 1,
                i = 0;

            for( ; i < arguments.length; i++) {
                die_if_non_numeric(arguments[i]);
                result *= arguments[i];
            }

            return result;
        },
        '/': function () {
            var i = 0,
                result = 1;

            require_at_least(1, arguments);

            if(arguments.length > 1) {
                result = arguments[i];
                i++;
            }

            for( ; i < arguments.length; i++) {
                die_if_non_numeric(arguments[i]);
                result /= arguments[i];
            }
            return result;
        },
        'zero?': function(x) {
            is_unary_numeric(arguments); 
            return x === 0;
        },
        'positive?': function(x) {
            is_unary_numeric(arguments); 
            return x > 0;
        },
        'odd?': function(x) {
            is_unary_numeric(arguments); 
            return (x % 2) !== 0;
        },
        'even?': function(x) {
            is_unary_numeric(arguments); 
            return (x % 2) === 0;
        },
        'max': function() {
            var i = 1,
                max = arguments[0];

            require_at_least(1, arguments);

            for( ; i < arguments.length; i++) {
                if(arguments[i] > max) {
                    max = arguments[i];
                }
            }
            return max;
         },
        'min': function() {
            var i = 1,
                min = arguments[0];

            require_at_least(1, arguments);

            for( ; i < arguments.length; i++) {
                if(arguments[i] < min) {
                    min = arguments[i];
                }
            }
            return min;
        },
        '<': function() {
            var first = arguments[0],
                i = 1;

            require_at_least(2, arguments);

            for( ; i < arguments.length; i++) {
                if(!(first < arguments[i])) {
                    return false;
                }  
            } 
            return true;
        },
        '<=': function() {
            var first = arguments[0],
                i = 1;

            require_at_least(2, arguments);

            for( ; i < arguments.length; i++) {
                if(!(first <= arguments[i])) {
                    return false;
                }  
            } 
            return true;
        },
        'gt': function() {
            //console.log('wtf');
            var first = arguments[0],
                i = 1;

            require_at_least(2, arguments);

            for( ; i < arguments.length; i++) {
                if(!(first > arguments[i])) {
                    return false;
                }  
            } 
            return true;
        },
        '>=': function() {
            var first = arguments[0],
                i = 1;

            require_at_least(2, arguments);

            for( ; i < arguments.length; i++) {
                if(!(first >= arguments[i])) {
                    return false;
                }  
            } 
            return true;
        },
        '=': function() {
            var first = arguments[0],
                i = 1;

            require_at_least(2, arguments);

            for( ; i < arguments.length; i++) {
                if(first !== arguments[i]) {
                    return false;
                }  
            } 
            return true;
        }
    };

    function List() {
        var list = [];
        var i = 0;
        for( ; i < arguments.length; i++) {
            list.push(arguments[i]);
        }
        return list;
    }

    function Pair(left, right) {
        this.left = left;
        this.right = right;
    } 

    function Symbol() {
    }

    function Char() {
    }

    function Vector() {
    }

    function has_constructor(constructor, args) {
	    require_exactly(1, args);
		return args[0].constructor === constructor;	
    }

    var type_predicates = {
		'boolean?': function() {
			return has_constructor(Boolean, arguments);
         },
        'symbol?': function() {
			return has_constructor(Symbol, arguments);
         },
        'char?': function() {
			return has_constructor(Char, arguments);
         },
        'vector?': function() {
			return has_constructor(Vector, arguments);
         },
        'procedure?': function() {
			return has_constructor(Function, arguments);
         },
        'pair?': function() {
			require_exactly(1, arguments);
			return !is_atom(arguments[0]) || (arguments[0] instanceof Pair); 
         },
         'non_list_pair?': function() {
             return has_constructor(Pair, arguments);
         },
        'number?': function() {
            return has_constructor(Number, arguments);
         }
    };

    var global_env = {
        'version': 'ALL_TOO_ALPHA',
        'foo': true,
        '+': arithmetic['+'],
        '-': arithmetic['-'],
        '*': arithmetic['*'],
        '/': arithmetic['/'],
        '<': arithmetic['<'],
        '<=': arithmetic['<='],
        'gt': arithmetic['gt'],
        '>=': arithmetic['>='],
        '=': arithmetic['='],
        'zero?': arithmetic['zero?'],
        'positive?': arithmetic['positive?'],
        'odd?': arithmetic['odd?'],
        'even?': arithmetic['even?'],
        'max': arithmetic['max'],
        'min': arithmetic['min'],
        'is_of_type': type_predicates['is_of_type'],
        'boolean?': type_predicates['boolean?'],
        'procedure?': type_predicates['procedure?'],
        'pair?': type_predicates['pair?'],
        'cons': data['cons'],
        'car': data['car'],
        'cdr': data['cdr'],
        'list': List
    };

    function is_atom(blob) {
        return !Array.isArray(blob) && !(blob instanceof Pair);
    }

    function is_null(blob) {
        return blob === [];
    }

    function is_special(atom) {
        return eval_special(atom) !== undefined;
    }

    function eval_special(syntax, local_env) {
        var keyword_defs = { 
            'define': function (name, value) {
                if(global_env[name] === undefined) {
                    global_env[name] = f_eval(value);
                }
                else {
                    throw name + " cannot be redefined";
                }
             },
            'lambda': function (args, body) {
                return function() {
                    var i = 0;
                    var local_env = {};
                    for( ; i < arguments.length; i++) {
                        local_env[args[i]] = arguments[i];
                    }
                    return f_eval(body, local_env);
                }
             },
            'if': function(bool, then_clause, else_clause) {
                var cond = f_eval(bool, local_env);

                // check to see what the spec allows in a conditional 
                if(!(cond === true || cond === false)) {
                    throw "Non-boolean conditional value: " + cond;
                }
                if(cond) {
                    return f_eval(then_clause, local_env);
                }
                else {
                    return f_eval(else_clause, local_env);
                }
            },
            'cond': function(list_of_cases) {
            },
            'quote': function() {
                require_exactly(1, arguments);
                return arguments[0];
            }   
        };
        return keyword_defs[syntax];
    }

    function is_special(atom) {
        return eval_special(atom) !== undefined;
    }

    function eval_identifier(identifier, local_env) {
        if(local_env !== undefined && local_env[identifier] !== undefined) {
            return local_env[identifier];
        }
        else {
            return global_env[identifier];
        }
    }

    function eval_atom(atom, local_env) {
        if(is_number(atom)) {
            return number(atom);
        }
        else if(is_string_literal(atom)) {
            return string_literal(atom);
        }
        else if(is_boolean(atom)) {
            return bool(atom);
        }
        else if(is_special(atom)) {
            return eval_special(atom, local_env);
        }
        else {
            var result = eval_identifier(atom, local_env);
            if(result === undefined) {
                throw atom + " is undefined";
            }
            return result;
        }
    };

    function map_in_place(arr, func) {
        var i = 0;

        if(arr.length === undefined) {
            throw "Argument length undefined"
        }

        for( ; i < arr.length; i++) {
            arr[i] = func(arr[i]);
        }
    }

    function map(arr, func) {
        var i = 0;
        var result = [];

        if(arr.length === undefined) {
            throw "Argument length undefined"
        }

        for( ; i < arr.length; i++) {
            result.push(func(arr[i]));
        }

        return result;
    }

    function f_eval(tree, local_env) {
        var result;
        var special = false;
        var evaled_tree;

        if(is_atom(tree)) {
            return eval_atom(tree, local_env);
        }

        evaled_tree = map(tree, function(item) {
            if(special) {
                return item; // don't immediately evaluate special form args
            }
            if(is_atom(item)) {
                special = is_special(item);
                result = eval_atom(item, local_env);
                return result;
            }
            else {
                return f_eval(item, local_env);
            }
        });

        if(typeof evaled_tree[0] === 'function') {
            return f_apply(evaled_tree, local_env);
        }
        else {
            return result;
        }
    } 

    function f_apply(s_exp, local_env) {
        return s_exp[0].apply(local_env || global_env, s_exp.slice(1)); 
    }

    function is_string_literal(atom) {
        try {
            string_literal(atom);
            return true;
        }
        catch(err) {
            return false;
        }
    }

    function string_literal(atom) {
        if(atom.length >= 2) {
            if(atom[0] === '"' 
                && atom[atom.length - 1] === '"') {
                    return eval(atom);
            }
        }
        throw "Atom not parseable as string literal";
    }

    function is_number(atom) {
        try {
            number(atom);
            return true;
        }
        catch(err) {
            return false;
        }
    }

    function number(atom) {
        var number = parseFloat(atom);
        if(isNaN(number)) {
            throw "Atom not parseable as number";
        }
        return number;
    }

    function is_boolean(atom) {
        return atom === '#t' || atom === '#f';
    }

    function bool(atom) {
        if(!is_boolean(atom)) {
            throw "Atom not parseable as boolean";
        }
        if(atom === '#t') {
            return true;
        }
        else {
            return false;
        }      
    }

    

    function tokenize(text) {
        // this is really limited and dependent on whitespace for now
        // racket apparently does something more clever.. look into it

        var whitespace = new RegExp("\\s+"),
            parens = new RegExp('(\\(|\\))'),
            split_on_wspace =  text.split(whitespace),
            non_empty = function (s) {
                return s.length > 0;
            },
            i = 0,
            tokens = [];

        for( ; i < split_on_wspace.length; i++) {
            tokens = tokens.concat(split_on_wspace[i].split(parens).filter(non_empty));
        }

        return tokens;
    }

    function treeify(tokens) {
        var tree = [],
            i = 0,
            opening = 0,
            subtree = [];

        for( ; i < tokens.length; i++) {
            if(tokens[i] === '(') {
                opening = 1;
                subtree = [];
                i++;
                while(opening > 0) {
                    if(tokens[i] === '(') {
                        opening++;
                    }
                    if(tokens[i] === ')') {
                        opening--;
                    }

                    if(opening > 0) {
                        subtree.push(tokens[i]);
                        i++;
                    }
                }
                tree.push(treeify(subtree)); 
            }
            else {
                tree.push(tokens[i]);
            }
        }
        return tree;
    }

    function repr(value) {
        var i = 0;
        if(!is_atom(value)) {
            if(value instanceof Pair) {
                return "(" + value.left + " . " + value.right + ")";
            }

            for( ; i < value.length; i++) {
                value[i] = repr(value[i]);    
            }
            return "(" + value.toString().split(',').join(', ') + ")";
        }
        else if(typeof value === 'string') {
            return '"' + value + '"';
        }
        return value;
    }

    var exported = {
        eval: function(text) {
            /*
            var i = 0;
            var list_of_exps = treeify(tokenize(text));
            console.log(list_of_exps);
            for( ; i < list_of_exps.length; i++) {
                // blah.. should this print?
                f_eval(list_of_exps[i]);
            }
            */
            try {
                return repr(f_eval(treeify(tokenize(text))[0]));
            }
            catch(err) {
                return err;
            }
        },
        treeify: treeify,
        tokenize: tokenize,
        is_atom: is_atom,
        eval_atom: eval_atom,
        is_string_literal: is_string_literal,
        string_literal: string_literal,
        is_number: is_number,
        number: number,
        is_boolean: is_boolean,
        bool: bool,
        is_special: is_special,
        map_in_place: map_in_place,
        f_eval: f_eval,
        f_apply: f_apply
    };

    return exported;
})();
