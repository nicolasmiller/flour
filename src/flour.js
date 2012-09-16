var Flour = (function() {
    var global_env = {
        // define some builtins here...
        'version': 'ALL_TOO_ALPHA',
        'global_env': true,
        'foo': true,
        '+' : function () {
            var result = 0,
                i = 0;
            for( ; i < arguments.length; i++) {
                if(typeof arguments[i] !== 'number') {
                    throw "Non-numeric argument";
                }
                result += arguments[i];
            }
            return result;
        } 
    };

    function is_atom(blob) {
        return !Array.isArray(blob);
    }

    function is_null(blob) {
        return blob === [];
    }

    function is_special(atom) {
        return eval_special(atom) !== undefined;
    }

    function eval_special(syntax) {
        var keyword_defs = { 
            'define': function (name, value) {
                if(global_env[name] === undefined) {
                    global_env[name] = value;
                }
                else {
                    throw name + " cannot be redefined";
                }
             },
            'lambda': function (args, body) {
             },
            'if': function(bool, then_clause, else_clase) {
            },
            'cond': function(list_of_cases) {
            }   
        };
        return keyword_defs[syntax];
    }

    function is_special(atom) {
        return eval_special(atom) !== undefined;
    }

    function eval_identifier(identifier) {
        // yeah this needs to be fleshed out.. 
        return global_env[identifier];
    }

    function eval_atom(atom) {
        if(is_number(atom)) {
            console.log('is number');
            return number(atom);
        }
        else if(is_string_literal(atom)) {
            console.log('is string literal');
            return string_literal(atom);
        }
        else if(is_boolean(atom)) {
            console.log('is boolean');
            return bool(atom);
        }
        else if(is_special(atom)) {
            console.log('is special');
            return eval_special(atom);
        }
        else {
            // think about how indentifier will be resolved in nested scopes
            console.log('is identifier');
            var result = eval_identifier(atom);
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

    function f_eval(tree) {
        var result;
        map_in_place(tree, function(item) {
            if(is_atom(item)) {
                result = eval_atom(item);
                return result;
            }
            else {
                return f_eval(item);
            }
        });
        return f_apply(tree);
    } 

    function f_apply(s_exp) {
        console.log(typeof s_exp[0]);
        console.log(s_exp);

        console.log(typeof s_exp.slice(1));
        console.log(s_exp.slice(1));

        return s_exp[0].apply(null, s_exp.slice(1)); 
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

    var exported = {
        eval: function(text) {
            return treeify(tokenize(text));
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
