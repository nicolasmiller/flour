var Flour = (function() {
    var global = {};

    function is_atom(blob) {
        return !Array.isArray(blob);
    }
    
    function eval_atom(atom) {
        if(is_number(atom)) {
            return number(atom);
        }
        else if(is_string_literal(atom)) {
            return string_literal(atom);
        }
        else if(is_boolean(atom)) {
            return bool(atom);
        }
        else if(is_syntax(atom)) {
            eval_syntax(atom);
        }
        else {
            eval_identifier(atom);
        }
    };

    function map_in_place(arr, func) {
        var i = 0;
        for( ; i < arr.length; i++) {
            arr[i] = func(arr[i]);
        }
    }

    function eval(tree) {
        map_in_place(tree, function() {
            if(is_atom(tree[i])) {
                return eval_atom(tree[i]);
            }
            else {
                return apply(tree[i]);
            }
        });
    }; 

    function apply(s_exp) {
        map_in_place(s_exp, function() {
            return eval(s_exp[i]);
        });
        s_exp[0].apply(null, s_exp.slice(1)); 
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

    function is_special(atom) {
        var keywords = { 
            'define': true,
            'lambda': true,
            'if': true,
            'cond': true    
        };
        return keywords[atom] !== undefined;
    }

    function tokenize(text) {
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
            i = 0;
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
        is_special: is_special
    };

    return exported;
})();
