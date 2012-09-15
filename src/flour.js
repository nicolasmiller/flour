var Flour = (function() {
    var global = {};

    function eval_atom(atom) {
        if(typeof atom === 'number') {
            return atom;
        }
    };

    function parse(text) {
        var whitespace = new RegExp("\\s+"),
            parens = new RegExp('(\\(|\\))');
            split_on_wspace =  text.split(whitespace),
            non_empty = function (s) {
                return s.length > 0;
            },
            i = 0,
            tokens = [];

        for( ; i < split_on_wspace.length; i++) {
            tokens = tokens.concat(split_on_wspace[i].split(parens).filter(non_empty));
        }

        return treeify(tokens);
    }

    function treeify(tokens) {
        tree = [];
        for(i = 0; i < tokens.length; i++) {
            console.log(tokens[i]);
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
                        subtree = tokens[i];
                    }
                    i++;
                }
                tree = tree.concat(treeify(subtree)); 
            }
            else {
                tree = tree.concat(tokens[i]);
            }
        }
        return tree;
    }

    var exported = {
        eval: function(text) {
            return parse(text);
        },
        treeify: treeify
    };

    return exported;
})();
