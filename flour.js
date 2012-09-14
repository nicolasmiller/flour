var Flour = (function() {
    var global = {};
    var 

    function eval_atom(atom) {
        if(typeof atom === 'number') {
            return atom;
        }
    };

    function tokenize(text) {
        var whitespace = new RegExp("\\s+"),
            parens = new RegExp('(\\(|\\))');
            tokens =  text.split(whitespace),
            non_empty = function (s) {
                return s.length > 0;
            },
            i = 0,
            result = [];

        for( ;i < tokens.length; i++) {
            result = result.concat(tokens[i].split(parens).filter(non_empty));
        }

        return result;
    }

    var exported = {
        eval: function(text) {
            return tokenize(text).toString();
        }
    };

    return exported;
})();
