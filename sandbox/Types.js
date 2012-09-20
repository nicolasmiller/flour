var list = function() {
    var i = 0;
    this.elems = null;
    this.tail = null;
    for( ; i < arguments.length; i++) {
        if(this.elems === null) {
            this.elems = new Pair(arguments[i], null);
            this.tail = this.elems;
        }
        else {
            this.elems.right = new Pair(arguments[i], null);
            this.elems = this.elems.right
        }
    }

    return tail;
};

var Pair = function(left, right) {
    this.left = left;
    this.right = right;
    this.car = function() {
        return this.left;
    };
    this.cdr = function() {
        return this.right;
    };
};

var F_Boolean = function(value) {
    this.value = value;
};
