var Environment = function() {
    function set(name, value) {
        this[name] = value;
    }
    function get(name) {
        return this[name];
    }

    return {
        set: set,
        get: get
    };
};

var Environment2 = function() {
    function set(name, value) {
        name = value;
    }
    function get(name) {
        return name;
    }

    return {
        set: set,
        get: get
    };
};

var Environment3 = function() {};

Environment3.prototype.set = function(name, value) {
    this[name]= value;
}

Environment3.prototype.get = function(name) {
    return this[name];
}
