Array.prototype.sum = function() {
    return this.reduce((p,n) => +p + +n);
}

Array.prototype.product = function() {
    return this.reduce((p,n) => +p * +n);
}

Array.prototype.without = function(other) {
    return this.filter(c => !Array.from(other).includes(c));
}

Array.prototype.intersect = function(other) {
    return this.filter(c => Array.from(other).includes(c));
}

String.prototype.sort = function() {
    return this.split('').sort().join('');
}

String.prototype.without = function(other) {
    return this.split('').without(other);
}

String.prototype.intersect = function(other) {
    return this.split('').intersect(other);
}

String.prototype.repeat = function(n) {
    if (n <= 0)
        return "";
    else
        return this + this.repeat(n - 1);
}

// This works for _most_ objects
Object.prototype.clone = function() { return JSON.parse(JSON.stringify(this)); };

Object.prototype.gets = function(x, y, defaultValue) { return this[`${x}_${y}`] === undefined ? defaultValue : this[`${x}_${y}`]; };
Object.prototype.sets = function(x, y, n) { this[`${x}_${y}`] = n; };

