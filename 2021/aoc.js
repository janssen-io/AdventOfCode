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

String.prototype.red = function() { return this.colour(31); }
String.prototype.green = function() { return this.colour(32); }
String.prototype.yellow = function() { return this.colour(33); }

String.prototype.colour = function(colourCode) {
    return `\x1b[${colourCode}m${this}\x1b[0m`;
}

// This works for _most_ objects
Object.prototype.clone = function() { return JSON.parse(JSON.stringify(this)); };

Object.prototype.getCell = function(x, y, defaultValue) {
    const storedValue = this[`${x}_${y}`];
    // Check for undefined explicitly, because empty string and 0 are also falsey.
    if (storedValue === undefined) {
        return defaultValue;
    }
    return storedValue;
};
Object.prototype.setCell = function(x, y, n) { this[`${x}_${y}`] = n; };

