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
    if(isNaN(n)) {
        throw new Error(n + ' is not a number')
    }
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

Object.prototype.getCell3d = function(x, y, z, defaultValue) {
    const storedValue = this[`${x}_${y}_${z}`];
    // Check for undefined explicitly, because empty string and 0 are also falsey.
    if (storedValue === undefined) {
        return defaultValue;
    }
    return storedValue;
};

Object.prototype.setCell = function(x, y, n = true) {
    this.dimensions = this.dimensions || { min_x: x, max_x: x, min_y: y, max_y: y };
    this.dimensions.min_x = Math.min(this.dimensions.min_x, x);
    this.dimensions.min_y = Math.min(this.dimensions.min_y, y);
    this.dimensions.max_x = Math.max(this.dimensions.max_x, x);
    this.dimensions.max_y = Math.max(this.dimensions.max_y, y);
     this[`${x}_${y}`] = n;
};
Object.prototype.setCell3d = function(x, y, z, n = true) {
    this.dimensions = this.dimensions || { min_x: x, max_x: x, min_y: y, max_y: y, min_z: z, max_z: z };
    this.dimensions.min_x = Math.min(this.dimensions.min_x, x);
    this.dimensions.min_y = Math.min(this.dimensions.min_y, y);
    this.dimensions.min_z = Math.min(this.dimensions.min_z, z);
    this.dimensions.max_x = Math.max(this.dimensions.max_x, x);
    this.dimensions.max_y = Math.max(this.dimensions.max_y, y);
    this.dimensions.max_z = Math.max(this.dimensions.max_z, z);
     this[`${x}_${y}_${z}`] = n;
};

Array.prototype.shuffle = function shuffleArray() {
  let curId = this.length;
  while (0 !== curId) {
    curId--;
    let randId = Math.floor(Math.random() * curId);
    let tmp = this[curId];
    this[curId] = this[randId];
    this[randId] = tmp;
  }
  return this;
}