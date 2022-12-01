// file reading
import {readFile} from 'fs';

Array.prototype.numSort = function() {
    return this.sort((a,b) => a-b);
}

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

Array.prototype.repeat = function(n) {
    let result = [];
    for(let i = 0; i < n; i++) {
        result = result.concat(this);
    }
    return result;
}

Array.prototype.group = function(delim = '', map) {
    if (typeof(map) !== 'function') {
        map = x => x;
    }
    const groups = [];
    let group = [];
    for(let line of this) {
        if (line == delim) {
            groups.push(group);
            group = [];
        }
        else {
            group.push(map(line))
        }
    }
    return groups;
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

Object.prototype.getCellWrapped = function(x, y, defaultValue) {
    if (x > this.dimensions.max_x) x = 0;
    if (y > this.dimensions.max_y) y = 0;
    if (x < this.dimensions.min_x) x = this.dimensions.max_x;
    if (y < this.dimensions.min_y) y = this.dimensions.max_y;
    return this.getCell(x, y, defaultValue);
};

Object.prototype.getCell3d = function(x, y, z, defaultValue) {
    const storedValue = this[`${x}_${y}_${z}`];
    // Check for undefined explicitly, because empty string and 0 are also falsey.
    if (storedValue === undefined) {
        return defaultValue;
    }
    return storedValue;
};

Array.prototype.getCell = function(x, y) {
    if (this.dimensions.x == undefined) throw new Error('Dimensions not set');
    if (this.dimensions.y == undefined) throw new Error('Dimensions not set');
    if(x >= this.dimensions.x) throw new Error(`Out of bounds: ${x} >= ${this.dimensions.x}`);
    if(y >= this.dimensions.y) throw new Error(`Out of bounds: ${y} >= ${this.dimensions.y}`);

    return this[y * this.dimensions.x + x];
}

Array.prototype.setCell = function(x, y, n) {
    if (this.dimensions.x == undefined) throw new Error('Dimensions not set');
    if (this.dimensions.y == undefined) throw new Error('Dimensions not set');
    if(x >= this.dimensions.x) throw new Error(`Out of bounds: ${x} >= ${this.dimensions.x}`);
    if(y >= this.dimensions.y) throw new Error(`Out of bounds: ${y} >= ${this.dimensions.y}`);

    this[y * this.dimensions.x + x] = n;
}

Array.prototype.getCellWrapped = function(x, y) {
    if (this.dimensions.x == undefined) throw new Error('Dimensions not set');
    if (this.dimensions.y == undefined) throw new Error('Dimensions not set');

    x = (x + this.dimensions.x) % this.dimensions.x;
    y = (y + this.dimensions.y) % this.dimensions.y;

    return this[y * this.dimensions.x + x];
}

Array.prototype.setCellWrapped = function(x, y, n) {
    if (this.dimensions.x == undefined) throw new Error('Dimensions not set');
    if (this.dimensions.y == undefined) throw new Error('Dimensions not set');

    x = (x + this.dimensions.x) % this.dimensions.x;
    y = (y + this.dimensions.y) % this.dimensions.y;

    this[y * this.dimensions.x + x] = n;
}

Object.prototype.setCell = function(x, y, n = true) {
    this.dimensions = this.dimensions || { min_x: x, max_x: x, min_y: y, max_y: y };
    this.dimensions.min_x = Math.min(this.dimensions.min_x, x);
    this.dimensions.min_y = Math.min(this.dimensions.min_y, y);
    this.dimensions.max_x = Math.max(this.dimensions.max_x, x);
    this.dimensions.max_y = Math.max(this.dimensions.max_y, y);
     this[`${x}_${y}`] = n;
};

Object.prototype.setCellWrapped = function(x, y, n = true) {
    if (x > this.dimensions.max_x) x = 0;
    if (y > this.dimensions.max_y) y = 0;
    if (x < this.dimensions.min_x) x = this.dimensions.max_x;
    if (y < this.dimensions.min_y) y = this.dimensions.max_y;
    return this.setCell(x, y, n);
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

function readAndSolve(input, solver) {
    if (!input) {
        const d = ('' + new Date().getDate()).padStart(2, '0');
        input = `${d}.input`;
    }
    readFile(input, 'utf8', function(error, data) {
        var lines = data.replace(/\r/g, '').trim().split('\n');
        console.log(input, 'answer: '.green(), solver(lines));
    });
}

export { readAndSolve }