// file reading
import {readFile} from 'fs';

Array.prototype.numSort = function() {
    return this.sort((a,b) => a-b);
}

Array.prototype.unique = function() {
    return this.filter((value, index, self) => self.indexOf(value) === index)
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

function range(start, end) {
    let result = [];
    for(let i = start; i < end; i++) {
        result.push(i)
    }
    return result;
}

Array.prototype.indexOfP = function(predicate) {
    for(let i = 0; i < this.length; i++) {
        if (predicate(this[i])) return i;
    }
    return -1;
}

Array.prototype.skipWhile = function(predicate) {
    let firstIndexNotMatch = this.indexOfP(x => !(predicate(x)))
    if (firstIndexNotMatch === -1) {
        return Array.from(this);
    }
    return this.slice(firstIndexNotMatch);
}

Array.prototype.takeWhile = function(predicate) {
    let firstIndexNotMatch = this.indexOfP(x => !(predicate(x)))
    if (firstIndexNotMatch === -1) {
        return [];
    }
    return this.slice(0, firstIndexNotMatch);
}

Array.prototype.repeat = function(n) {
    let result = [];
    for(let i = 0; i < n; i++) {
        result = result.concat(this);
    }
    return result;
}

Array.prototype.chunk = function(n) {
    let result = [];
    for(let i = 0; i < this.length; i += n) {
        result.push(this.slice(i, i + n));
    }
    return result;
}

Array.prototype.last = function() { 
    return this[this.length - 1]
};

Array.prototype.split = function(delim = '', map) {
    if (typeof(map) !== 'function') {
        map = x => x;
    }
    if (typeof(delim) !== 'function') {
        delim = x => x === delim;
    }
    const groups = [];
    let group = [];
    for(let line of this) {
        if (delim(line)) {
            groups.push(group);
            group = [];
        }
        else {
            group.push(map(line))
        }
    }
    if (group.length > 0) { // push the last group in case there is no delimiting line after the final group
        groups.push(group);
    }
    return groups;
}

String.prototype.last = function() { 
    return this[this.length - 1]
};

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

class Scanner {
    constructor(message, isEof) {
        this._message = message;
        this._cursor = 0;
        this._isEof = isEof;
    }

    /**
     * Reads the next characters from the message and updates the cursor position.
     * @param {number} length The number of characters to read
     * @returns {string} The characters from the current cursor position
     */
    read(length) {
        let part;
        if (length === undefined) {
            part = this._message.slice(this._cursor); // read until the end
            this._cursor = this._message.length;
        } else {
            if (this._cursor + length > this._message.length) {
                throw new Error("Cannot read " + length + "bits, remaining length is " + (this._message.length - this._cursor));
            }
            part = this._message.slice(this._cursor, this._cursor + length);
            this._cursor += length;
        }
        return part;
    }

    /**
     * Shows the next characters from the message without updating the cursor position.
     * @param {number} end The number of characters to peek
     * @returns {string} The characters from the current cursor position
     */
    peak(end) {
        let part;
        if (end === undefined) {
            part = this._message.slice(this._cursor);
        } else {
            if (this._cursor + end > this._message.length) {
                throw new Error("Cannot peak " + end + "bits, remaining length is " + (this._message.length - this._cursor));
            }
            part = this._message.slice(this._cursor, this._cursor + end);
        }
        return part;
    }

    /**
     * @returns The remaining length of the message
     */
    length() { return this._message.length - this._cursor; }

    /**
     * Peeks at the characters of the message from the current cursor position.
     * @returns The remainder of the message from the current cursor position.
     */
    remainingMessage() {
        return this._message.slice(this._cursor);
    }

    isEOF() {
        if (this.is_Eof === undefined) {
            return this._cursor === this._message.length;
        }
        else {
            return this._isEof(arguments) 
        }
    }
}

/**
 * 
 * @param {Array} q Array with items in the queue. Should contain a single item to start with.
 * @param {function} genStates Generate the next states based on the current item
 * @param {function} isMatch Determine whether the current state is a viable solution
 * @param {function} key Generate a hask-key to add the current item to a visited list
 * @param {function} show Create a debug representation of the item
 * @returns 
 */
function bfs(q, genStates, isMatch, key, show = undefined) {
    return search(q, genStates, isMatch, key, show, Array.prototype.shift)
}

/**
 * 
 * @param {Array} q Array with items in the queue. Should contain a single item to start with.
 * @param {function} genStates Generate the next states based on the current item
 * @param {function} isMatch Determine whether the current state is a viable solution
 * @param {function} key Generate a hask-key to add the current item to a visited list
 * @param {function} show Create a debug representation of the item
 * @returns 
 */
function dfs(q, genStates, isMatch, key, show = undefined) {
    return search(q, genStates, isMatch, key, show, Array.prototype.pop)
}

/**
 * 
 * @param {Array} q Array with items in the queue. Should contain a single item to start with.
 * @param {function} genStates Generate the next states based on the current item
 * @param {function} isMatch Determine whether the current state is a viable solution
 * @param {function} key Generate a hask-key to add the current item to a visited list
 * @param {function} show Create a debug representation of the item
 * @param {function} next Grab the next item from the queue/stack (Array.shift or Array.pop respectively for bfs/dfs) 
 * @returns 
 */
function search(q, genStates, isMatch, key, show, next) {
    const solutions = [];
    const seen = {};
    let i = 0;
    while(q.length) {
        const state = next.apply(q);
        let k = key(state);
        if  (seen[k]) continue;
        else seen[k] = true;

        i++;
        if (show) {
            console.log({i, k, state: show(state), seen });
        }

        if (isMatch(state)) {
            solutions.push(state);
        }
        q = q.concat(genStates(state));
    }
    return solutions;
}

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

function readAndSolve(input, solver, delim = '\n', then) {
    if (!input) {
        const d = ('' + new Date().getDate()).padStart(2, '0');
        input = `${d}.input`;
    }
    readFile(input, 'utf8', function(error, data) {
        if(error) console.error(error)
        var lines = data.replace(/\r/g, '').trim().split(delim);
        console.log(input, 'answer: '.green(), solver(lines));
        if (typeof(then) === 'function') {
            then();
        }
    });
}

export { readAndSolve, Scanner, dfs, bfs, range }