// file reading
import {readFile} from 'fs';

function* genmap (gen, f) {
    for(let item of gen) yield f(item);
}

Array.prototype.withIndex = function(offset = 0) {
    return this.map((item, i) => [item, i + offset]);
}

Array.prototype.cartesian = function(other) {
    if (!Array.isArray(other)) throw new Error("other is not an array");
    const product = [];
    for(let a of this) {
        for(let b of other) {
            product.push([a,b]);
        }
    }
    return product;
}

Array.prototype.numSort = function(reverse) {
    return this.sort((a,b) => reverse ? b - a : a - b);
}
Array.prototype.numSortBy = function(reverse, s) {
    return this.sort((a,b) => reverse ? s(b) - s(a) : s(a) - s(b));
}

Array.prototype.unique = function() {
    return this.filter((value, index, self) => self.indexOf(value) === index)
}

Array.prototype.sum = function() {
    return this.reduce((p,n) => +p + +n, 0);
}

Array.prototype.product = function() {
    return this.reduce((p,n) => +p * +n, 1);
}

Array.prototype.without = function(other, eql) {
    if (typeof(eql) !== 'function') {
        return this.filter(item => !Array.from(other).includes(item));
    }
    else {
        return this.filter(item => !Array.from(other).some(mine => eql(item, mine)));
    }
}

Array.prototype.intersect = function(other, eql) {
    if (typeof(eql) !== 'function') {
        return this.filter(item => Array.from(other).includes(item));
    }
    else {
        return this.filter(item => Array.from(other).some(mine => eql(item, mine)));
    }
}

/**
 * Generate an array of consecutive numbers.
 * @param {number} start Inclusive lowest number
 * @param {number} end Exclusive highest number
 * @returns [start..end-1]
 */
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

Array.prototype.last = function(n = 0) { 
    if (n < 0) throw new Error("n must be 0 or greater")
    if (n >= this.length) throw new Error("n must be smaller than the length: " + this.length)
    return this[this.length - 1 - n]
};

Array.prototype.split = function(delim = '', map) {
    if (typeof(map) !== 'function') {
        map = x => x;
    }
    if (typeof(delim) !== 'function') {
        const delimString = delim
        delim = x => x === delimString;
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

String.prototype.filter = function(pred) {
    return this.split('').filter(pred).join('');
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

String.prototype.numbers = function(delim) {
    return this.split(delim || /[ ,]/)
        .map(words => words.replace(/[^\d-]+/g, ''))
        .filter(x => x.length > 0 && !isNaN(x))
        .map(x => +x);
}

// This works for _most_ objects
Object.prototype.clone = function() { return JSON.parse(JSON.stringify(this)); };

Object.prototype.filter = function(p) {
    const result = {};
    for(let [k, v] of Object.entries(this)) {
        if (p(k, v)) {
            result[k] = v;
        }
    }
    return result;
};

Object.prototype.merge = function(other, p) {
    if (typeof(p) !== 'function') {
        p = () => true;
    }
    const result = Object.assign(this, other);
    for(let k of Object.keys(this).intersect(Object.keys(other))) {
        result[k] = p(this[k], other[k]) ? this[k] : other[k];
    }
    return result;
}

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

Object.prototype.keys = function() { return Object.keys(this); }

Object.prototype.setCell = function(x, y, n = true) {
    this.dimensions = this.dimensions || { min_x: x, max_x: x, min_y: y, max_y: y };
    this.dimensions.min_x = Math.min(this.dimensions.min_x || 0, x);
    this.dimensions.min_y = Math.min(this.dimensions.min_y || 0, y);
    this.dimensions.max_x = Math.max(this.dimensions.max_x || 0, x);
    this.dimensions.max_y = Math.max(this.dimensions.max_y || 0, y);
     this[`${x}_${y}`] = n;
};

Object.prototype.map2D = function(fmap) {
    const mappedGrid = { dimensions: this.dimensions };
    for(let x of range(0, this.dimensions.max_x + 1)) {
        for(let y of range(0, this.dimensions.max_y + 1)) {
            mappedGrid.setCell(x, y, fmap(this.getCell(x, y), x, y));
        }
    }
    return mappedGrid;
}

Object.prototype.setCellWrapped = function(x, y, n = true) {
    if (x > this.dimensions.max_x) x = 0;
    if (y > this.dimensions.max_y) y = 0;
    if (x < this.dimensions.min_x) x = this.dimensions.max_x;
    if (y < this.dimensions.min_y) y = this.dimensions.max_y;
    return this.setCell(x, y, n);
};

Object.prototype.setCell3d = function(x, y, z, n = true) {
    this.dimensions = this.dimensions || { min_x: x, max_x: x, min_y: y, max_y: y, min_z: z, max_z: z };
    this.dimensions.min_x = Math.min(this.dimensions.min_x || 0, x);
    this.dimensions.min_y = Math.min(this.dimensions.min_y || 0, y);
    this.dimensions.min_z = Math.min(this.dimensions.min_z || 0, z);
    this.dimensions.max_x = Math.max(this.dimensions.max_x || 0, x);
    this.dimensions.max_y = Math.max(this.dimensions.max_y || 0, y);
    this.dimensions.max_z = Math.max(this.dimensions.max_z || 0, z);
    this[`${x}_${y}_${z}`] = n;
};

Object.prototype.rows = function(defaultValue) {
    const rows = [];
    console.log(this.dimensions)
    for(let y of range(this.dimensions.min_y, this.dimensions.max_y + 1)) {
        let row = [];
        for(let x of range(this.dimensions.min_x, this.dimensions.max_x + 1)) {
            row.push(this.getCell(x, y, defaultValue));
        }
        rows.push(row);
    }
    return rows;
}

Object.prototype.showGrid = function(defaultValue, reverse) {
    let rows = this.rows(defaultValue)
    if (reverse) rows = rows.reverse();

    return rows
        .map(row => row.join(''))
        .join('\r\n');
}

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
function* search(q, genStates, isMatch, key, show, next, maxLength) {
    const solutions = [];
    const seen = {};
    let i = 0;
    // for(let s of q) {
    //     s.depth = s.depth || 0;
    // }
    let nextItem = next.apply(q)
    while(nextItem) {
        const state = nextItem;
        let k = key(state);
        if  (seen[k]) {
            nextItem = next.apply(q)
            continue;
        }
        else seen[k] = true;

        i++;
        if (i % 100_000 == 0) {
            console.log("search: ", i, q.length, nextItem)
        }
        if (show) {
            console.log("====== SEARCH ======")
            console.log({i, k, state: show(state) }); //, seen });
        }

        if (isMatch(state)) {
            // console.log('match', state);
            solutions.push(state);
            yield { i, state };
            // console.log('Found:', state)
            nextItem = next.apply(q)
            continue;
            
        }
        // q = q.concat(Array.from(genStates(state)).map(s => { s.depth = (state.depth || 0) + 1; return s}));
        // if (sort === undefined) continue;
        // q = q.sort(sort);
        for(let s_ of genStates(state)) {
            q.push(s_);
        }
        nextItem = next.apply(q)
        // if (q.length > 10_000) console.log(q.length);
    }
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

function readAndSolve(input, solver, delim = '\n') {
    return new Promise((resolve, reject) => {
        if (!input) {
            const d = ('' + new Date().getDate()).padStart(2, '0');
            input = `${d}.input`;
        }
        readFile(input, 'utf8', function(error, data) {
            if(error) {
                console.error(error)
                reject(error);
            }
            var lines = data.replace(/\r/g, '').split(delim);
            const answer = solver(lines);
            resolve({file: input, answer});
        });
    })
}

function printAnswer(answer) {
    console.log(answer.file, 'answer:'.green());
    console.log(answer.answer);
}

function test(title, expected, actual) {
    const result = JSON.stringify(expected) == JSON.stringify(actual);
    console.log(result ? '🎄' : '🎅', result ? title.green() : title.red());
    if(!result) {
        console.log('Expected:'.colour(30), expected);
        console.log('  Result:'.colour(30), actual);
    }
}

function assert(predicate, msg) {
    if (!predicate()) {
        throw new Error(msg);
    }
}

function* primes(max) {
    yield 1;
    yield 2;
    const primes = [2];
    for (let candidate = 3; candidate < max; candidate++) {
        const isDivisible = primes.some(prime => candidate % prime == 0);
        if (!isDivisible) {
            primes.push(candidate);
            yield candidate
        }
    }
}

function manhattan(p, q) {
    const x = Math.abs(p.x - q.x);
    const y = Math.abs(p.y - q.y);
    const z = Math.abs(p.z - q.z) || 0;
    return x + y + z;
}

export {
    readAndSolve,
    printAnswer,
    test,
    Scanner,
    dfs,
    bfs,
    search,
    range,
    genmap,
    assert,
    primes,
    manhattan,
}