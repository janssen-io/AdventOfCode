// file reading
const fs = require('fs');

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// Helpers
class Message {
    constructor(message) {
        this._message = message;
        this._cursor = 0;
    }

    read(end) {
        let part;
        if (end === undefined) {
            part = this._message.slice(this._cursor);
            this._cursor = message.length;
        } else {
            if (this._cursor + end > this._message.length) {
                throw new Error("Cannot read " + end + "bits, remaining length is " + (this._message.length - this._cursor));
            }
            part = this._message.slice(this._cursor, this._cursor + end);
            this._cursor += end;
        }
        return part;
    }

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

    length() { return this._message.length - this._cursor; }

    remainingMessage() {
        return this._message.slice(this._cursor);
    }

    isEOF() {
        return this.remainingMessage().filter(x => x == '1').length == 0;
    }
}

const partsToDec = (parts) => parseInt(parts.join(''), 2);
function parseLiteral(rest) {
    let parts = [];
    while(true) {
        let byte = rest.read(5);
        let slice = byte.slice(1);

        parts = parts.concat(slice);
        if (byte[0] === '0') {
            break;
        }
    }
    return partsToDec(parts);
}

function parsePacket(message) {
    if(message.isEOF()) {
        //  message.read(); // eat the rest of the bits
        return;
    }

    const header = message.read(3).join('');
    const typeId = message.read(3);
    const type = partsToDec(typeId);

    // console.log({header, type, head: message.peak()})
    return parseBody(header, type, message);
}

function parseBody(header, type, message) {
    // console.log({header, type, message})
    if (type === 4) {
        let literal = parseLiteral(message);
        return { header, value: literal, type }
    }

    const lengthTypeId = message.read(1)[0];

    if (lengthTypeId == '0') {
        let totalLength = partsToDec(message.read(15));
        if (totalLength == 0) {
            throw new Error("Total length cannot be 0");
        }
        let parsedPackets = [];
        let startLength = message.length();
        while(startLength - totalLength < message.length())
        {
            let parsed = parsePacket(message);
            parsedPackets.push(parsed);
        }
        let r = { header, type, children: parsedPackets }
        return r;
    }
    else if (lengthTypeId == '1') {
        let numberOfSubPackets = partsToDec(message.read(11));
        if (numberOfSubPackets == 0) {
            throw new Error("Number of subpackets cannot be 0");
        }
        let parsedPackets = [];
        for(let i = 0; i < numberOfSubPackets; i++) {
            let parsed = parsePacket(message);
            parsedPackets.push(parsed);
        }
        let r = { header, type, children: parsedPackets }
        return r;
    }
}

const NAMES = ["sum", "product", "min", "max", "literal", "gt", "lt", "eq"];
const TYPES = NAMES.reduce( (p,n, i) => { p[n] = i; return p; }, {});

// part 1
function sumHeader(tree) {
    if(!tree) {
        return 0;
    }

    let h = parseInt(tree.header, 2);
    if (tree.type === TYPES.literal) {
        return h;
    }
    else {
        return h + tree.children.reduce((p, n) => p + sumHeader(n), 0);
    }
}

function evaluate(tree) {
    if (tree.type === TYPES.literal) {
        return +tree.value;
    }

    let values = tree.children.map(evaluate);
    if(values.indexOf(undefined) > -1) {
        printAst(tree);
    }
    switch(tree.type) {
        case TYPES.sum:
            return values.reduce((p, n) => p + n, 0);
        case TYPES.product:
            return values.reduce((p, n) => p * n, 1);
        case TYPES.min:
            return values.reduce((p, n) => n < p ? n : p, Number.MAX_VALUE);
        case TYPES.max:
            return values.reduce((p, n) => n > p ? n : p, Number.MIN_VALUE);
        case TYPES.gt:
            return values[0] > values[1]  ? 1 : 0;
        case TYPES.lt:
            return values[0] < values[1]  ? 1 : 0;
        case TYPES.eq:
            return values[0] == values[1] ? 1 : 0;
    }
}

function printAst(tree, d = 0) {
    if (tree.type === TYPES.literal) {
        console.log(" ".repeat(d * 4) + tree.header + ': literal(value: ' + tree.value + ')');
    }
    else {
        console.log(" ".repeat(d * 4) + tree.header + ': operator(kind: ' + NAMES[tree.type].toUpperCase() + ', size:' + tree.children.length + ')');
        for(let i = 0; i < tree.children.length; i++) {
            print(tree.children[i], d + 1);
        }
    }
}

// Alternative solution: create javascript and evaluate it.
function sum(...args) { return args.reduce((a,b) => a + b); }
function product(...args) { return args.reduce((a,b) => a * b); }
function min(...args) { return args.reduce((a,b) => a < b ? a : b); }
function max(...args) { return args.reduce((a,b) => a > b ? a : b); }
function gt(...args) { return args.reduce((a,b) => a > b  ? 1 : 0); }
function lt(...args) { return args.reduce((a,b) => a < b  ? 1 : 0); }
function eq(...args) { return args.reduce((a,b) => a == b ? 1 : 0); }

function printJs(tree, d = 0, indent = false) {
    if (tree.type === TYPES.literal) {
        return '\n ' + ' '.repeat(d * 2 * indent) + tree.value;
    }
    else {
        let str = '\n ';
        str += " ".repeat(d * 2 * indent) + NAMES[tree.type] + '(';
        let cs = [];
        for(let i = 0; i < tree.children.length; i++) {
            cs.push(printJs(tree.children[i], d + 1));
        }
        str += cs.join(', ');
        str += ')';
        return str;
    }
}

// solvers
function solve(lines) {
    const text = lines[0].split('');
    let message = text.map(x => parseInt(x, 16).toString(2).padStart(4, '0')).join('').split('');
    const reader = new Message(message);

    let ast = parsePacket(reader);
    // console.log(printJs(ast).split('\n').join(''));
    // return eval(printJs(ast));
    return evaluate(ast);
}

let file = process.argv[2] || 'example';
p(`16-${file}.txt`, solve);