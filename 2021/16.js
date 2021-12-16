// file reading
const fs = require('fs');
require('./aoc.js')
const Data = require('./heap.js')

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

    version() {
        const version = this._message.slice(this._cursor, this._cursor + 3);
        this._cursor += 3;
        return version;
    }

    type() {
        const type = this._message.slice(this._cursor, this._cursor + 3);
        this._cursor += 3;
        return partsToDec(typeId, 2);
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
         message.read(); // eat the rest of the bytes
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
        let startLength = message.remainingMessage().length;
        while(startLength - totalLength < message.remainingMessage().length)
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

const LITERAL = 4;
function sumHeader(tree, d = 0) {
    if(!tree) {
        return 0;
    }

    let h = parseInt(tree.header, 2);
    if (tree.type === LITERAL) {
        return h;
    }
    else {
        return h + tree.children.reduce((p, n) => p + sumHeader(n, d + 1), 0);
    }
}

function evaluate(tree, d = 0) {
    if(!tree) {
        return 0;
    }

    if (tree.type === LITERAL) {
        return +tree.value;
    }
    else if(tree.type === 0) { // sum
        return tree.children.reduce((p, n) => p + evaluate(n, d + 1), 0);
    }
    else if(tree.type === 1) { // product
        return tree.children.reduce((p, n) => p * evaluate(n, d + 1), 1);
    }
    else if(tree.type === 2) { // min
        return tree.children.reduce((p, n) => {
            let next = evaluate(n, d + 1);
            return next < p ? next : p;
        }, Number.MAX_VALUE);
    }
    else if(tree.type === 3) { // max
        return tree.children.reduce((p, n) => {
            let next = evaluate(n, d + 1);
            return next > p ? next : p;
        }, Number.MIN_VALUE);
    }
    else if(tree.type === 5) { // GT
        return +evaluate(tree.children[0], d + 1) > +evaluate(tree.children[1], d + 1) ? 1 : 0;
    }
    else if(tree.type === 6) { // LT
        return evaluate(tree.children[0], d + 1) < evaluate(tree.children[1], d + 1) ? 1 : 0;
    }
    else if(tree.type === 7) { // LT
        return evaluate(tree.children[0], d + 1) == evaluate(tree.children[1], d + 1) ? 1 : 0;
    }
}

function printAst(tree, d = 0) {
    if (tree.type === LITERAL) {
        console.log(" ".repeat(d * 2) + tree.header + ': literal(' + tree.value + ')');
    }
    else {
        console.log(" ".repeat(d * 2) + tree.header + ': operator(' + tree.type + ')');
        for(let i = 0; i < tree.children.length; i++) {
            printAst(tree.children[i], d + 1);
        }
    }
}

// solvers
function solve(lines) {
    const text = lines[0].split('');
    let message = text.map(x => parseInt(x, 16).toString(2).padStart(4, '0')).join('').split('');
    const reader = new Message(message);

    let ast = parsePacket(reader);
    printAst(ast);
    return evaluate(ast);
}

let file = process.argv[2] || 'example';
p(`16-${file}.txt`, solve);