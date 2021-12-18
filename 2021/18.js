// file reading
const fs = require('fs');
require('./aoc');

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// Helpers
class SnailFishReader {
    constructor(text) {
        this._message = text;
        this._cursor = 0;
        this._depth = -1;
    }

    read(end) {
        if (end === undefined) {
            let part = this._message.slice(this._cursor);
            this._cursor = this._message.length;
            return part;
        }
        if (this._cursor + end > this._message.length) {
            console.log('read', {m: this._message.length, c: this._cursor})
            throw new Error("Cannot read " + end + "characters, remaining length is " + (this._message.length - this._cursor));
        }

        let part = this._message.slice(this._cursor, this._cursor + end);
        this._cursor += end;
        return part;
    }

    readNextNumber() {
        let num = undefined;
        let opens = 0;
        let closes = 0;
        while(!this.isEOF() && num === undefined) {
            let char = this.read(1);
            switch(char) {
                case '[':
                    this._depth++;
                    opens++;
                    break;
                case ']':
                    this._depth--;
                    closes++;
                    break;
                case ',':
                    break;
                default:
                    num = char;
                    while(!isNaN(this.peak(1))) {
                        char = this.read(1);
                        num += char;
                    }
                    break;
            }
        }
        return { number: +num, depth: this._depth, opens, closes };
    }

    peak(end) {
        let part;
        if (end === undefined) {
            part = this._message.slice(this._cursor);
        } else {
            if (this._cursor + end > this._message.length) {
                return undefined;
            }
            part = this._message.slice(this._cursor, this._cursor + end);
        }
        return part;
    }

    isEOF() {
        return this._cursor == this._message.length;
    }
}

class SnailFishBuilder {
    constructor() {
        this._parts = [];
        this._nextNumber = 0;
        this._opens = 0;
    }

    writeNumber(number, { closes, opens } = { closes: 0, opens: 0 }) {
        this._opens += opens - closes;
        let text = ']'.repeat(closes) + '['.repeat(opens) + (number + this._nextNumber);
        this._nextNumber = 0;
        this._parts.push(text);
    }

    carryToNextNumber(next) {
        this._nextNumber = next;
    }

    tryAddToPreviousNumber(last) {
        if (this._parts.length == 0) {
            return false;
        }
        let lastPart = this._parts[this._parts.length - 1];

        //replace non-digits with nothing
        let lastNumber = +lastPart.replace(/\D+/g, '');

        //replace all digits with the new digit
        this._parts[this._parts.length - 1] = lastPart.replace(/\d+/, lastNumber + last)

        return true;
    }

    closePair() {
        this._parts.push(']');
        this._opens--;
    }

    toString() {
        return this._parts.join(',')
            .replace(/,]/g, ']')            // no comma's before closing
            .replace(/\]\[/g, '],[')        // always comma's between arrays
            .replace(/\](\d+)/g, '],$1');   // always comma's between arrays and numbers
    }
}

function reduce(builder, reader, singleStep = false) {
    let left = reader.readNextNumber();
    let hasExploded = false;
    let hasSplit = false;
    if (!isNaN(left.number) && left.depth == 4) {
        explode(builder, left, reader);
        hasExploded = true;
    }
    else if (left.number >= 10) {
        split(builder, left);
        hasSplit = true;
    }
    else if (!isNaN(left.number)) {
        builder.writeNumber(left.number, { ...left });
    }

    // returns
    if (singleStep) {
        return {
            builder, reader, didAction, isEnd: reader.isEOF(),
            result: builder.toString() + reader.peak() // prevent side-effects
        }
    }
    if(hasExploded || hasSplit) { // restart
        let soFar = builder.toString() ;
        let todo = reader.read();
        console.log((hasExploded ? 'exploded: ' : 'splitted: ') + (soFar + ' ' + todo).yellow());
        return reduce(new SnailFishBuilder(), new SnailFishReader(soFar + todo));
    }
    else if (reader.isEOF()) { // finished
        return builder.toString() + ']'.repeat(builder._opens);
    }
    else { // continue
        return reduce(builder, reader);
    }
}

function explode(builder, left, reader) {
    // add to previous number
    builder.tryAddToPreviousNumber(left.number, { ...left }); // does nothing if no last number

    // replace exploding pair with 0
    builder.writeNumber(0, { closes: left.closes , opens: left.opens - 1})

    let right = reader.readNextNumber();
    if (!reader.isEOF()) {
        reader.read(1); // finish reading this pair by chomping the closing bracket
    }
    else {
        console.error({left, right, soFar: builder.toString()});
        throw new Error("Unexpected end of file while reading exploding pair.");
    }

    let firstRight = reader.readNextNumber();
    // if there is a next number, add the right number to it and write it
    if (!isNaN(firstRight.number)) {
        builder.carryToNextNumber(right.number);
        builder.writeNumber(firstRight.number, { opens: firstRight.opens, closes: firstRight.closes})
    }
    // else we are at the end
}

function split(builder, number) {
    // console.group('SPLIT');
    // console.log('SPLIT');
    // console.log({number})
    let left = Math.floor(number.number / 2);
    let right = Math.ceil(number.number / 2);
    // console.log('pre: '+ builder.toString())
    builder.writeNumber(left, { closes: number.closes, opens: number.opens + 1}); // extra open for new pair
    builder.writeNumber(right);
    builder.closePair(); // close the new pair
    // console.log('post: '+ builder.toString())
}

function add(a, b) {
    let _a = eval(a);
    let _b = eval(b);
    let addition =  JSON.stringify([_a, _b])
    console.debug('+' + addition);
    return addition;
}

function r(a, b) {
    let addition = add(a,b);
    return reduce(new SnailFishBuilder(), new SnailFishReader(addition));
}

function newReduce(line) {
    return reduce(new SnailFishBuilder(), new SnailFishReader(line));
}

// solvers


function solve(lines) {
    // test("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
    // test("[7,[6,[5,[4,[3,2]]]]", "[7,[6,[5,[7,0]]]]")
    // test("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
    // test("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    // test("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    // test("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    // test("[30]", "[[[7,8],[7,8]]]")
    // test("[[[[[1,2],[3,4]]]]]", "[[[[5,0]]]]")
    // test("[[[[1,1],[2,2]],[3,3]],[4,4]]", "[[[[1,1],[2,2]],[3,3]],[4,4]]")
    // test("[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]", "[[[[3,0],[5,3]],[4,4]],[5,5]]")
    // sumtest(["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"], "[[[[5,0],[7,4]],[5,5]],[6,6]]")
    test("[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]", "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")
    return;
    lines = lines.map(l => l.trim());

    // lines = ["[[[0,[4,5]],[0,0]], 3]", "[7, 0]"];
    // console.log("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]");
    // return newReduce("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]");
    let result = lines[0];
    console.log(result);
    for(let i = 1; i < lines.length; i++) {
        console.log(lines[i]);
        result = r(result, lines[i]);
        console.log(result);
        console.log();
    }
    return result;
}

function test(input, expect) {
    console.log('   input:', input)
    let result = newReduce(input);
    if (expect != result) {
        console.error({ input, result, expect });
        throw new Error("Test Failed.")
    }
}

function sumtest(input, expect) {
    let result = input[0];
    for(let i = 1; i < input.length; i++) {
        result = r(result, input[i]);
    }
    if (expect != result) {
        console.error({ input, result, expect });
        throw new Error("Sumtest Failed.")
    }
}

let file = process.argv[2] || 'example';
p(`18-${file}.txt`, solve);