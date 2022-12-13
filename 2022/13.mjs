import { readAndSolve, printAnswer, test } from '../aoc.mjs'

function compare(left, right, debug) {
    if (debug) console.log({leftItem: left, rightItem: right})

    if (left === undefined) {
        // Both ran out
        if (right === undefined) return 0;

        // Left ran out
        return 1;
    }

    if (right === undefined) {
        // Right ran out
        return -1;
    }

    if (Array.isArray(left) && Array.isArray(right)) {
        let result = 0;
        for(let i = 0; i <= left.length && result === 0; i++) {
            result = compare(left[i], right[i], debug);
        }
        return result;
    }

    if (Array.isArray(left)) return compare(left, [right], debug);
    if (Array.isArray(right)) return compare([left], right, debug);

    return Math.sign(right - left);
}

const solveP1 = (lines) => {
    return lines
        .map((pair, index) => {
            const [left, right] = pair.split('\n').map(eval);
            const result = compare(left, right);
            if(result === -1) return null;
            return index;
        })
        .filter(index => index !== null)
        .map(i => i + 1) // packets are 1-indexed
        .sum();
}

const solveP2 = (lines, dividerPackets = []) => {
    const packets = lines
        .flatMap(group => group.split('\n'))
        .map(eval)
        .concat(dividerPackets)
        .sort(compare)
        .reverse();

    return dividerPackets
        .map(dividerPacket => packets.indexOfP(packet => JSON.stringify(packet) == JSON.stringify(dividerPacket)))
        .map(index => index + 1) // packets are 1-indexed
        .product();
}

const solve = (lines) => {
    return { 
        p1: solveP1(lines),
        p2: solveP2(lines, [[[2]], [[6]]]),
    }
}

(async () => {
    let example = await readAndSolve('13.example.input', solve, '\n\n');
    test('Example p1', 13, example.answer.p1)
    test('Example p2', 140, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve(process.argv[2] || '13.input', solve, '\n\n');
    printAnswer(puzzle);
})();