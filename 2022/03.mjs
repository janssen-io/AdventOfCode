import { readAndSolve } from '../aoc.mjs'

const isLower = (c) => c.toLowerCase() === c;
const prio = (c) => c.charCodeAt() + (isLower(c) ? 1 - 'a'.charCodeAt() : 27 - 'A'.charCodeAt())

const getValue = group => group.unique().map(prio).sum();

function solve(lines) {
    const p1 = lines
        .map(line => [line.substring(0, line.length / 2), line.substring(line.length / 2)])
        .map(([left, right]) => left.intersect(right))
        .map(getValue)
        .sum();

    const p2 = lines
        .chunk(3)
        .map(group => group.reduce((acc, backpack) => acc.intersect(backpack)))
        .map(getValue)
        .sum();

    return {p1, p2}
};

readAndSolve(process.argv[2] || '03.input', solve);
// readAndSolve(process.argv[2] || '03 copy.input', solve);