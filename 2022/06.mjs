import { readAndSolve, range } from '../aoc.mjs'

function solveFor(markerLength) {
    return ([line]) =>
        range(markerLength, line.length)
            .map(markerEnd => line.slice(markerEnd - markerLength, markerEnd))
            .map(marker => new Set(marker).size)
            .indexOfP(testLength => testLength == markerLength) + markerLength;
}

const solve1 = solveFor(4);
const solve2 = solveFor(14);
const solve = lines => { return { p1: solve1(lines), p2: solve2(lines) } }

readAndSolve(process.argv[2] || '06.input', solve, '\n');