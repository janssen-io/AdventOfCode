import { readAndSolve, range } from '../aoc.mjs'

function solve(markerLength) {
    return function([line]) {
        return range(markerLength, line.length)
            .map(markerEnd => line.slice(markerEnd - markerLength, markerEnd))
            .map(packet => new Set(packet).size)
            .indexOfP(length => length == markerLength) + markerLength;
    }
}

readAndSolve(process.argv[2] || '04.input', solve(4), '\n');
readAndSolve(process.argv[2] || '04.input', solve(14), '\n');