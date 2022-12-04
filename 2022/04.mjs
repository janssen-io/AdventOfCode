import { readAndSolve } from '../aoc.mjs'

const lineToSections = (line) => line.split(/[,-]/).map(x => +x).chunk(2);

const isWhollyContainedIn = ([left, right]) =>
    (left[0] >= right[0] && left[1] <= right[1])
        || (right[0] >= left[0] && right[1] <= left[1]);

const isPartiallyContainedIn = ([left, right]) => 
         (left[0] >= right[0] && left[0] <= right[1])
            || (left[1] >= right[0] && left[1] <= right[1])
            || (right[0] >= left[0] && right[0] <= left[1])
            || (right[1] >= left[0] && right[1] <= left[1]);

function solve(lines) {
    const p1 = lines.map(lineToSections).map(isWhollyContainedIn).filter( x => x).length;
    const p2 = lines.map(lineToSections).map(isPartiallyContainedIn).filter( x => x).length;

    return {p1, p2};
};

readAndSolve(process.argv[2] || '04.input', solve);