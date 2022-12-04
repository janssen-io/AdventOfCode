import { readAndSolve, range } from '../aoc.mjs'

const lineToSections = (line) => {
    return line.split(/[,-]/).map(x => +x).chunk(2);
}

const isContainedIn = ([left, right]) => {
        let result = (left[0] >= right[0] && left[0] <= right[1])
            || (left[1] >= right[0] && left[1] <= right[1])
            || (right[0] >= left[0] && right[0] <= left[1])
            || (right[1] >= left[0] && right[1] <= left[1]);
        return result;
}

function solve(lines) {
    return lines.map(lineToSections).map(isContainedIn).filter( x => x).length;
};

readAndSolve(process.argv[2] || '04.input', solve);