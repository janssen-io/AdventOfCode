import { readAndSolve } from '../aoc.mjs'

const parseInstruction = (line) => line.split(/[^\d]+/).filter(x => x);

function parseHeader(header) {
    const cols = header.slice(0, header.length - 1);
    const numColumns = header.last().trim().last();
    let columns = []
    for(let i = 0; i < numColumns; i++) {
        for(let row = 0; row < cols.length; row++) {
            columns[i] = columns[i] || []
            if (cols[row][i * 4 + 1] != ' ')
                columns[i].push(cols[row][i * 4 + 1])
        }
    }
    return columns
}

function apply(movedStackFn) {
    return (configuration, [num, from, to]) => {
        configuration[to - 1] = [
            ...movedStackFn(configuration[from - 1].splice(0, num)),
            ...configuration[to - 1]];
        return configuration;
    }
}

function _solve(lines, part) {
    const [header, remainder] = lines.split('')
    const instructions = remainder.map(parseInstruction);
    const movedStackFn = part == 1 ? xs => xs.reverse() : xs => xs;

    return instructions
        .reduce(apply(movedStackFn), parseHeader(header))
        .map(x => x[0])
        .join('');
};

const solve = lines => { return { p1: _solve(lines, 1), p2: _solve(lines, 2) }}

readAndSolve(process.argv[2] || '04.input', solve, '\n');