import { readAndSolve } from '../aoc.mjs'

const parseInstruction = (line) => line.split(/[^\d]+/).filter(x => x);

function parseHeader(header) {
    const bottomUp = header.reverse().slice(1);
    const numColumns = header[0].trim().last();
    let columns = []
    for(let i = 0; i < numColumns; i++) {
        for(let row = 0; row < bottomUp.length; row++) {
            columns[i] = columns[i] || []
            if (bottomUp[row][i * 4 + 1] != ' ')
                columns[i].push(bottomUp[row][i * 4 + 1])
        }
    }
    return columns
}

function apply(configuration, [num, from, to]) {
    for(let i = 0; i < num; i++) {
        configuration[to - 1].push(configuration[from - 1].pop());
    }
    return configuration;
}
function apply2(configuration, [num, from, to]) {
    configuration[to - 1] = [...configuration[from - 1].slice(0, num), ...configuration[to - 1],];
    configuration[from - 1] = configuration[from - 1].slice(num);
    return configuration;
}

function solve1(lines) {
    const header = lines.takeWhile(x => x !== '');
    const remainder = lines.skipWhile(x => x[0] !== 'm');
    const instructions = remainder.map(parseInstruction);

    const configuration = instructions.reduce(
        (config, ins) => apply(config, ins),
        parseHeader(header));

    return configuration.map(x => x.last()).join('');
};

function solve2(lines) {
    const header = lines.takeWhile(x => x !== '');
    const remainder = lines.skipWhile(x => x[0] !== 'm');
    const instructions = remainder.map(parseInstruction);

    const configuration = instructions.reduce(
        (config, ins) => apply2(config, ins),
        parseHeader(header).map(col => col.reverse()));

    return configuration.map(x => x[0]).join('');
};

const solve = lines => { return { p1: solve1(lines), p2: solve2(lines) }}

readAndSolve(process.argv[2] || '04.input', solve, '\n');