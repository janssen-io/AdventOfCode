import {readAndSolve} from '../aoc.mjs'

function solve(lines) {
    const elves = lines.group('', x => +x);
    const calories = elves
        .map(elf => elf.sum())
        .numSort()
        .reverse();

    const p1 = calories[0];
    const p2 = calories.slice(0, 3).sum();
    return {p1, p2};
}

readAndSolve(process.argv[2] || '01.input', solve);