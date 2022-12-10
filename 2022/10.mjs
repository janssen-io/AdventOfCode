import { readAndSolve, range } from '../aoc.mjs'

const solve = (lines) => {
    const registers = { x: 1 };
    let cycle = 1;

    let readCycles = [20, 60, 100, 140, 180, 220];
    let reads = [];

    let instructions = lines.map(line => line.split(' '));

    for (let instruction of instructions) {
        let oldX = 0;
        if (instruction[0] == 'noop') {
            cycle++;
        }
        else if (instruction[0] == 'addx') {
            cycle += 1;
            oldX = registers.x;
            registers['x'] += new Number(instruction[1]);
            cycle += 1;
        }

        if (cycle >= readCycles[0]) {
            let c = readCycles.shift();
            if (cycle > c) {
                reads.push(oldX * c);
            }
            else {
                reads.push(registers.x * c);
            }
        }
    }
    return reads.sum();
}

readAndSolve(process.argv[2] || '10.input', solve, '\n');