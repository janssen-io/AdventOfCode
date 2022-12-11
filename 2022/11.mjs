import { readAndSolve } from '../aoc.mjs'

function parse(monkey, worryDiv = 3, mod) {
    const numbersOnLine = monkey.split('\n').map(l => l.numbers());
    const id = numbersOnLine[0][0]
    const opConstant = numbersOnLine[2][0]
    const testConstant = numbersOnLine[3][0]
    const trueMonkey = numbersOnLine[4][0]
    const falseMonkey = numbersOnLine[5][0]
    if (mod == undefined) mod = Number.MAX_VALUE;
    return {
        id,
        items: numbersOnLine[1],
        operation: old => Math.floor((monkey.includes('*') 
            ? old * (opConstant || old) 
            : old + (opConstant || old)) / worryDiv) % mod,
        test: (level, monkeys) => level % testConstant == 0 
            ? monkeys[trueMonkey].items.push(level)
            : monkeys[falseMonkey].items.push(level)
    }
}

const solveFor = (monkeys, rounds, worryRate) => {
    const n = rounds;
    const maxTest = monkeys.map(m => +m.split('\n')[3].numbers()[0]).product();
    const players = monkeys.map(m => parse(m, worryRate, maxTest));
    const counter = {};
    for(let i = 0; i < n; i++) {
        for(let player of players) {
            const itemSize = player.items.length;
            for(let x = 0; x < itemSize; x++) {
                const item = player.items.shift();
                const level = player.operation(item);
                player.test(level, players);
                counter[player.id] = (counter[player.id] || 0) + 1;
            }
        }
    }

    return Object.values(counter).numSort().reverse().slice(0, 2).product();
}

const solve = (lines) => {
    return { p1: solveFor(lines, 20, 3), p2: solveFor(lines, 10_000, 1)}
}

readAndSolve(process.argv[2] || '11.input', solve, '\n\n');