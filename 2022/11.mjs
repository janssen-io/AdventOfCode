import { readAndSolve } from '../aoc.mjs'

function parse(monkey) {
    const data = monkey.split('\n');
    const opConstant = +data[2].replace(/[^\d]+/g, '');
    const testConstant = +data[3].replace(/[^\d]+/g, '');
    const trueMonkey = +data[4].replace(/[^\d]+/g, '');
    const falseMonkey = +data[5].replace(/[^\d]+/g, '');
    const id = +data[0].replace(/[^\d]+/g, '');
    return {
        id,
        items: data[1].replace(/[^\d,]+/g, '').split(',').map(x => +x),
        operation: old => Math.floor((data[2].includes('*') ? old * (opConstant || old) : old + (opConstant || old)) / 3),
        test: (level, monkeys) => level % testConstant == 0 
            ? monkeys[trueMonkey].items.push(level)
            : monkeys[falseMonkey].items.push(level)
            // ? console.log(id, trueMonkey, level) || monkeys[trueMonkey].items.push(level)
            // : console.log(id, falseMonkey, level) || monkeys[falseMonkey].items.push(level)
    }
}

const solve = (monkeys) => {
    const n = 20;
    const players = monkeys.map(parse)
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

    console.log(counter);
    return Object.values(counter).numSort().reverse().slice(0, 2).product();

}

readAndSolve(process.argv[2] || '11.input', solve, '\n\n');