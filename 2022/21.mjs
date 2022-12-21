import { readAndSolve, printAnswer } from '../aoc.mjs'

class Monkey {
    constructor(name, number, left, op, right) {
        this.name = name;
        this.number = number;
        this.left = left;
        this.op = op;
        this.right = right;
        this.hasYelled = false;
    }

    yell(monkeys) {
        let result = { done: false }
        if (isNaN(this.number)) return result;
        if (this.hasYelled) return result;
        if (this.name == 'root') {
            // Monkeys don't "return" numbers, they throw them of course!
            // Future employers: no, I don't usually use exceptions for control flow.
            throw new Error(this.number);
        }
        this.hasYelled = true;
        monkeys.forEach(monkey => monkey.receive(this.name, this.number, monkeys))
    }

    receive(monkey, number, monkeys) {
        if (this.left == monkey) {
            this.left = number;
        }
        if (this.right == monkey) {
            this.right = number;
        }
        if (!(isNaN(this.left) || isNaN(this.right))) {
            this.number = eval(`${this.left} ${this.op} ${this.right}`);
            return this.yell(monkeys);
        }
    }
}

const solveFor = (lines) => {
    const numbers = {};
    const operations = {};
    const monkeys = [];
    for (const line of lines) {
        const [monkey, value] = line.split(': ')
        if (isNaN(value)) {
            const [left, op, right] = value.split(' ');
            operations[monkey] = [left, op, right];
            monkeys.push(new Monkey(monkey, NaN, left, op, right));
        }
        else {
            const right = +value;
            numbers[monkey] = right;
            monkeys.push(new Monkey(monkey, right));
        }
    }

    while (true)
        for (const m of monkeys) {
            const r = m.yell(monkeys)
        }
}

const solveFor2 = (lines) => {
    const numbers = {};
    const operations = {};
    const monkeys = [];
    for (const line of lines) {
        const [monkey, value] = line.split(': ')
        if (isNaN(value)) {
            operations[monkey] = value.replaceAll(' ', '');
        }
        else {
            const right = +value;
            numbers[monkey] = right;
        }
        monkeys.push(monkey);
    }
    numbers.humn = 'X';
    operations.root = operations.root.replace('+', '=');
    let lastRoot = operations.root;
    for (let i = 0; i < lines.length; i++) {
        for (const monkey of monkeys) {
            // console.log(monkey);
            if (monkey in numbers) continue;
            for (const key of monkeys) {
                if (key in numbers) {
                    operations[monkey] = operations[monkey].replaceAll(key, numbers[key])
                }
                else {
                    operations[monkey] = operations[monkey].replaceAll(key, '(' + operations[key] + ')')
                }
            }
        }
        if (operations.root == lastRoot) {
            break;
        }
        lastRoot = operations.root;
    }

    let [left, right] = operations.root.split('=')
    try { left = eval(left); } catch { }
    try { right = eval(right); } catch { }
    return { message: "Exercise for the reader: solve for x so that left == right", left, right }; 
}

const solve = (lines) => {
    let p1;
    try {
        solveFor(lines)
    }
    catch(e) {
        p1 = +e.message
    }
    return {
        p1,
        p2: solveFor2(lines),
    }
}

(async () => {
    const puzzle = await readAndSolve('21.input', solve, '\n');
    printAnswer(puzzle);
})();
