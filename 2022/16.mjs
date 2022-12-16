import { readAndSolve, printAnswer, test, range } from '../aoc.mjs'

const solveFor = (lines) => {
    return lines.length;
}

const solveFor2 = (lines) => {
}

const solve = (lines) => {
    return {
        p1: solveFor(lines),
        // p2: solveFor2(lines, max),
    }
}

(async () => {
    let example = await readAndSolve('16.example.input', solve, '\n');
    test('Example p1', 5, example.answer.p1)
    test('Example p2', 5, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve('16.input', solve, '\n');
    printAnswer(puzzle);
})();