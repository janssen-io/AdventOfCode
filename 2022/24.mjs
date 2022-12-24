import { readAndSolve, printAnswer, test } from '../aoc.mjs'


function solveFor(lines) {
}


const solve = (lines) => {
    return {
        p1: solveFor(lines),
        // p2: solveFor(lines),
    }
}

(async () => {
    let example = await readAndSolve('24.example.input', solve, '\n');
    test('Example p1', undefined, example.answer.p1)
    test('Example p2', undefined, example.answer.p2)

    const puzzle = await readAndSolve('24.input', solve, '\n');
    printAnswer(puzzle);
})();
