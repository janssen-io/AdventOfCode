import { readAndSolve, printAnswer, test, search, range, assert, bfs, dfs } from '../aoc.mjs'
import { Heap } from '../heap.mjs'

const solveFor = (lines) => {
    return lines;
}

const solve = (lines) => {
    return solveFor(lines);
}

(async () => {
    let example = await readAndSolve('19.example.input', solve, '\n');
    test('Example p1', 64, example.answer.p1)
    test('Example p2', 58, example.answer.p2)

    const puzzle = await readAndSolve('19.input', solve, '\n');
    printAnswer(puzzle);
})();
