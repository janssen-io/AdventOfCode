import { readAndSolve, printAnswer, test } from '../aoc.mjs'

function solveFor(lines) {
    // Lesson from yesterday: don't use global variables.
    // If we need them today, let's try to only scope everything
    // to the `solveFor` method. That is, use local functions
    // or pass all dependencies explicitly.
    // This ensures that every call to `solveFor` uses 'fresh'
    // variables.

}

const solve = (lines) => {
    return {
        p1: solveFor(lines),
        // p2: solveFor(lines),
    }
}

(async () => {
    const day = 25;
    let example = await readAndSolve(`${day}.example.input`, solve, '\n');
    test('Example p1', undefined, example.answer.p1)
    // test('Example p2', undefined, example.answer.p2)

    const puzzle = await readAndSolve(`${day}.input`, solve, '\n');
    printAnswer(puzzle);
})();
