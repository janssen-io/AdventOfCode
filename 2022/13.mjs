import { readAndSolve, printAnswer, test } from '../aoc.mjs'

function compare(leftItem, rightItem, debug) {
    if (debug) console.log({leftItem, rightItem})
    if (leftItem === undefined) {
        if (rightItem === undefined) return 0;
        console.log("left ran out")
        return 1;
    }
    else if (rightItem === undefined) {
        console.log("right ran out")
        return -1;
    }

    if (Array.isArray(leftItem) && Array.isArray(rightItem)) {
        if (leftItem.length === 0) {
            if (rightItem.length === 0) return 0;
            return 1;
        }
        let result = 0;
        for(let i = 0; i <= leftItem.length && result === 0; i++) {
            result = compare(leftItem[i], rightItem[i], debug);
        }
        
        return result;
    }
    if (Number.isInteger(leftItem) && Number.isInteger(rightItem)) {
        if (+leftItem === +rightItem) return 0;
        if (+leftItem < +rightItem) return 1;
        if (+leftItem > +rightItem) return -1;
        else throw new Error({leftItem, rightItem, cmp: leftItem > rightItem});
    }
    if (Array.isArray(leftItem) && Number.isInteger(rightItem)) {
        return compare(leftItem, [rightItem], debug);
    }
    if (Number.isInteger(leftItem) && Array.isArray(rightItem)) {
        return compare([leftItem], rightItem, debug);
    }
}

const solveFor = (lines) => {
    const indices = [];
    for(let index = 0; index < lines.length; index++) {
        const pair = lines[index];
        const left_right = pair.split('\n');
        const [left, right] = left_right.map(eval);

        let result = 0;
        for(let i = 0; i <= left.length && result === 0; i++) {
            result = compare(left[i], right[i], false);
        }
        // console.log({index, result}, '\n')
        if(result != -1) {
            indices.push(index + 1) // +1 because data is 1-indexed;
        }
    }
    console.log(indices)
    return indices.sum();
}

const solve = (lines) => {
    return { 
        p1: solveFor(lines),
        // p2: solveFor(lines)
    }
}

(async () => {
    let example = await readAndSolve('13.example.input', solve, '\n\n');
    test('Example p1', 13, example.answer.p1)
    // test('Example p2', 29, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve(process.argv[2] || '13.input', solve, '\n\n');
    test('Incorrect p1 (should be red)', 5742, puzzle.answer.p1)
    test('Incorrect p1 (should be red)', 2853, puzzle.answer.p1)
    test('Incorrect p1 (should be red)', 3773, puzzle.answer.p1)
    printAnswer(puzzle);
})();