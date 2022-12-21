import { readAndSolve, printAnswer, test, search, range, assert, bfs, dfs, manhattan } from '../aoc.mjs'

// ok for part 1
function move(arr, i) {
    const num = arr[i].value;
    if (num == undefined) throw new Error(`Number is undefined at ${i} in ${arr}`)
    if (num == 0) {
        arr[i].seen = true;
        return arr;
    }
    if (num > 0) {
        arr.splice(i, 1);
        arr.splice((i + num) % arr.length, 0, { value: num, seen: true });
    }
    if (num < 0) {
        arr.splice(i, 1);
        let newIndex = i + num;
        while (newIndex < 0) newIndex += arr.length;
        arr.splice(newIndex, 0, { value: num, seen: true });
    }
    // console.log(arr.map(a => a.value))
    return arr;
}

// realize, I just had to mod arr.length for the displacement
function move3(arr, originalIndex) {
    const movedTo = getMovedIndex(arr, originalIndex);
    const num = arr[movedTo].value;
    arr.splice(movedTo, 1);
    // const numLoops = Math.abs(Math.floor(num / arr.length));
    const displaceTo = mod(movedTo + num, arr.length);
    arr.splice(displaceTo, 0, { value: num, originalIndex, seen: true });
    // console.log({ originalIndex, movedTo, num, displaceTo: movedTo + displacement, displaceToMod: displaceTo, l: arr.length });
    // console.log(arr);
    return arr;
}

function testMove2() {
    // const lines = [0, 1, 2];
    // return solveFor(lines, 4, 2);
    const lines = [1, 0, -3, 1, 1, 1, 1, 1];
    return solveFor(lines, 1, 1);
}

// keep adding n until it's greater than 0
const mod = (x, n) => x < 0 ? mod(x + 1e10* n, n) : x % n;

const getMovedIndex = (lines, i) => lines.indexOfP(l => l.originalIndex == i);

// mix between p1 and first attempt for p2.
// const solveFor = (lines, key = 1, cycles = 1) => {
//     console.log({ key, cycles, length: lines.length })
//     lines = lines.map((num, i) => { return { value: +num * key, seen: false, originalIndex: i, movedIndex: i } });
//     for (let cycle = 0; cycle < cycles; cycle++) {
//         console.log({ cycle })
//         let done = false;
//         lines = lines.map(l => { l.seen = false; return l; });
//         while (!done) {
//             let seenEverything = true
//             for (let i = 0; i < lines.length; i++) {
//                 const line = lines[indexOfOriginal(lines, i)];
//                 if (!line.seen) {
//                     seenEverything = false;
//                     try {
//                         move2(lines, i);
//                     }
//                     catch (e) {
//                         console.error(cycle, i, lines);
//                         throw e;
//                     }
//                     break;
//                 }
//             }
//             // console.log('looped', done, lines)
//             done = seenEverything;
//         }
//         console.log('sorted:', lines.numSortBy(l => l.movedIndex).map(l => l.value))
//     }
//     lines = lines.numSortBy(l => l.movedIndex);
//     console.log('final', lines)
//     const index0 = lines.indexOfP(a => a.value == 0);
//     const result = [1000, 2000, 3000]
//         .map(i => (i + index0) % lines.length)
//         .map(i => lines[i].value)
//         .sum();
//     console.log({ index0, result })
//     return result;
//     // return lines[index0]
//     // return lines
// }

const solveFor = (lines, key = 1, cycles = 1, loops = 0) => {
    console.log({ key, cycles, length: lines.length })
    lines = lines.map((n, i) => { return { value: +n * key + (+n < 0 ? -loops : loops) * lines.length, originalIndex: i } });
    for (let cycle = 0; cycle < cycles; cycle++) {
        lines.forEach(l => l.seen = false);
        console.log({ cycle })
        for (let i = 0; i < lines.length; i++) {
            try {
                move3(lines, i);
            }
            catch (e) {
                console.error(cycle, i, lines);
                throw e;
            }
        }
        console.log('looped', cycle, lines)
    }
    const index0 = lines.indexOfP(a => a.value == 0);
    const result = [1000, 2000, 3000]
        .map(i => (i + index0) % lines.length)
        .map(i => (lines[i].value < 0 ? loops : -loops) * lines.length + lines[i].value)
        .sum();
    console.log({ index0, result })
    return result;
    // return lines[index0]
    // return lines
}


const solve = (lines) => {
    return {
        p1: solveFor(lines, 1, 1, 0),
        p2: solveFor(lines, 811589153, 10)
    }
}

(async () => {
    // return testMove2();
    let example = await readAndSolve('20.example.input', solve, '\n');
    test('Example p1', 3, example.answer.p1)
    test('Example p2', 1623178306, example.answer.p2)

    const puzzle = await readAndSolve('20.input', solve, '\n');
    printAnswer(puzzle);
})();
