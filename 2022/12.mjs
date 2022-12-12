import { bfs, readAndSolve, printAnswer, test } from '../aoc.mjs'

function readGrid(lines, startingPoints) {
    const grid = {
        dimensions: {
            min_x: 0, min_y: 0,
            max_x: lines[0].length,
            max_y: lines.length
        }
    }

    let startingCoords = [];
    for (let y = 0; y < grid.dimensions.max_y; y++) {
        for (let x = 0; x < grid.dimensions.max_x; x++) {
            grid.setCell(x, y, lines[y][x]);
            if (lines[y][x] == 'S') {
                grid.setCell(x, y, 'a');
            }
            if (startingPoints.includes(lines[y][x])) {
                startingCoords.push({ x, y });
            }
        }
    }
    return { grid, startingCoords };
}

function delta(grid, a, b) {
    try {
        const h_a = grid.getCell(a.x, a.y).codePointAt();
        let h_b = grid.getCell(b.x, b.y).codePointAt();
        if (h_b == 'E'.codePointAt()) h_b = 'z'.codePointAt();
        return h_b - h_a;
    }
    catch (e) {
        console.log({ a, b });
        throw e;
    }
}

function getNeighbours(grid) {
    return function* ({ x, y }) {
        const left = { x: x - 1, y };
        const right = { x: x + 1, y }
        const up = { y: y - 1, x }
        const down = { y: y + 1, x }
        if (x > grid.dimensions.min_x && delta(grid, { x, y }, left) <= 1) {
            yield left;
        }
        if (x < grid.dimensions.max_x - 1 && delta(grid, { x, y }, right) <= 1) {
            yield right;
        }
        if (y > grid.dimensions.min_y && delta(grid, { x, y }, up) <= 1) {
            yield up;
        }
        if (y < grid.dimensions.max_y - 1 && delta(grid, { x, y }, down) <= 1) {
            yield down;
        }
    }
}

const solveFor = (lines, startingPoints) => {
    const grid = readGrid(lines, startingPoints);
    const heightMap = grid.grid;
    const genStates = getNeighbours(heightMap);
    const isMatch = ({ x, y }) => heightMap.getCell(x, y) === 'E';
    const key = ({ x, y }) => `x${x},y${y}`;
    let show = x => x;
    show = false;

    return grid.startingCoords.map(start => {
        const solutionIter = bfs([start], genStates, isMatch, key, show)
        const top = solutionIter.next();
        if (top.value.length || top.value.state) {
            return top.value.state.depth;
        }
    }).filter(x => x).numSort()[0];
}

const solve = (lines) => {
    return { p1: solveFor(lines, ['S']), p2: solveFor(lines, ['S', 'a']) } //, p2: solveFor(lines) }
}

(async () => {
    let example = await readAndSolve('12.example.input', solve, '\n');
    let puzzle = await readAndSolve(process.argv[2] || '12.input', solve, '\n');

    test('example p1', 31, example.answer.p1)
    test('example p2', 29, example.answer.p2)
    console.log(puzzle);
})();