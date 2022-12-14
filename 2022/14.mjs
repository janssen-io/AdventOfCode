import { readAndSolve, printAnswer, test } from '../aoc.mjs'


const ROCK = '#'; // '█';
const SAND = 'o';
const FREE = '.';

let floor_y = 0;
function readGrid(lines) {
    const grid = {};
    const coords = lines.map(line =>
        line
            .split(' -> ')
            .map(pairs => pairs.split(','))
            .map(([x, y]) => {
                floor_y = Math.max(floor_y, +y + 2);
                return { x: +x, y: +y }
            })
    );

    let previous = undefined;
    for (let corners of coords) {
        for (let corner of corners) {
            if (previous == undefined) {
                grid.setCell(corner.x, corner.y, ROCK);
            }
            else {
                const delta = {
                    x: Math.sign(corner.x - previous.x),
                    y: Math.sign(corner.y - previous.y)
                };
                for (let x = previous.x; x != corner.x + delta.x; x += delta.x) {
                    grid.setCell(x, corner.y, ROCK);
                }
                for (let y = previous.y; y != corner.y + delta.y; y += delta.y) {
                    grid.setCell(corner.x, y, ROCK);
                }
            }
            previous = corner;
        }
        previous = undefined;
    }
    return grid;
}

function spawnSand(grid, origin) {
    if (grid.dimensions.min_x > origin.x
        || grid.dimensions.max_x < origin.x
        || grid.dimensions.min_y > origin.y
        || grid.dimensions.max_y < origin.y) {
        // console.log(origin)
        // return true;
    }
    for (let y = origin.y + 1; ; y++) {
        if (y == floor_y - 1) {
            for(let x = origin.x - 10; x <= origin.x + 10; x++)
            grid.setCell(x, floor_y, ROCK);
        }
        if (grid.getCell(origin.x, y, FREE) === FREE) {
            continue;
        }
        // current x,y is occupied; check left
        if (grid.getCell(origin.x - 1, y, FREE) === FREE
            // && grid.getCell(origin.x - 1, y - 1, FREE) === FREE) {
        ){
            return spawnSand(grid, { x: origin.x - 1, y });
        }
        // current x,y and left is occupied; check right
        else if (grid.getCell(origin.x + 1, y, FREE) === FREE
            // && grid.getCell(origin.x + 1, y - 1, FREE) === FREE) {
        ){
            return spawnSand(grid, { x: origin.x + 1, y });
        }
        // current x,y and left and right is occupied; place on top.
        else {
            grid.setCell(origin.x, y - 1, SAND);
            if (origin.x == 500 && y - 1 == 1) {
                return true;
            }
            break;
        }

    }
    return false;
}

const solveP1 = (lines) => {
    const grid = readGrid(lines);
    const sandOrigin = { x: 500, y: 0 };
    grid.setCell(sandOrigin.x, sandOrigin.y, '+');

    for (let i = 0; i < 10_000_000_000_000; i++) {
        if (spawnSand(grid, sandOrigin)) {
            // console.log('\n', grid.showGrid(FREE))
            return i;
        }

    }

    return '\n' + grid.showGrid(FREE);
}

const solveP2 = (lines) => {
    return lines
}

const solve = (lines) => {
    return {
        p1: solveP1(lines),
        // p2: solveP2(lines),
    }
}

(async () => {
    let example = await readAndSolve('14.example.input', solve, '\n');
    test('Example p1', 24, example.answer.p1)
    // test('Example p2', 140, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve('14.input', solve, '\n');
    // printAnswer(puzzle);
    console.log(puzzle.answer.p1)
})();