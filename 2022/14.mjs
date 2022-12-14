import { readAndSolve, printAnswer, test } from '../aoc.mjs'


const ROCK = '#'; // '█';
const SAND = 'o';
const FREE = '.';
const SAND_ORIGIN = { x: 500, y: 0 };

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
                // could probably order them from low -> high, but this also works.
                const delta = {
                    x: Math.sign(corner.x - previous.x),
                    y: Math.sign(corner.y - previous.y)
                };
                // off-by-one: it should go up to AND INCLUDING the corner.
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

function spawnSand(grid, origin, spawnFloor = false) {
    if (grid.dimensions.min_x > origin.x || grid.dimensions.max_x < origin.x) {
        // p1: We went farther than the most left|right rock.
        //     We fall into the abyss/void/eternal sadness.
        if (!spawnFloor) return true;
    }
    for (let y = origin.y; ; y++) {
        // p2: But wait, there's a floor. And we are standing on it!
        if (y == floor_y) {
            // put one to the left and right, so we can check its neighbours too.
            grid.setCell(origin.x, floor_y, ROCK);
            grid.setCell(origin.x - 1, floor_y, ROCK);
            grid.setCell(origin.x + 1, floor_y, ROCK);
        }

        // We're falling
        if (grid.getCell(origin.x, y, FREE) === FREE) continue;

        // Current x,y is occupied; check left
        if (grid.getCell(origin.x - 1, y, FREE) === FREE)
            return spawnSand(grid, { x: origin.x - 1, y }, spawnFloor);

        // Left is also occupied; check right
        else if (grid.getCell(origin.x + 1, y, FREE) === FREE)
            return spawnSand(grid, { x: origin.x + 1, y }, spawnFloor);

        // Both left and right are occupied; place on top.
        else {
            grid.setCell(origin.x, y - 1, SAND);
            // p2: reached the sand origin. Success!
            if (origin.x == SAND_ORIGIN.x && y - 1 == SAND_ORIGIN.y) {
                return true;
            }
            break;
        }

    }
    return false;
}

const solveFor = (lines, spawnFloor) => {
    const grid = readGrid(lines);

    // Place one higher, so it does not interfere with the simulation
    grid.setCell(SAND_ORIGIN.x, SAND_ORIGIN.y - 1, '+');

    let unitsSpawned = 1;
    while(!spawnSand(grid, SAND_ORIGIN, spawnFloor)) {
        unitsSpawned++;
    }

    return unitsSpawned;
    // return '\n' + grid.showGrid(FREE);
}

const solve = (lines) => {
    return {
        // p1: - 1, because we should not count the last spawned unit. That one fell into the void.
        p1: solveFor(lines, false) - 1,
        p2: solveFor(lines, true),
    }
}

(async () => {
    let example = await readAndSolve('14.example.input', solve, '\n');
    test('Example p1', 24, example.answer.p1)
    test('Example p2', 93, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve('14.input', solve, '\n');
    printAnswer(puzzle);
})();