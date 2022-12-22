import { readAndSolve, printAnswer, test, assert } from '../aoc.mjs'

const WARP = ' ';
const OPEN = '.';
const WALL = '#';

let maxX = 0;
let maxY = 0;
function set(grid, x, y, value) {
    if (x > maxX) maxX = x;
    if (y > maxY) maxY = y;

    grid[`${x}_${y}`] = value;
}

function get(grid, x, y, value = undefined) {
    const result = grid[`${x}_${y}`];
    if (typeof(result) === 'undefined') {
        return value;
    }
    return result;
}

function parse(lines) {
    const grid = {};
    for (let y = 0; y < lines.length; y++) {
        for (let x = 0; x < lines[y].length; x++) {
            set(grid, x, y, lines[y][x]);
        }
    }
    return grid;
}

function show(grid) {
    for(let y = 0; y <= maxY; y++) {
        let row = ''
        for(let x = 0; x <= maxX; x++) {
            row += get(grid, x, y, ' ');
        }
        console.log(row);
    }
}

const eql = (left, right) => { return      left.x == right.x &&  left.y == right.y; };
const add = (left, right) => { return { x: left.x  + right.x, y: left.y  + right.y } };
const sub = (left, right) => { return { x: left.x  - right.x, y: left.y  - right.y } };
const E = { x: 1, y: 0 };
const S = { x: 0, y: 1 };
const W = { x: -1, y: 0 };
const N = { x: 0, y: -1 };

const rotate = (rotation, direction) => {
    if (rotation == 'R') {
        if (eql(direction, N)) return E;
        if (eql(direction, E)) return S;
        if (eql(direction, S)) return W;
        if (eql(direction, W)) return N;
    }
    else if (rotation == 'L') {
        if (eql(direction, N)) return W;
        if (eql(direction, E)) return N;
        if (eql(direction, S)) return E;
        if (eql(direction, W)) return S;
    }
    throw 'Invalid rotation: ' + rotate;
}

const toArrow = (direction) => {
    if (eql(direction, N)) return '^';
    if (eql(direction, E)) return '>';
    if (eql(direction, S)) return 'v';
    if (eql(direction, W)) return '<';
}

function walk(grid, pos, instructions) {
    const debugGrid = {...grid};
    set(debugGrid, pos.x, pos.y, toArrow(pos.r).green());
    for (const [distance, i] of instructions.distances.withIndex()) {
        console.log({distance, i, rotation: instructions.rotations[i]})
        // first we walk
        for (let d = 0; d < distance; d++) {
            let nextPos = add(pos, pos.r);
            const nextTile = get(grid, nextPos.x, nextPos.y, WARP);
            if (nextTile === WALL) {
                break;
            }
            else if (nextTile === WARP) {
                // warping from right to left side
                if (pos.r.x > 0) { nextPos.x = 0; }
                // warping from left to right side
                else if (pos.r.x < 0) { nextPos.x = maxX; }

                // warping from bottom to top side
                if (pos.r.y > 0) { nextPos.y = 0; }
                // warping from top to bottom side
                else if (pos.r.y < 0) { nextPos.y = maxY; }

                let warpedTile = get(grid, nextPos.x, nextPos.y, WARP)

                // Find a tile on the other side
                while (warpedTile === WARP) {
                    nextPos = add(nextPos, pos.r);
                    warpedTile = get(grid, nextPos.x, nextPos.y, WARP)
                }

                // If the other side is wall, don't move there
                if (warpedTile == WALL) {
                    break;
                }
            }
            // show(grid)
            pos.x = nextPos.x;
            pos.y = nextPos.y;
            set(debugGrid, pos.x, pos.y, toArrow(pos.r));
            assert(
                () => get(grid, pos.x, pos.y, WARP) !== WARP,
                `We walked into the void (${get(grid, pos.x, pos.y)}) on turn ${i}: ${JSON.stringify(pos)}`)
        }

        // then we rotate
        if (instructions.rotations[i]) {
            pos.r = rotate(instructions.rotations[i], pos.r);
        }
        else {
            console.log('end of input', i);
        }
    }
    set(debugGrid, pos.x, pos.y, toArrow(pos.r).red());
    show(debugGrid);
    return pos;
}

function password(pos) {
    let facing;
    if (eql(E, pos.r)) facing = 0;
    if (eql(S, pos.r)) facing = 1;
    if (eql(W, pos.r)) facing = 2;
    if (eql(N, pos.r)) facing = 3;
    console.log({ row: pos.y + 1, column: pos.x + 1, facing, r: pos.r });
    return 1000 * (pos.y + 1) + 4 * (pos.x + 1) + facing;
}

function solveFor(lines) {
    const grid = parse(lines[0].split('\n'));
    const instructions = {
        distances: lines[1].split(/[RL]/).map(x => +x),
        rotations: lines[1].split(/\d+/).filter(x => x)
    }
    console.log({instructions})

    let pos = {
        x: 0,
        y: 0,
        r: E
    }
    while (get(grid, pos.x, pos.y) !== OPEN) {
        pos.x++;
    }
    console.log({maxX, maxY})
    pos = walk(grid, pos, instructions);
    return password(pos);
}


const solve = (lines) => {
    return {
        p1: solveFor(lines),
        // p2: solveFor(lines),
    }
}

(async () => {
    let example = await readAndSolve('22.example.input', solve, '\n\n');
    test('Example p1', 6032, example.answer.p1)
    test('Example p2', undefined, example.answer.p2)
    assert(() => example.answer.p1 == 6032, "Example p1 failed")

    // too high: 156098
    // too high: 173006
    const puzzle = await readAndSolve('22.input', solve, '\n\n');
    printAnswer(puzzle);
})();
