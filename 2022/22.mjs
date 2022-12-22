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

function warpLinear(grid, pos) {
    let warpedPos = add(pos, pos.r);

    // warping from right to left side
    if (pos.r.x > 0) { warpedPos.x = 0; }
    // warping from left to right side
    else if (pos.r.x < 0) { warpedPos.x = maxX; }

    // warping from bottom to top side
    if (pos.r.y > 0) { warpedPos.y = 0; }
    // warping from top to bottom side
    else if (pos.r.y < 0) { warpedPos.y = maxY; }

    let warpedTile = get(grid, warpedPos.x, warpedPos.y, WARP)

    // Find a tile on the other side
    while (warpedTile === WARP) {
        warpedPos = add(warpedPos, pos.r);
        warpedTile = get(grid, warpedPos.x, warpedPos.y, WARP)
    }

    // If the other side is wall, don't move there
    const canWarp = warpedTile !== WALL;
    return { canWarp, warpedPos };
}

function warpCubicExample(grid, pos) {
    const faceSize = (maxY + 1) / 3;
    /* determine what face we are on
            1
        2-3-4
            5-6
    */
    // Y
    // 1: y < faceSize;
    // 2,3,4: faceSize <= y < faceSize * 2
    // 5,6: faceSize * 2 <= y

    // X
    // 1,4,5: faceSize * 2 <= x < faceSize * 3
    // 2: x < faceSize
    // 3: faceSize <= x < faceSize * 2
    // 6: faceSize * 3 <= x

    const faceX = {
        1: [faceSize * 2, faceSize * 3],
        2: [faceSize * 0, faceSize * 1],
        3: [faceSize * 1, faceSize * 2],
        4: [faceSize * 2, faceSize * 3],
        5: [faceSize * 2, faceSize * 3],
        6: [faceSize * 3, faceSize * 4]
    }

    const faceY = {
        1: [faceSize * 0, faceSize * 1],
        2: [faceSize * 1, faceSize * 2],
        3: [faceSize * 1, faceSize * 2],
        4: [faceSize * 1, faceSize * 2],
        5: [faceSize * 2, faceSize * 3],
        6: [faceSize * 2, faceSize * 3],
    }

    let face = 0;
    for(let i = 1; i <= 6; i++) {
        if (faceX[i][0] <= pos.x && pos.x < faceX[i][1]
            && faceY[i][0] <= pos.y && pos.y < faceY[i][1]) {
            face = i;
            break;
        }
    }
    console.log({faceSize, pos, faceX, faceY});
    assert(() => face != 0, "No Face");

    // cubic warps:
    // 1R -> 6(right) + rotate 2R
    // 4R -> 6(top) + rotate R
    // 6R -> 1(left) + rotate 2R

    // 1L -> 3(top) + rotate L
    // 2L -> 6(bottom) + rotate R
    // 5L -> 3(bottom) + rotate R

    // 1U -> 2(top) + rotate 2R
    // 2U -> 1(top) + rotate 2R
    // 3U -> 1(left) + rotate R
    // 6U -> 4(right) + rotate L

    // 2D -> 5(bottom) + rotate 2R
    // 3D -> 5(left) + rotate L
    // 5D -> 1(left) + rotate R
    // 6D -> 2(left) + rotate L

    // cornerDistance = current pos % faceSize;
    const getFaceEdge = (faceId, edge, cornerDistance) => {
        switch(edge) {
            case 'top':
                return {
                    x: faceX[faceId][0] + (faceSize - cornerDistance - 1),
                    y: faceY[faceId][0],
                    r: { x: 0, y: 1 },
                }
            case 'right':
                return {
                    x: faceX[faceId][0],
                    y: faceY[faceId][0] + (faceSize - cornerDistance - 1),
                    r: { x: -1, y: 0 },
                }
            case 'bottom':
                return {
                    x: faceX[faceId][0] + (faceSize - cornerDistance - 1),
                    y: faceY[faceId][1] - 1,
                    r: { x: 0, y: -1 },
                }
            case 'left':
                return {
                    x: faceX[faceId][0],
                    y: faceY[faceId][0] + (faceSize - cornerDistance - 1),
                    r: { x: 1, y: 0 },
                }
            default:
                throw 'Unexpected edge: ' + edge;
        }
    }

    let warpedPos;
    switch(face) {
        case 1:
            if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(6, 'right', pos.y % faceSize)
            }
            else if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(3, 'top', pos.y % faceSize)
            }
            else if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(2, 'top', pos.x % faceSize)
            }
            break
        case 2:
            if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(6, 'bottom', pos.y % faceSize)
            }
            else if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(1, 'top', pos.x % faceSize)
            }
            else if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(5, 'bottom', pos.x % faceSize)
            }
            break
        case 3:
            if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(1, 'left', (faceSize - (pos.x % faceSize)) - 1)
            }
            else if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(5, 'left', pos.x % faceSize)
            }
            break
        case 4:
            if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(6, 'top', pos.y % faceSize)
            }
            break
        case 5:
            if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(3, 'bottom', pos.y % faceSize)
            }
            else if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(2, 'bottom', pos.x % faceSize)
            }
            break
        case 6:
            if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(1, 'left', pos.y % faceSize)
            }
            else if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(4, 'top', pos.x % faceSize)
            }
            else if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(2, 'left', pos.x % faceSize)
            }
            break
    }
    console.log({warpedPos, face, r: pos.r})

    let warpedTile = get(grid, warpedPos.x, warpedPos.y, WARP)
    assert(() => warpedTile !== WARP, "Warped into the void");

    // If the other side is wall, don't move there
    const canWarp = warpedTile !== WALL;
    return { canWarp, warpedPos };
}

function warpCubic(grid, pos) {
    const faceSize = (maxX + 1) / 3;
    /* determine what face we are on
          1-2
          3
        5-4
        6
    */

    const faceX = {
        1: [faceSize * 1, faceSize * 2],
        2: [faceSize * 2, faceSize * 3],
        3: [faceSize * 1, faceSize * 2],
        4: [faceSize * 1, faceSize * 2],
        5: [faceSize * 0, faceSize * 1],
        6: [faceSize * 0, faceSize * 1],
    }

    const faceY = {
        1: [faceSize * 0, faceSize * 1],
        2: [faceSize * 0, faceSize * 1],
        3: [faceSize * 1, faceSize * 2],
        4: [faceSize * 2, faceSize * 3],
        5: [faceSize * 2, faceSize * 3],
        6: [faceSize * 3, faceSize * 4],
    }

    let face = 0;
    for(let i = 1; i <= 6; i++) {
        if (faceX[i][0] <= pos.x && pos.x < faceX[i][1]
            && faceY[i][0] <= pos.y && pos.y < faceY[i][1]) {
            face = i;
            break;
        }
    }
    console.log({faceSize, pos, faceX, faceY});
    assert(() => face != 0, "No Face");

    const getFaceEdge = (faceId, edge, cornerDistance) => {
        switch(edge) {
            case 'top':
                return {
                    x: faceX[faceId][0] + cornerDistance,
                    y: faceY[faceId][0],
                    r: { x: 0, y: 1 },
                }
            case 'right':
                return {
                    x: faceX[faceId][0],
                    y: faceY[faceId][0] + cornerDistance,
                    r: { x: -1, y: 0 },
                }
            case 'bottom':
                return {
                    x: faceX[faceId][0] + cornerDistance,
                    y: faceY[faceId][1] - 1,
                    r: { x: 0, y: -1 },
                }
            case 'left':
                return {
                    x: faceX[faceId][0],
                    y: faceY[faceId][0] + cornerDistance,
                    r: { x: 1, y: 0 },
                }
            default:
                throw 'Unexpected edge: ' + edge;
        }
    }

    let warpedPos;
    const regular = {
        x: pos.x % faceSize,
        y: pos.y % faceSize,
    }
    const inverse = {
        x: faceSize - 1 - (pos.x % faceSize),
        y: faceSize - 1 - (pos.y % faceSize),
    }
    switch(face) {
        case 1:
            if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(6, 'left', regular.x)
            }
            else if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(5, 'left', inverse.y)
            }
            break
        case 2:
            if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(6, 'bottom', regular.x)
            }
            else if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(4, 'right', inverse.y)
            }
            else if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(3, 'right', regular.x)
            }
            break
        case 3:
            if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(5, 'top', regular.y);
            }
            else if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(2, 'bottom', regular.y);
            }
            break
        case 4:
            if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(2, 'right', inverse.y)
            }
            if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(6, 'right', regular.x)
            }
            break
        case 5:
            if (eql(N, pos.r)) {
                warpedPos = getFaceEdge(3, 'left', regular.x)
            }
            else if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(1, 'left', inverse.y)
            }
            break
        case 6:
            if (eql(E, pos.r)) {
                warpedPos = getFaceEdge(4, 'bottom', regular.y)
            }
            else if (eql(S, pos.r)) {
                warpedPos = getFaceEdge(2, 'right', regular.x)
            }
            if (eql(W, pos.r)) {
                warpedPos = getFaceEdge(1, 'top', inverse.y)
            }
            break
    }
    console.log({warpedPos, face, r: pos.r})

    let warpedTile = get(grid, warpedPos.x, warpedPos.y, WARP)
    assert(() => warpedTile !== WARP, "Warped into the void");

    // If the other side is wall, don't move there
    const canWarp = warpedTile !== WALL;
    return { canWarp, warpedPos };
}

function walk(grid, pos, instructions, warpFrom) {
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
                const { canWarp, warpedPos} = warpFrom(grid, pos);
                if (!canWarp) break;
                nextPos = warpedPos;
            }
            // show(grid)
            pos.x = nextPos.x;
            pos.y = nextPos.y;
            pos.r = nextPos.r || pos.r;
            set(debugGrid, pos.x, pos.y, toArrow(pos.r));
            assert(
                () => get(grid, pos.x, pos.y, WARP) !== WARP,
                `We walked into the void (${get(grid, pos.x, pos.y)}) on turn ${i}: ${JSON.stringify(pos)}`)
        }

        // then we rotate
        if (instructions.rotations[i]) {
            pos.r = rotate(instructions.rotations[i], pos.r);
            set(debugGrid, pos.x, pos.y, toArrow(pos.r));
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

function solveFor(lines, warpFrom) {
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
    pos = walk(grid, pos, instructions, warpFrom);
    return password(pos);
}


const solve = (cubicWarper) => (lines) => {
    return {
        p1: solveFor(lines, warpLinear),
        p2: solveFor(lines, cubicWarper),
    }
}

(async () => {
    let example = await readAndSolve('22.example.input', solve(warpCubicExample), '\n\n');
    test('Example p1', 6032, example.answer.p1)
    test('Example p2', 5031, example.answer.p2)
    assert(() => example.answer.p1 == 6032, "Example p1 failed")
    assert(() => example.answer.p2 == 5031, "Example p2 failed")

    // too high: 156098
    // too high: 173006

    // too low: 123130
    const puzzle = await readAndSolve('22.input', solve(warpCubic), '\n\n');
    printAnswer(puzzle);
})();
