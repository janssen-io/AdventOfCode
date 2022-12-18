import { readAndSolve, printAnswer, test, range, assert, bfs, dfs } from '../aoc.mjs'

const AXES = ['x', 'y', 'z'];
function setUnexposedSides(a, b) {
    const theSame = a.map((c, i) => c == b[i]);
    const oneDiff = a.map((c, i) => c - b[i]);
    const axisIndex = oneDiff.indexOfP(x => x == 1 || x == -1);
    if (theSame.filter(x => x).length == 2 && axisIndex !== -1) {
        const axis = AXES[axisIndex];
        
        a.unexposedSides[axis].push(oneDiff[axisIndex]);
        b.unexposedSides[axis].push(oneDiff[axisIndex] * -1);
    }
    return theSame.filter(x => x).length == 2 && oneDiff.filter(x => x).length == 1;
}

const eql = ([a, b, c], [x, y, z]) => a == x && b == y && c == z;
const between = (low, current, high) => low <= current && current <= high;
const getKey = (cube) => cube.join(',');
const inBounds = (bounds, cube, offset = 0) =>
    between(bounds.min_x - offset, cube[0], bounds.max_x + offset)
    && between(bounds.min_y - offset, cube[1], bounds.max_y + offset)
    && between(bounds.min_z - offset, cube[2], bounds.max_z + offset);

const getAllSides = ([x, y, z]) => [
    [x - 1, y, z],
    [x + 1, y, z],
    [x, y - 1, z],
    [x, y + 1, z],
    [x, y, z - 1],
    [x, y, z + 1],
];

function isReachable(cube, bounds, cubeGrid) {
    const outside = [bounds.min_x - 1, bounds.min_y - 1, bounds.min_z - 1];
    const isMatch = (state) => eql(state, outside)
    let show = (state) => state;
    show = false;

    const genStates = function (state) {
        return getAllSides(state).filter((side) =>
            // We can only go to a cube if it's free or if it's our destination
            (!cubeGrid[getKey(side)] || eql(side, cube))
            && inBounds(bounds, side, 1));
    }

    const solutions = dfs([cube], genStates, isMatch, getKey, show);
    return !!solutions.next().value;
}

function getExposedSides(cube) {
    const sides = [];
    for (const [axis, i] of AXES.withIndex()) {
        if (cube.unexposedSides[axis].length == 0) {
            const side = cube.map(x => x);
            side[i] += 1;
            sides.push(side);
            side[i] -= 2;
            sides.push(side);
        }
    }
    // console.log({exposed: sides})
    return sides;
}

const numberOfUnexposedSides = (cube) => Object.values(cube.unexposedSides).map(side => side.length).sum();

const solveFor = (lines) => {
    let cubes = lines.map(line => line.split(',').map(x => +x));
    let cubeGrid = cubes.reduce((grid, cube) => {
        grid[getKey(cube)] = true;
        return grid;
    }, {})
    // naive, compare all cubes with ones further down the line to set unexposed sides; 
    cubes.forEach(cube => cube.unexposedSides = { x: [], y: [], z: [] });
    const bounds = {
        min_x: cubes[0][0],
        max_x: cubes[0][0],
        min_y: cubes[0][1],
        max_y: cubes[0][1],
        min_z: cubes[0][2],
        max_z: cubes[0][2],
    }
    for (let i = 0; i < cubes.length; i++) {
        updateBounds(cubes, i, bounds);
        for (let j = i; j < cubes.length; j++) {
            setUnexposedSides(cubes[i], cubes[j]);
        }
    }

    /* === p1 === */
    const numberOfUnexposedSidesPerCube = cubes.map(numberOfUnexposedSides);
    const p1 = cubes.length * 6 - numberOfUnexposedSidesPerCube.sum();

    /* === p2 === */
    let p2 = p1;
    const pockets = {};
    for (let side of cubes.flatMap(getExposedSides)) {
        if (pockets[getKey(side)]) {
            p2--;
            continue;
        }

        // out of bounds is always reachable
        if (!inBounds(bounds, side)) continue;
        if (isReachable(side, bounds, cubeGrid)) continue;

        pockets[getKey(side)] = true;
        console.log('pocket', side)
        p2--;
    }
    return { p1, p2 };
}

const solve = (lines) => {
    return solveFor(lines);
}

(async () => {
    let example = await readAndSolve('18.bart.input', solve, '\n');
    test('Example p1', 64, example.answer.p1)
    test('Example p2', 58, example.answer.p2)

    // 3018 too high
    const puzzle = await readAndSolve('18.input', solve, '\n');
    printAnswer(puzzle);
})();
function updateBounds(cubes, i, bounds) {
    if (cubes[i][0] < bounds.min_x) { bounds.min_x = cubes[i][0]; }
    if (cubes[i][1] < bounds.min_y) { bounds.min_y = cubes[i][1]; }
    if (cubes[i][2] < bounds.min_z) { bounds.min_z = cubes[i][2]; }
    if (cubes[i][0] > bounds.max_x) { bounds.max_x = cubes[i][0]; }
    if (cubes[i][1] > bounds.max_y) { bounds.max_y = cubes[i][1]; }
    if (cubes[i][2] > bounds.max_z) { bounds.max_z = cubes[i][2]; }
}

