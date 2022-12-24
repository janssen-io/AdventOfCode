import { readAndSolve, printAnswer, test, manhattan, assert } from '../aoc.mjs'
import { Heap } from '../heap.mjs'

const WALL = '#';
const FREE = '.';

let maxX = 0;
let maxY = 0;
let minX = 0;
let minY = 0;
let allowSizeChange = true;

const add = (left, right) => { return { x: left.x + right.x, y: left.y + right.y } };
const directions = {
    '>': { x: 1, y: 0 },
    '<': { x: -1, y: 0 },
    'v': { x: 0, y: 1 },
    '^': { x: 0, y: -1 },
}

const setP = (grid, { x, y }, value) => set(grid, x, y, value);
function set(grid, x, y, value) {
    if (allowSizeChange) {
        if (x > maxX) maxX = x;
        if (y > maxY) maxY = y;
        if (x < minX) minX = x;
        if (y < minY) minY = y;
    }
    else {
        if (x > maxX) throw "x > maxX: " + x
        if (y > maxY) throw "y > maxY: " + y
        if (x < minX) throw "x < minX: " + x
        if (y < minY) throw "y < minY: " + y
    }

    grid[`${x}_${y}`] = value;
}

const getP = (grid, { x, y }, value = undefined) => get(grid, x, y, value);
function get(grid, x, y, value = undefined) {
    const result = grid[`${x}_${y}`];
    if (typeof (result) === 'undefined') {
        return value;
    }
    return result;
}

function parse(lines) {
    const grid = {};
    for (let y = 0; y < lines.length; y++) {
        for (let x = 0; x < lines[y].length; x++) {
            if (lines[y][x] === FREE) {
                set(grid, x, y, []);
            }
            else {
                set(grid, x, y, [lines[y][x]]);
            }
        }
    }
    return grid;
}

function show(grid, me, prev) {
    if (me && prev) {
        console.log({me, d: manhattan(me, prev)})
        assert(() => manhattan(me, prev) <= 1, "Distance too big: " + JSON.stringify({me, prev}));
    }

    if (me)
        assert(() => getP(grid, me, []).length== 0, "Ran into something: " + JSON.stringify(getP(grid, me, [])));
    for (let y = minY; y <= maxY; y++) {
        let row = ''
        for (let x = minX; x <= maxX; x++) {
            if (me && me.x == x && me.y == y) {
                row += 'E'.yellow();
            }
            else {
                const value = get(grid, x, y, '.');
                if (Array.isArray(value)) {
                    if (value.length > 1) {
                        row += value.length;
                    }
                    else if (value.length == 0) {
                        row += '.'
                    }
                    else {
                        row += value[0]
                    }
                }
                else {
                    row += value;
                }
                if (prev && prev.x == x && prev.y == y) {
                    row = row.substring(0, row.length - 1) + row[row.length - 1].red();
                }
            }
        }
        console.log(row);
    }
}

function wallCount(grid) {
    return Object.values(grid).filter(cell => Array.isArray(cell) && cell[0] == '#').length;
}

function nextStates(state, start, end) {
    let newGrid = {}
    if (gridByTime[state.time]) {
        newGrid = gridByTime[state.time];
    }
    else {
        const grid = gridByTime[state.time - 1];
        for (let x = minX; x <= maxX; x++) {
            for (let y = minY; y <= maxY; y++) {
                // Ensure grid is properly filled. Not strictly necessary,
                // but makes reasoning and troubleshooting down the line easier.
                const currentValue = getP(newGrid, { x, y }, []);
                setP(newGrid, { x, y }, currentValue);

                const currents = get(grid, x, y, []);
                if (currents.length == 0) {
                    continue; // tile was empty on the current grid, nothing to do.
                }
                for (const currentTile of currents) {
                    // Walls don't move, but we need to make sure we copy them to the new grid.
                    if (currentTile == WALL) {
                        setP(newGrid, { x, y }, [WALL])
                        continue;
                    }

                    // Any other non-empty tile can only be a blizzard
                    const direction = directions[currentTile];
                    if (!direction) throw "Non-empty array, but not a blizzard? " + currents;

                    // Check where we are moving and if that tile is a wall.
                    let targetPos = add({ x, y }, direction);
                    const targetTile = getP(grid, targetPos, []);

                    // The input contains no vertical blizzards on start.x and end.x
                    // Any move is therefore a spot inside the wall or a wall.

                    // Sanity check that blizzards really don't try to move to start and end.
                    const eql = (a, b) => a.x == b.x && a.y == b.y;
                    if (eql(targetPos, start) || eql(targetPos, end)) {
                        throw new Error("Blizzard trying to get into start/end");
                    }

                    if (targetTile[0] === WALL) {
                        // warping left, move past the wall on the same y
                        if (direction.x < 0) targetPos = { x: maxX - 1, y: targetPos.y };

                        // warping right, move past the wall on the same y
                        if (direction.x > 0) targetPos = { x: 1, y: targetPos.y };

                        // warping up, move past the wall on the same x 
                        if (direction.y < 0) { targetPos = { x: targetPos.x, y: maxY - 1 }; }

                        // warping down, move past the wall on the same x
                        if (direction.y > 0) { targetPos = { x: targetPos.x, y: 1 }; }
                    }

                    // Get the tile from the new grid. If it doesn't exist yet, it's free (empty)
                    const newTile = getP(newGrid, targetPos, []);
                    assert(() => newTile.length == 1 || !newTile.includes('#'), "Trying to push something onto a wall")
                    newTile.push(currentTile);
                    setP(newGrid, targetPos, newTile);

                }
            }
        }
        // Ensure the walls stay intact.
        const wc1 = wallCount(newGrid);
        const wc2 = wallCount(grid);
        assert(() => wc1 == wc2, "Wall count difference: " + wc1 + " vs " + wc2);
        // console.log(wc1, wc2)
        gridByTime[state.time] = newGrid;
        // show(newGrid);
        // console.log(gridByTime);
        console.log(state.time);
    }

    const states = [];
    for (let cardinalDir of Object.values(directions)) {
        const newPos = add(state.E, cardinalDir);

        if (newPos.x < minX || newPos.x > maxX || newPos.y < minY || newPos.y > maxY) continue;
        const newTile = getP(newGrid, newPos, [])

        // Empty tile = no blizzards or walls
        if (newTile.length == 0) {
            try {
                assert(
                    () => (newPos.x > 0 && newPos.x < maxX) || (newPos.x == start.x || newPos.x == end.x),
                    `Ran into a wall at: ${newPos.x},${newPos.y}`
                )
                assert(
                    () => (newPos.y > 0 && newPos.y < maxX) || (newPos.y == start.y || newPos.y == end.y),
                    `Ran into a wall at: ${newPos.x},${newPos.y}`
                )
            }
            catch (e) {
                // console.log(newGrid);
                throw e;
            }
            states.push({
                time: state.time + 1,
                E: newPos,
                p: state.p.concat(newPos)
            });
        }
    }

    // If we are surrounded by blizzards in the next minute
    // Then we must wait. 
    // if (states.length === 0) {
        // We can only wait if our waiting spot is NOT occupied by a blizzard now.
        if (getP(newGrid, state.E, []).length === 0) {
            states.push({
                time: state.time + 1,
                E: { ...state.E },
                p: state.p.concat({ ...state.E })
            });
        }
        // else: swallowed by blizzard 🥶 
        // no further states to explore.
    // }

    return states;
}

let gridByTime = {};

function simulate(start, end) {
    const init = {
        time: 1,
        E: start,
        p: [start]
    }
    const k = state => `[${state.time}] (${state.E.x},${state.E.y})`;
    const seen = new Set();

    let q = [];
    q.push(init);
    console.log(' === START === ')
    while (q.length) {
        const state = q.shift();
        // console.log(state);
        // if (state.time > maxX * maxY) break;
        const key = k(state);
        if (seen.has(key)) continue;
        seen.add(key);

        if (state.E.x == end.x && state.E.y == end.y) {
            // console.log({ p: state.p })
            return state;
        }
        const next = nextStates(state, start, end)
        // console.log(next);
        q.push(...next);
    }
    return 'no solutions found.'
}

const sleep = (time) => new Promise(resolve => setTimeout(resolve, time));

function solveFor(lines, start, end) {
    const grid = parse(lines);
    // Sanity check to see if we got the end right.
    console.log('end', getP(grid, end))
    for (const dir of Object.values(directions)) {
        console.log('neighbours', dir, getP(grid, add(end, dir)));
    }
    gridByTime = { 0: grid };

    allowSizeChange = false;

    const ans = simulate(start, end);
    animateSolution(false);
    console.log(ans.p, ans.E)
    return ans.time - 1;

    async function animateSolution(shouldWait = false) {
        let previousPos = ans.p[0];
        for (let [time, g] of Object.entries(gridByTime)) {
            console.clear();
            console.log(time);
            show(g, ans.p[time], previousPos);
            previousPos = ans.p[time];
            if (shouldWait)
                await waitKeyPressed();
        }
    }
}


const solve = (start, end) => (lines) => {
    minX = minY = maxX = maxY = 0;
    allowSizeChange = true;
    return {
        p1: solveFor(lines, start, end),
        // p2: solveFor(lines),
    }
}

function waitKeyPressed() {
    return new Promise(resolve => {
        process.stdin.resume();
        process.stdin.once("data", (data) => {
            process.stdin.pause();
            resolve(data.toString());
        });
    });
}


(async () => {
    let example = await readAndSolve('24.example.input', solve({ x: 1, y: 0 }, { x: 6, y: 5 }), '\n');
    test('Example p1', 18, example.answer.p1)
    test('Example p2', undefined, example.answer.p2)

    const puzzle = await readAndSolve('24.input', solve({ x: 1, y: 0 }, { x: 120, y: 26 }), '\n');
    printAnswer(puzzle);

    // 153: too low
    // 362, 276
})();
