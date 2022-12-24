import { readAndSolve, printAnswer, test, manhattan } from '../aoc.mjs'
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
const BLIZZARD = Object.keys(directions);

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

function show(grid, me) {
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
            }
        }
        console.log(row);
    }
}

function nextStates(state) {
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
                const currentValue = getP(newGrid, {x, y}, []);
                setP(newGrid, {x, y}, currentValue);

                const currents = get(grid, x, y, []);
                if (currents.length == 0) {
                    continue; // tile was empty on the current grid, nothing to do.
                }
                for (const current of currents) {
                    // Walls don't move, but we need to make sure we copy them to the new grid.
                    if (current == WALL) {
                        setP(newGrid, { x, y }, [WALL])
                        continue;
                    }

                    // Any other 'current' can only be a blizzard
                    const direction = directions[current];
                    if (!direction) throw "Non-empty array, but not a blizzard? " + currents;

                    // Check where we are moving and if that tile is a wall.
                    let targetPos = add({ x, y }, direction);
                    const targetTile = getP(grid, targetPos, []);

                    // The input contains no vertical blizzards on start.x and end.x
                    // Any move is therefore a spot inside the wall or a wall.
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
                    newTile.push(current);
                    setP(newGrid, targetPos, newTile);

                }
            }
        }
        gridByTime[state.time] = newGrid;
        // show(newGrid);
        // console.log(gridByTime);
    }

    const states = [];
    for (let cardinalDir of Object.values(directions)) {
        // Spot is open
        const pos = add(state.E, cardinalDir);
        if (pos.x < minX || pos.x > maxX || pos.y < minY || pos.y > maxY) continue;
        const spot = getP(newGrid, pos, [])
        // console.log(spot, pos, maxX, maxY)
        // console.log(newGrid)
        if (spot.length == 0) {
            states.push({
                time: state.time + 1,
                E: pos,
                p: state.p.concat(pos)
            });
        }
    }

    // If we are surrounded by blizzards in the next minute
    // Then we must wait. 
    if (states.length === 0) {
        // We can only wait if our waiting spot is NOT occupied by a blizzard now.
        if (getP(newGrid, state.E, []).length === 0) {
            states.push({
                time: state.time + 1,
                E: { ...state.E },
                p: state.p.concat({ ...state.E })
            });
        }
        // else: swallowed by blizzard 🥶
    }

    return states;
}

const gridByTime = {};

function simulate(start, end) {
    const init = {
        time: 1,
        E: start,
        p: [start]
    }
    const k = state => `[${state.time % (maxX * maxY)}] (${state.E.x},${state.E.y})`;
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
            console.log({ state })
            return state;
        }
        const next = nextStates(state, start, end)
        // console.log(next);
        q.push(...next);
    }
    return 'no solutions found.'
}

function solveFor(lines, start, end) {
    const grid = parse(lines);
    // Sanity check to see if we got the end right.
    console.log('end', getP(grid, end))
    for (const dir of Object.values(directions)) {
        console.log('neighbours', dir, getP(grid, add(end, dir)));
    }
    gridByTime[0] = grid;

    allowSizeChange = false;

    const ans = simulate(start, end);
    // for (let [time, g] of Object.entries(gridByTime)) {
    //     console.log(time)
    //     show(g, ans.p[time]);
    // }
    return ans;
}


const solve = (start, end) => (lines) => {
    minX = minY = maxX = maxY = 0;
    allowSizeChange = true;
    return {
        p1: solveFor(lines, start, end),
        // p2: solveFor(lines),
    }
}


(async () => {
    let example = await readAndSolve('24.example.input', solve({ x: 1, y: 0 }, { x: 6, y: 5 }), '\n');
    test('Example p1', 18, example.answer.p1)
    test('Example p2', undefined, example.answer.p2)

    // 153: too low
    const puzzle = await readAndSolve('24.input', solve({ x: 1, y: 0 }, { x: 120, y: 26 }), '\n');
    printAnswer(puzzle);
})();
