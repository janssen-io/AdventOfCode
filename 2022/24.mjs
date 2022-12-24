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

function show(grid) {
    for (let y = minY; y <= maxY; y++) {
        let row = ''
        for (let x = minX; x <= maxX; x++) {
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
        console.log(row);
    }
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
                const currents = get(grid, x, y, []);
                for (const current of currents) {
                    if (current == WALL) {
                        setP(newGrid, {x, y }, [WALL])
                        continue;
                    }
                    const direction = directions[current];
                    if (!directions[current]) continue;
                    const pos = add({ x, y }, direction);
                    const spot = getP(grid, pos);
                    if (spot === undefined || spot[0] === WALL) {
                        let newPos;
                        if (direction.x < 0) newPos = { x: maxX - 1, y: pos.y };
                        if (direction.x > 0) newPos = { x: 1, y: pos.y };
                        if (direction.y < 0) {
                            if (pos.x == end.x) {
                                newPos = { x: pos.x, y: maxY }
                            }
                            else {
                                newPos = { x: pos.x, y: maxY - 1 };
                            }
                        }
                        if (direction.y > 0) {
                            if (pos.x == start.x) {
                                newPos = { x: pos.x, y: 0 }
                            }
                            else {
                                newPos = { x: pos.x, y: 1 };
                            }
                        }
                        const newSpot = getP(newGrid, newPos, []);
                        newSpot.push(current);
                        setP(newGrid, newPos, newSpot);
                    }
                    else {
                        const newSpot = getP(newGrid, pos, []);
                        newSpot.push(current);
                        setP(newGrid, pos, newSpot);
                    }
                }
            }
        }
        gridByTime[state.time] = newGrid;
    }

    const nextStates = [];
    for (let cardinalDir of Object.values(directions)) {
        // Spot is open
        const pos = add(state.E, cardinalDir);
        if (pos.x < minX || pos.x > maxX || pos.y < minY || pos.y > maxY) continue;
        const spot = getP(newGrid, pos, [])
        if (spot !== undefined && spot.length == 0) {
            nextStates.push({
                time: state.time + 1,
                E: pos
            });
        }
    }
    if (nextStates.length === 0) {
        if (getP(newGrid, state.E, []).length > 0) {
            return []; // swallowed by blizzard;
        }
        // wait in this spot
        nextStates.push({ time: state.time + 1, E: { ...state.E } });
    }

    return nextStates;
}

const gridByTime = {};

function simulate(start, end) {
    const init = {
        time: 0,
        E: start,
    }
    const cycle = gcd(maxX, maxY);
    console.log(cycle);
    const k = state => `[${state.time % (maxX * maxY)}] (${state.E.x},${state.E.y})`;
    const seen = new Set();

    let q = new Heap(s => s?.E, (a, b) => b === undefined ? false : manhattan(a, end) > manhattan(b, end))
    q.push(init);
    while (q.length) {
        const state = q.shift();
        console.log(state);
        if (state.time > 20) return;
        const key = k(state);

        if (seen.has(key)) continue;
        seen.add(key);

        if (state.E.x == end.x && state.E.y == end.y) {
            return state;
        }
        const next = nextStates(state, start, end)
        console.log(next);
        q.push(...next);
    }
    return 'no solutions found.'
}

function solveFor(lines, start, end) {
    const grid = parse(lines);
    gridByTime[-1] = grid;

    allowSizeChange = false;

    const ans = simulate(start, end);
    // for (let [time, g] of Object.entries(gridByTime)) {
    //     console.log(time, g)
    //     show(g);
    // }
    return ans;
}


const solve = (start, end) => (lines) => {
    minX = minY = maxX = maxY = 0;
    return {
        p1: solveFor(lines, start, end),
        // p2: solveFor(lines),
    }
}


(async () => {
    let example = await readAndSolve('24.example.input', solve({ x: 1, y: 0 }, { x: 6, y: 5 }), '\n');
    test('Example p1', 18, example.answer.p1)
    test('Example p2', undefined, example.answer.p2)

    // const puzzle = await readAndSolve('24.input', solve({ x: 1, y: 0 }, { x: 120, y: 26 }), '\n');
    // printAnswer(puzzle);
})();
