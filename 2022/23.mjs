import { readAndSolve, printAnswer, test, assert } from '../aoc.mjs'

const add = (left, right) => { return { x: left.x + right.x, y: left.y + right.y } };
const E = { x: 1, y: 0 };
const S = { x: 0, y: 1 };
const W = { x: -1, y: 0 };
const N = { x: 0, y: -1 };
const NE = add(N, E);
const SE = add(S, E);
const NW = add(N, W);
const SW = add(S, W);

const proposedDirections = [
    [N, NE, NW],
    [S, SE, SW],
    [W, NW, SW],
    [E, NE, SE],
]

const allDirections = proposedDirections.flat().unique();

let maxX = 0;
let maxY = 0;
let minX = 0;
let minY = 0;

const setP = (grid, { x, y }, value) => set(grid, x, y, value);
function set(grid, x, y, value) {
    if (x > maxX) maxX = x;
    if (y > maxY) maxY = y;
    if (x < minX) minX = x;
    if (y < minY) minY = y;

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
            if (lines[y][x] == '#')
                set(grid, x, y, lines[y][x]);
        }
    }
    return grid;
}

function show(grid) {
    for (let y = minY; y <= maxY; y++) {
        let row = ''
        for (let x = minX; x <= maxX; x++) {
            row += get(grid, x, y, '.');
        }
        console.log(row);
    }
}

const hasNeighbour = (grid, point, directions) => {
    for (let dir of directions) {
        const p = add(point, dir);
        if (getP(grid, add(point, dir), '.') != '.') return true;
    }
    return false;
}

// Create a grid with coordinates from elves who want to move to that spot
// If there are more than one elf in that spot, then we can use the original
// coordinates to put them back on their original spot.
//
// Elves will never propose to move to a spot where there currently is an elf.
// So elves that do not move, can safely propose to 'move' into the spot 
// they are current standing on.
function propose(grid, startIndex) {
    const proposalGrid = {};
    // loop over all elves;
    for (let x = minX; x <= maxX; x++) {
        elfloop:
        for (let y = minY; y <= maxY; y++) {
            if (get(grid, x, y, '.') == '.') continue;
            const t = new Date();
            // If we don't have a neighbour at all, then don't move.
            // 'Propose' to stay. And continue with the next spot.
            if (!hasNeighbour(grid, { x, y }, allDirections)) {
                // propose to stay in place
                // if another elf plans to move here, it will see this neighbour on the original grid
                // and not even propose to move
                setP(proposalGrid, { x, y }, [{ x, y }]);
                continue;
            }


            // Otherwise, check all proposed directions starting from startIndex.
            for (let i = startIndex; i < startIndex + proposedDirections.length; i++) {
                const moveDirection = proposedDirections[i % proposedDirections.length];
                // If we don't have a neighbour in this quarter, then propose to move.
                if (hasNeighbour(grid, { x, y }, moveDirection)) continue;

                // we have a neighbour somewhere, but not in this quarter. Propose to move there.
                const target = add({ x, y }, moveDirection[0]);
                const currentValue = getP(proposalGrid, target, []);
                currentValue.push({ x, y });
                setP(proposalGrid, target, currentValue);
                continue elfloop;
            }

            // Finally, if we didn't do a proposal to move, then don't move.
            // 'Propose' to stay. And continue with the next spot.
            setP(proposalGrid, { x, y }, [{ x, y }]);
        }
    }
    return proposalGrid;
}

function walk(proposal) {
    const grid = {};
    for (let x = minX; x <= maxX; x++) {
        for (let y = minY; y <= maxY; y++) {
            const elves = get(proposal, x, y, []);
            if (elves.length == 1) {
                // Single elf wants to move to {x, y}
                // They can go there. :)
                set(grid, x, y, '#');
            }
            else {
                // Multiples elves want to move to {x, y}
                // Set them back to their original location.
                for (const elf of elves) {
                    setP(grid, elf, '#')
                }
            }
        }
    }
    return grid;
}

function gridEqual(a, b) {
    for (let x = minX; x <= maxX; x++) {
        for (let y = minY; y <= maxY; y++) {
            if (get(a, x, y, '.') !== get(b, x, y, '.')) return false;
        }
    }
    return true;
}

function emptySpots(grid, elfCount) {
    let elfMinX, elfMaxX, elfMinY, elfMaxY;
    elfMinX = elfMaxX = elfMinY = elfMaxY = 0;
    for (let x = minX; x <= maxX; x++) {
        for (let y = minY; y <= maxY; y++) {
            if (get(grid, x, y, '.') == '#') {
                if (x < elfMinX) elfMinX = x;
                if (x > elfMaxX) elfMaxX = x;
                if (y < elfMinY) elfMinY = y;
                if (y > elfMaxY) elfMaxY = y;
            }
        }
    }
    return (elfMaxX - elfMinX + 1) * (elfMaxY - elfMinY + 1) - elfCount;
}

function solveFor(lines, maxIterations) {
    let grid = parse(lines);
    const elfCount = Object.values(grid).length;

    let elvesStoppedMoving = false;
    let p1, p2;
    for (let i = 0; ; i++) {
        if (i % 200 == 100) console.log(i)
        // p1
        if (i == maxIterations) {
            p1 = emptySpots(grid, elfCount);
        }

        const proposal = propose(grid, i);
        const newGrid = walk(proposal);

        // p2
        if (gridEqual(newGrid, grid)) {
            grid = newGrid;
            p2 = i + 1;
            elvesStoppedMoving = true;
            break;
        }

        grid = newGrid;
    }

    const elves = Object.values(grid).length;

    assert(() => elves == elfCount, "Oh no, the elves procreated")
    return { p1: p1 || emptySpots(grid, elfCount), p2 };
}


const solve = (lines) => {
    // Ensure dimensions are reset between solves!
    minX = minY = maxX = maxY = 0;
    return solveFor(lines, 10)
}

(async () => {
    let example = await readAndSolve('23.example.input', solve, '\n');
    test('Example p1', 25, example.answer.p1)
    test('Example p2', 4, example.answer.p2)

    const puzzle = await readAndSolve('23.input', solve, '\n');
    printAnswer(puzzle);
})();
