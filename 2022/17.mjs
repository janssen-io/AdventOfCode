import { readAndSolve, printAnswer, test, range, assert } from '../aoc.mjs'

const rockTypes = [
    ['####'],
    ['.#.', '###', '.#.'],
    // Reverse, because we're flipping the rows upside down.
    ['..#', '..#', '###'].reverse(),
    ['#..', '#..', '#..', '#..'],
    ['##.', '##.']
]

// Last x-index where a rock still can be placed
const LEFT = 0;
const RIGHT = 6;

function getRockCoordinates(rock, xLeft, yTop) {
    let coordinates = [];
    for (let i = 0; i < rock.length; i++) {
        const row = rock[i];
        const currentWidth = row.filter(x => x == '#').length;
        const indexOfFirst = row.indexOf('#');
        const currentLeft = xLeft + indexOfFirst;
        coordinates = coordinates.concat(range(currentLeft, currentLeft + currentWidth).map(x => { return { x, y: yTop + i } }));
    }
    return coordinates;
}

function testGetRockCoordinates() {
    assert(
        () => JSON.stringify(getRockCoordinates(rockTypes[0], 2, 0)) == `[{"x":2,"y":0},{"x":3,"y":0},{"x":4,"y":0},{"x":5,"y":0}]`,
        JSON.stringify(getRockCoordinates(rockTypes[0], 2, 0))
    )
    assert(
        () => JSON.stringify(getRockCoordinates(rockTypes[1], 2, 0)) == `[{"x":3,"y":0},{"x":2,"y":1},{"x":3,"y":1},{"x":4,"y":1},{"x":3,"y":2}]`,
        JSON.stringify(getRockCoordinates(rockTypes[1], 2, 0))
    )
    assert(
        () => JSON.stringify(getRockCoordinates(rockTypes[2], 2, 0)) == `[{"x":2,"y":0},{"x":3,"y":0},{"x":4,"y":0},{"x":4,"y":1},{"x":4,"y":2}]`,
        JSON.stringify(getRockCoordinates(rockTypes[2], 2, 0))
    )
    assert(
        () => JSON.stringify(getRockCoordinates(rockTypes[3], 2, 0)) == `[{"x":2,"y":0},{"x":2,"y":1},{"x":2,"y":2},{"x":2,"y":3}]`,
        JSON.stringify(getRockCoordinates(rockTypes[3], 2, 0))
    )
    assert(
        () => JSON.stringify(getRockCoordinates(rockTypes[4], 2, 0)) == `[{"x":2,"y":0},{"x":3,"y":0},{"x":2,"y":1},{"x":3,"y":1}]`,
        JSON.stringify(getRockCoordinates(rockTypes[4], 2, 0))
    )
}

function add(p, q) { return { x: p.x + q.x, y: p.y + q.y }; }

function canMoveInto(room, coordinates) {
    for (let point of coordinates) {
        if (point.x < LEFT || point.x > RIGHT) return false;
        if (room.getCell(point.x, point.y)) return false;
    }
    return true;
}

function drawRock(room, position, symbol = '#') {
    for (const point of position) {
        room.setCell(point.x, point.y, symbol);
    }
}

const solveFor = (lines, numRocks) => {
    testGetRockCoordinates();
    const directions = lines[0].split('').map(c => c == '>' ? { x: 1, y: 0 } : { x: -1, y: 0 });
    const room = initializeRoom();
    let maxY = 0;
    let nextDir = 0;

    const memo = {}

    for (let nextRock = 0; nextRock < numRocks; nextRock++) {
        const type = nextRock % rockTypes.length;

        // check if we've seen this rock type moving in these directions before.
        if (memo[`${type}_${nextDir}`]) {
            const { nextRock: previousRock, maxY: previousMax } = memo[`${type}_${nextDir}`];
            previousMax.push(maxY);
            previousRock.push(nextRock);
            if (previousMax.length > 3) {
                const remainingRocks = numRocks - nextRock;
                const cycleLength = nextRock - previousRock.last(1);

                // check if all the cycles on this (rock,direction) combo
                // actually have the same height difference.
                const isRepeating = previousMax
                    // calculate delta height between cycles
                    // i instead of i - 1, because sliced index starts at 0 for max_1
                    .slice(1)
                    .map((max, i) => max - previousMax[i]) 
                    // get all unique numbers, if there's only 1, then delta height is the same
                    // for every cycle.
                    .unique()
                    .length == 1;

                // if we can fit the remainder by repeating the cycle
                if (remainingRocks % cycleLength == 0 && isRepeating) {
                    const numberOfCyclesRemaining = remainingRocks / cycleLength;
                    console.log('cycle:', {  cycleLength, previousRock, previousMax })
                    return maxY + (maxY - previousMax.last(1)) * numberOfCyclesRemaining;
                }
            }
        }
        else {
            memo[`${type}_${nextDir}`] = { nextRock: [nextRock], maxY: [maxY] }
        }

        ({ maxY, nextDir } = dropNextRock(nextRock, maxY, room, directions, nextDir));
    }

    return maxY;
}

const solve = (lines) => {
    return {
        p1: solveFor(lines, 2022),
        p2: solveFor(lines, 1_000_000_000_000),
    }
}

(async () => {
    let example = await readAndSolve('17.example.input', solve, '\n');
    test('Example p1', 3068, example.answer.p1)
    test('Example p2', 1514285714288, example.answer.p2)

    const puzzle = await readAndSolve('17.input', solve, '\n');
    printAnswer(puzzle);
})();

function initializeRoom() {
    const room = {
        dimensions: {
            min_x: 0, min_y: 0,
            max_x: 6, max_y: 0 // just some initial dimensions + extra width for drawing
        }
    };
    for (let x = LEFT; x <= RIGHT; x++) {
        room.setCell(x, 0, '-');
    }
    return room;
}

function dropNextRock(nextRock, maxY, room, directions, nextDir) {
    const type = rockTypes[nextRock % rockTypes.length];
    const spawnAt = { x: 2, y: maxY + 4 };
    let position = getRockCoordinates(type, spawnAt.x, spawnAt.y);
    assert(
        () => canMoveInto(room, position),
        `Cannot spawn rock ${nextRock} (${nextRock % rockTypes.length}) at (${spawnAt.x}, ${spawnAt.y}).`
    );

    while (true) {
        const dir = directions[nextDir];
        nextDir = (nextDir + 1) % directions.length;
        // const tmpRoom = { ...room }
        // drawRock(tmpRoom, position, '@');
        // console.log(tmpRoom.showGrid('.', true))

        const pushedPosition = position.map(p => add(p, dir));
        if (canMoveInto(room, pushedPosition)) {
            position = pushedPosition;
        }
        else {
        }

        const fallenPosition = position.map(p => add(p, { x: 0, y: -1 }));
        if (canMoveInto(room, fallenPosition)) {
            position = fallenPosition;
        }
        else {
            // break if we hit rock or bottom
            break;
        }
    }
    drawRock(room, position);
    for (let { x, y } of position) {
        if (y > maxY) maxY = y;
    }
    return { maxY, nextDir };
}

function* primes(max) {
    yield 1;
    yield 2;
    const primes = [2];
    for (let candidate = 3; candidate < max; candidate++) {
        const isDivisible = primes.some(prime => candidate % prime == 0);
        if (!isDivisible) {
            primes.push(candidate);
            yield candidate
        }
    }
}