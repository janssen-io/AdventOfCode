import { readAndSolve, printAnswer, test, range } from '../aoc.mjs'


const target_row = 2_000_000;
const mult = 4_000_000;

function parse(line) {
    const data = line.numbers();
    return {
        sensor: { x: data[0], y: data[1] },
        beacon: { x: data[2], y: data[3] },
    }
}

function manhattan(origin, target) {
    const d = Math.abs(origin.x - target.x) + Math.abs(origin.y - target.y);
    return d;
}

const solveFor = (lines, y) => {
    const data = lines.map(parse);
    // console.log(data)
    const distances = data.map(d => manhattan(d.sensor, d.beacon))
    const maxDistance = Math.max(...distances);
    const grid = {};
    data.map(({ sensor, beacon }) => {
        grid.setCell(sensor.x, sensor.y, 'S');
        grid.setCell(beacon.x, beacon.y, 'B');
    })

    const min_x = Math.min(...data.flatMap(d => [d.beacon.x, d.sensor.x]));
    const max_x = Math.max(...data.flatMap(d => [d.beacon.x, d.sensor.x]));

    console.log({ min_x, max_x })
    const impossiblePositions = [];
    // for(let y = 0; y < 30; y++)
    for (let x = min_x - maxDistance; x <= max_x + maxDistance; x++) {
        // if (x==2) console.log('loop')
        const maybeBeacon = { x, y };
        // const canPlaceBeacon = data.map((d, i) => manhattan(d.sensor, maybeBeacon) > distances[i]);
        if (grid.getCell(x, y) == "S") {
            console.log({ x, y });
            impossiblePositions.push(maybeBeacon);
            continue;
        }

        for (let i = 0; i < data.length; i++) {
            if (manhattan(data[i].sensor, maybeBeacon) <= distances[i]) {
                if (grid.getCell(x, y) == "B") {
                    continue;
                }
                grid.setCell(x, y, '#');
                impossiblePositions.push(maybeBeacon);
                break;
            }
        }
    }
    // console.log(grid.showGrid('.'))
    // console.log(impossiblePositions)
    return impossiblePositions.length;
}

const solveFor2 = (lines, max) => {
    const data = lines.map(parse);
    const distances = data.map(d => manhattan(d.sensor, d.beacon))
    let g = { dimensions: {
        min_x:0, min_y:0, max_x: max, max_y: max
    }};
    g.setCell(14, 11, 'T');
    let ps = {}
    for (let y = 0; y <= max; y++) {
        const intervals = data.map((d, i) => {
            const delta = Math.abs(d.sensor.y - y);
            if (delta > distances[i]) return null; // scanner not in range of this row
            const interval = [d.sensor.x - (distances[i] - delta), d.sensor.x + (distances[i] - delta)];
            return interval;
        }).filter(x => x != null);
        intervals.push([-1, -1])
        intervals.push([max + 1, max + 1])
        const sorted = intervals.sort((a, b) => a[0] - b[0]);
        let high = 0;
        for(let [l, h] of sorted) {
            if (high > max) break;
            if (h < 0) continue;
            if (l > high + 1) {
                const hole = range(Math.max(0, high + 1), Math.min(max, l));
                ps[y] = (ps[y] || []).concat(hole);
                // for(let x of hole) {
                //     g.setCell(x, y, '?')
                // }
            }
            high = Math.max(h, high);
        }
    }
    // console.log(g.showGrid('.'))
    for(let y of Object.keys(ps)) {
        if (ps[y].length) {
            console.log({x: ps[y], y})
            return ps[y] * 4_000_000 + +y; // ps[y] should only contain a single number
        }
    }

}

const solve = (y, max) => {
    return (lines) => {
        return {
            p1: solveFor(lines, y),
            p2: solveFor2(lines, max),
        }
    }
}

(async () => {
    let example = await readAndSolve('15.example.input', solve(10, 20), '\n');
    test('Example p1', 26, example.answer.p1)
    test('Example p2', 56_000_011, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve('15.input', solve(2_000_000, 4_000_000), '\n');
    printAnswer(puzzle);
})();