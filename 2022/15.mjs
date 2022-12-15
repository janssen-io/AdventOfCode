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
    for(let y = 0; y < 30; y++)
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
    console.log(grid.showGrid('.'))
    // console.log(impossiblePositions)
    return impossiblePositions.length;
}

const workSize = 1;
function doWork(data, max) {
    const distances = data.map(d => manhattan(d.sensor, d.beacon))
    const maxDistance = Math.max(...distances);
    const grid = {};
    data.map(({ sensor, beacon }) => {
        grid.setCell(sensor.x, sensor.y, '#');
        grid.setCell(beacon.x, beacon.y, '#');
    })
    let g = { dimensions: {
        min_x:0, min_y:0, max_x: max, max_y: max
    }};
    g.setCell(14, 11, 'T');
    console.log({max})
    let px = {}; //new Set();
    let py = new Set();
    let t = 0
    let ps = {}
    for (let y = 0; y <= max; y++) {
        t = new Date();
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
        // console.log(sorted.length);
        for(let [l, h] of sorted) {
            if (high > max) break;
            if (h < 0) continue;
            if (l > high + 1) {
                const hole = range(Math.max(0, high + 1), Math.min(max, l));
                ps[y] = (ps[y] || []).concat(hole);
                //
                // px[y] = (px[y] || []).concat(range(Math.max(0, high + 1), Math.min(l, max)));
            }
            high = Math.max(h, high);
        }
        // console.log({y, t: new Date() - t})
    }
    for(let y of Object.keys(ps)) {
        if (ps[y].length) {
            console.log({x: ps[y], y})
            return ps[y] * 4_000_000 + +y;
        }
    }
    // console.log("finished x")
    // console.log(g.showGrid('.'))
    // console.log(g)
    let possible_xy = [];
    // for (let x = 0; x <= max; x++) {
    //     const intervals = data.map((d, i) => {
    //         const delta = Math.abs(d.beacon.x - x);
    //         if (delta > distances[i]) return null; // scanner not in range of this row
    //         const interval = [d.beacon.y - (distances[i] - delta), d.beacon.y + (distances[i] - delta)];
    //         return interval;
    //     }).filter(x => x != null);
    //     intervals.push([-1, -1])
    //     intervals.push([max + 1, max + 1])
    //     const sorted = intervals.sort((a, b) => a[0] - b[0]);
    //     let high = 0;
    //     for(let [l, h] of sorted) {
    //         if (high > max) break;
    //         if (h < 0) continue;
    //         if (l > high) {
    //             // px.add(x);
    //             // if(x == 14) console.log({x, lh: [l, h], high})
    //             // // possible is whole between high and current low
    //             // for(let y of range(Math.max(0, high + 1), Math.min(l,max))) {
    //                 // if (x == 14) console.log({x, y, has: py.has(y)})
    //                 // if (py.has(y)) {
    //                 //     possible_xy.push({x, y});
    //                 // }
    //             // }
    //             // for(let y of p.ys) {
    //             //     g.setCell(x, y, 'y');
    //             // }
    //             // console.log(p)
    //             // possible_y.push(p);
    //         }
    //         high = Math.max(h, high);
    //     }
    // }
    // console.log('finished y', px.size, py.size, px.size * py.size);
    // for(let x of px) {
    //     let t = new Date();
    //     for(let y of py) {
    //         const maybeBeacon = {x, y};
    //         if (data.every((d, i) => manhattan(d.sensor, maybeBeacon) > distances[i])) {
    //             yield maybeBeacon.x * 4_000_000  + maybeBeacon.y;
    //         }
    //     }
    //     console.log({x, t: new Date() - t});
    // }
    // yield 'end';
    // return possible_y;
    // console.log(g.showGrid('.'))
    // // return possible_y;
    // console.log("finished scan")
    // for(let {x, y} of possible_xy) {
    //     if (x < 0 || x > max || y < 0 || y> max) continue;
    //     const maybeBeacon = {x, y};
    //     if (data.every((d, i) => manhattan(d.sensor, maybeBeacon) > distances[i])) {
    //         yield maybeBeacon.x * 4_000_000  + maybeBeacon.y;
    //     }
    // }
    // for(let y of Object.keys(px)) {
    //     for (let x of px[y]) {
    //         if (x < 0 || x > max || y < 0 || y> max) continue;
    //         const maybeBeacon = {x, y};
    //         if (data.every((d, i) => manhattan(d.sensor, maybeBeacon) > distances[i])) {
    //             yield maybeBeacon.x * 4_000_000  + maybeBeacon.y;
    //         }
    //     }
    // }
    // yield 'end'
}

const solveFor2 = (lines, max) => {
    const data = lines.map(parse);
    // console.log(data)
    // for(let d of data) {
    //     const distance = manhattan(d.sensor, d.beacon)
    //     for(let x = d.sensor.x - distance; x <= d.sensor.x + distance; x++) {
    //         for(let y = d.sensor.y - distance; y <= d.sensor.y + distance; y++) {
    //             if (grid.getCell(x, y) == '#') continue;
    //             if (manhattan(d.sensor, {x, y}) <= distance) {
    //                 grid.setCell(x, y, '#');
    //             }
    //         }
    //     }
    // }
    // for(let x = 0; x <= max; x++) {
    //     console.log({x})
    //     for(let y = 0; y <= max; y++) {
    //         if (grid.getCell(x, y) == '#') continue;

    //         const maybeBeacon = {x, y};
    //         if (data.every((d, i) => manhattan(d.sensor, maybeBeacon) > distances[i])) {
    //             return {x, y, r: x * mult + y}
    //         }
    //         else {
    //             grid.setCell(x, y, '#');
    //         }
    //     }
    // }
    // const x_ranges = [];
    // const y_ranges = [];
    // for(let i = 0; i < data.length; i++) {
    //     x_ranges.push([data.beacon.x - distances[i], data.beacon.x + distances[i], data.beacon.y]);
    //     y_ranges.push([data.beacon.y - distances[i], data.beacon.y + distances[i], data.beacon.x]);
    // }

    const r = doWork(data, max);
    return r;
    return;


    return 'sad'
    for (let x = (max / 4) * n; x <= (max / 4) * (n + 1); x++) {
        console.log({ x })
        for (let y = 0; y <= max; y++) {
            // for(let i = 0; i < data.length; i++) {
            //     if (manhattan(data[i].sensor, maybeBeacon) <= distances[i]) {
            //         if (grid.getCell(x, y) == "B") {
            //             continue;
            //         }
            //         impossiblePositions.push(maybeBeacon);
            //         break;
            //     }
            // }
            const maybeBeacon = { x, y };
            // let isImpossible = false;
            // for(let i = 0; i < data.length && !isImpossible; i++) {
            //     if (manhattan(data[i].sensor, maybeBeacon) <= distances[i]) {
            //         if (grid.getCell(x, y) == "B") {
            //             continue;
            //         }
            //         isImpossible = true;
            //     }
            // }
            // if (!isImpossible) {
            //     return {x, y, r: x * mult + y}
            // }
            if (data.every((d, i) => manhattan(d.sensor, maybeBeacon) > distances[i])) {
                if (grid.getCell(x, y) == "B") {
                    continue;
                }
                return { x, y, r: x * mult + y }
            }
        }
    }
    return 'sad'
}

const solve = (y, max) => {
    return (lines) => {
        return {
            // p1: solveFor(lines, y),
            p2: solveFor2(lines, max, +process.argv[2]),
        }
    }
}

(async () => {
    let example = await readAndSolve('15.example.input', solve(10, 20), '\n');
    // test('Example p1', 26, example.answer.p1)
    test('Example p2', 56_000_011, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve('15.input', solve(2_000_000, 4_000_000), '\n');
    test('Puzzle p1 [RED]', 4724678, puzzle.answer.p1)
    printAnswer(puzzle);
})();