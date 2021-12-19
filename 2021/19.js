// file reading
const fs = require('fs');
const { first } = require('lodash');
require('./aoc');

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

const xyz = (x,y,z) => { return { x, y, z} };
const eql = (p1, p2) => p1.x === p2.x && p1.y === p2.y && p1.z === p2.z;
const add = (p1, p2) => xyz(p1.x + p2.x, p1.y + p2.y, p1.z + p2.z);

Array.prototype.intersectWith = function intersectWith(b, equalFn) {
    const result = [];
    for(let item of this) {
        for(let b_item of b) {
            if (equalFn(item, b_item)) result.push(item);
        }
    }
    return result;
}

Array.prototype.indexOfWith = function intersectWith(item, equalFn) {
    for(let i = 0; i < this.length; i++) {
        if (equalFn(item, this[i])) return i;
    }
    return -1;
}

function parseScanner(lines, cursor) {
    let id = +lines[cursor].replace(/\D+/g, '');
    cursor++;
    let beacons = [];
    while(lines[cursor]) {
        let parts = lines[cursor].trim().split(',').map(x => +x);
        beacons.push(xyz(parts[0], parts[1], parts[2] || 0));
        cursor++;
    }

    return { scanner: { id, beacons: beacons.sort(createSort3d(true)), pos: undefined }, cursor };
}

function createSort3d(ascending = false) {
    return function sort3d(p1, p2) {
        let mod = ascending ? -1 : 1;
        if (p1.x < p2.x) return  1 * mod;
        if (p1.x > p2.x) return -1 * mod;
        if (p1.y < p2.y) return  1 * mod;
        if (p1.y > p2.y) return -1 * mod;
        if (p1.z < p2.z) return  1 * mod;
        if (p1.z > p2.z) return -1 * mod;
        return 0;
    }
}

const delta = (p1, p2) => xyz(p1.x - p2.x, p1.y - p2.y, p1.z - p2.z);

function createDeltas(beacons) {
    const deltas = [];
    // console.log('-- ', beacons);
    for(let i = 0; i < beacons.length; i++) {
        for(let j = i + 1; j < beacons.length; j++) {
            deltas.push(delta(beacons[i], beacons[j]));
        }
    }
    return deltas;
}

function createOrientations(beacons) {
    let newBeacons = [];
    for (let beacon of beacons) {
        let coords = [beacon.x, beacon.y, beacon.z];
        for(let face = 0; face < 3; face++) {
            for(let signMask = 0; signMask < 8; signMask++) {
                let swapped = (signMask % 2 == 0 ? [1,2] : [2,1]).map(x => (x + face) % 3)
                let shiftedCoords = [ coords[face], coords[swapped[0]], coords[swapped[1]] ];
                let result = []
                for (let i = 0; i < 3; i++) {
                    result[i] = (signMask & (1 << i)) == 0 ? shiftedCoords[i] * -1 : shiftedCoords[i]
                }
                newBeacons[face * 8 + signMask] = newBeacons[face * 8 + signMask] || [];
                newBeacons[face * 8 + signMask].push(xyz(result[0], result[1], result[2]));
            }
        }
    }
    return newBeacons;
}

// function createOrientations(beacons) {
//     let newBeacons = [];
//     for (let beacon of beacons) {
//         let coords = [beacon.x, beacon.y, beacon.z];
//         for(let face = 0; face < 3; face++) {
//             let shiftedCoords = [0,1,2].map(i => coords[(i + face) % 3]);
//             for(let signMask = 0; signMask < 8; signMask++) {
//                 let result = []
//                 for (let i = 0; i < 3; i++) {
//                     result[i] = (signMask & (1 << i)) == 0 ? shiftedCoords[i] * -1 : shiftedCoords[i]
//                 }
//                 newBeacons[face * 8 + signMask] = newBeacons[face * 8 + signMask] || [];
//                 newBeacons[face * 8 + signMask].push(xyz(result[0], result[1], result[2]));
//             }
//         }
//     }
//     return newBeacons;
// }


let grid = {};
let grid2d = {};
let numBeacons = 0;

function inc(x, y) {
    let v = grid.getCell(x, y) || 0;
    grid2d.setCell(x, y, v + 1);
}

function findMatchingScanner(s0, s1) {
    let d0 = createDeltas(s0.beacons)
    for(let s1_beacons of createOrientations(s1.beacons)) {
        let d1 = createDeltas(s1_beacons);
        let overlaps = d0.intersectWith(d1, eql);
        if (overlaps.length >= 12) {
            const s0_b = findOverlappingBeacon(s0.beacons, overlaps[0]);
            const s1_b = findOverlappingBeacon(s1_beacons, overlaps[0]);

            let offset = delta(s0_b, s1_b);
            s1.pos = add(s0.pos, offset);
            s1.beacons = s1_beacons; // proper orientation
            drawBeacons(s1);
            grid.setCell3d(s1.pos.x, s1.pos.y, s1.pos.z, 'S');
            grid2d.setCell(s1.pos.x, s1.pos.y, 'S');
            return overlaps;
        }
    }
    return false;
}

function findOverlappingBeacon(beacons, overlappingDelta) {
    let foundDelta = false;
    for (i = 0; i < beacons.length && !foundDelta; i++) {
        for (j = 1; j < beacons.length && !foundDelta; j++) {
            if (eql(delta(beacons[i], beacons[j]), overlappingDelta)) {
                foundDelta = true;
                s1_b = beacons[i];
            }
        }
    }
    return s1_b;
}

function drawBeacons(s1) {
    for(let beacon of s1.beacons) {
        let p = add(s1.pos, beacon);
        if (!grid.getCell3d(p.x, p.y, p.z)) {
            numBeacons++;
            grid.setCell3d(p.x, p.y, p.z, 'B');
        }
        inc(p.x, p.y);
    }
}

function print2d() {
    for(let y = grid2d.dimensions.min_y; y <= grid2d.dimensions.max_y; y++) {
        let row = '';
        for(let x = grid2d.dimensions.min_x; x <= grid2d.dimensions.max_x; x++) {
            row += grid2d.getCell(x, y, '.');
        }
        console.log(row);
    }
}

function countGrid() {
    let counter = 0;
    console.log('dim', grid.dimensions)
    for(let z = grid.dimensions.min_z; z <= grid.dimensions.max_z; z++) {
        for(let y = grid.dimensions.min_y; y <= grid.dimensions.max_y; y++) {
            for(let x = grid.dimensions.min_x; x <= grid.dimensions.max_x; x++) {
                // console.log({x, y, z})
                if (grid.getCell3d(x, y, z)) {
                    counter++;
                }
            }
        }
    }
    return counter;
}

function solve(lines) {
    let cursor = 0;
    let scanners = [];
    while(cursor < lines.length) {
        let scanner = parseScanner(lines, cursor);
        cursor = scanner.cursor + 1;
        scanners.push(scanner.scanner);
    }
    scanners[0].pos = xyz(0,0,0);
    grid2d.setCell(0, 0, 'S');

    let allOverlaps = []
    let scannersWithPos = [scanners[0]]
    drawBeacons(scanners[0]);
    let s0;
    while(s0 = scannersWithPos.pop()) {
        for(let j = 0; j < scanners.length; j++) {
            let s1 = scanners[j];
            if (s1.id == s0.id) continue;
            if (s1.pos) continue;

            findMatchingScanner(s0, s1);
            if (s1.pos) {
                console.log(s1.id, s1.pos);
                scannersWithPos.push(s1);
            }
        }
    }
    // print2d();
    console.log(numBeacons);
}

let file = process.argv[2] || 'input';
p(`19-${file}.txt`, solve);