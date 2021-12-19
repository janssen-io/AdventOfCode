// file reading
const fs = require('fs');
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
            // else console.log('neq', item, b_item);
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

    return { scanner: { id, beacons: beacons.sort(createSort3d(false)), pos: undefined }, cursor };
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
        deltas[i] = []
        for(let j = 0; j < beacons.length; j++) {
            if (i == j) continue;
            deltas[i][j] = delta(beacons[i], beacons[j]);
        }
    }
    return deltas;
}

// This is no-where near correct, but worked perfectly for the small example and most scanners in the bigger example.
// Threw me off for a few hours. :D
function createOrientations(beacons) {
    let newBeacons = [];
    for (let beacon of beacons) {
        let coords = [beacon.x, beacon.y, beacon.z];
        for(let face = 0; face < 3; face++) {
            let shiftedCoords = [0,1,2].map(i => coords[(i + face) % 3]);
            for(let signMask = 0; signMask < 8; signMask++) {
                let result = []
                for (let i = 0; i < 3; i++) {
                    result[i] = (signMask & (1 << i)) == 0 ? shiftedCoords[i] * -1 : shiftedCoords[i]
                    if (result[i] === 0) { result[i] = 0} // get rid of -0 to be sure.
                }
                newBeacons[face * 8 + signMask] = newBeacons[face * 8 + signMask] || [];
                newBeacons[face * 8 + signMask].push(xyz(result[0], result[1], result[2]));
            }
        }
    }
    return newBeacons;
}

// Closer, but messes up, ones you start facing other directions, I think.
function createOrientations2(beacons) {
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
                    if (result[i] === 0) { result[i] = 0} // get rid of -0 to be sure.
                }
                newBeacons[face * 8 + signMask] = newBeacons[face * 8 + signMask] || [];
                newBeacons[face * 8 + signMask].push(xyz(result[0], result[1], result[2]));
            }
        }
    }
    return newBeacons;
}

// Created by using a single die.
// image 1 = x, 4 = y and 5 = z. Rotate it a few times around the axes and you get this:
// Fool proof, but takes a while to construct...
function createOrientations3(beacons) {
    let newBeacons = [];
    for (let beacon of beacons) {
        let orientations = [
            xyz(...[ beacon.x,  beacon.y,  beacon.z]),
            xyz(...[ beacon.x, -beacon.z,  beacon.y]),
            xyz(...[ beacon.x, -beacon.y, -beacon.z]),
            xyz(...[ beacon.x,  beacon.z, -beacon.y]),

            xyz(...[-beacon.x,  beacon.y, -beacon.z]),
            xyz(...[-beacon.x,  beacon.z,  beacon.y]),
            xyz(...[-beacon.x, -beacon.y,  beacon.z]),
            xyz(...[-beacon.x, -beacon.z, -beacon.y]),

            xyz(...[ beacon.y, -beacon.x,  beacon.z]),
            xyz(...[ beacon.y, -beacon.z, -beacon.x]),
            xyz(...[ beacon.y,  beacon.x, -beacon.z]),
            xyz(...[ beacon.y,  beacon.z,  beacon.x]),

            xyz(...[-beacon.y,  beacon.x,  beacon.z]),
            xyz(...[-beacon.y, -beacon.z,  beacon.x]),
            xyz(...[-beacon.y, -beacon.x, -beacon.z]),
            xyz(...[-beacon.y,  beacon.z, -beacon.x]),

            xyz(...[ beacon.z,  beacon.x,  beacon.y]),
            xyz(...[ beacon.z, -beacon.y,  beacon.x]),
            xyz(...[ beacon.z, -beacon.x, -beacon.y]),
            xyz(...[ beacon.z,  beacon.y, -beacon.x]),

            xyz(...[-beacon.z,  beacon.y,  beacon.x]),
            xyz(...[-beacon.z, -beacon.x,  beacon.y]),
            xyz(...[-beacon.z, -beacon.y, -beacon.x]),
            xyz(...[-beacon.z,  beacon.x, -beacon.y]),
        ];

            for(let i = 0; i < 24; i++) {
                newBeacons[i] = (newBeacons[i] || []);
                newBeacons[i].push(orientations[i]);
            }
    }

    return newBeacons;
}

function mult(matrix, vector) {
    return matrix.map(row => row[0] * vector[0] + row[1] * vector[1] + row[2] * vector[2])
}

// courtesy of https://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
let rotMatrices = [
    [[ 0, 0, 1], [ 0, 1, 0], [-1, 0, 0]],
    [[ 0, 0, 1], [ 0,-1, 0], [ 1, 0, 0]],
    [[ 0, 0, 1], [ 1, 0, 0], [ 0, 1, 0]],
    [[ 0, 0, 1], [-1, 0, 0], [ 0,-1, 0]],
    [[ 0, 0,-1], [ 0, 1, 0], [ 1, 0, 0]],
    [[ 0, 0,-1], [ 0,-1, 0], [-1, 0, 0]],
    [[ 0, 0,-1], [ 1, 0, 0], [ 0,-1, 0]],
    [[ 0, 0,-1], [-1, 0, 0], [ 0, 1, 0]],
    [[ 0, 1, 0], [ 0, 0, 1], [ 1, 0, 0]],
    [[ 0, 1, 0], [ 0, 0,-1], [-1, 0, 0]],
    [[ 0, 1, 0], [ 1, 0, 0], [ 0, 0,-1]],
    [[ 0, 1, 0], [-1, 0, 0], [ 0, 0, 1]],
    [[ 0,-1, 0], [ 0, 0, 1], [-1, 0, 0]],
    [[ 0,-1, 0], [ 0, 0,-1], [ 1, 0, 0]],
    [[ 0,-1, 0], [ 1, 0, 0], [ 0, 0, 1]],
    [[ 0,-1, 0], [-1, 0, 0], [ 0, 0,-1]],
    [[ 1, 0, 0], [ 0, 0, 1], [ 0,-1, 0]],
    [[ 1, 0, 0], [ 0, 0,-1], [ 0, 1, 0]],
    [[ 1, 0, 0], [ 0, 1, 0], [ 0, 0, 1]],
    [[ 1, 0, 0], [ 0,-1, 0], [ 0, 0,-1]],
    [[-1, 0, 0], [ 0, 0, 1], [ 0, 1, 0]],
    [[-1, 0, 0], [ 0, 0,-1], [ 0,-1, 0]],
    [[-1, 0, 0], [ 0, 1, 0], [ 0, 0,-1]],
    [[-1, 0, 0], [ 0,-1, 0], [ 0, 0,-1]],
]

// Borrowed some matrices from the internet, also pretty fool proof but tedious.
// Thanks internet!
function createOrientations4(beacons) {
    let newBeacons = [];
    for(beacon of beacons) {
        let orientations = rotMatrices.map(matrix => xyz(...mult(matrix, [beacon.x, beacon.y, beacon.z])))
        for(let i = 0; i < 24; i++) {
            newBeacons[i] = (newBeacons[i] || []);
            newBeacons[i].push(orientations[i]);
        }
    }
    return newBeacons;
}

let grid = {};

function findMatchingScanner(s0, s1) {
    let d0 = createDeltas(s0.beacons)
    for(let s1_beacons of createOrientations3(s1.beacons)) {
        let d1 = createDeltas(s1_beacons);
        let deltas = [] // contains the index of the beacons that overlap

        for(let i = 0; i < s0.beacons.length; i++) {
            for(let j = 0; j < s1_beacons.length; j++) {
                // we need to filter out the empty ones, because of
                let overlap = d0[i].filter(x => x).intersectWith(d1[j].filter(x => x), eql);
                if (overlap.length >= N_OVERLAP - 1) { // they both see at least n - 1 other beacons at the same distance
                    deltas.push([i,j]); // meaning beacon s0[i] is at the same place as s1[j]
                }
            }
        }
        if (deltas.length >= N_OVERLAP) {
            s1.beacons = s1_beacons; // proper orientation
            const s0_b = s0.beacons[deltas[0][0]];
            const s1_b = s1.beacons[deltas[0][1]];

            let offset = delta(s0_b, s1_b);
            s1.pos = add(s0.pos, offset);
            drawBeacons(s1);
            // Don't draw the scanners, that throws off the results. >.>
            // grid.setCell3d(s1.pos.x, s1.pos.y, s1.pos.z, 'S');
            // grid2d.setCell(s1.pos.x, s1.pos.y, 'S');
            return true;
        }
    }
    return false;
}

function drawBeacons(s1) {
    for(let beacon of s1.beacons) {
        let p = add(s1.pos, beacon);
        grid.setCell3d(p.x, p.y, p.z, 'B');
    }
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

    let scannersWithPos = [scanners[0]]
    drawBeacons(scanners[0]);
    let s0;
    while(s0 = scannersWithPos.shift()) {
        for(let j = 0; j < scanners.length; j++) {
            let s1 = scanners[j];
            if (s1.id == s0.id) continue;
            if (s1.pos) continue;

            findMatchingScanner(s0, s1);
            if (s1.pos) {
                console.log('Paired!', s0.id, s1.id, s1.pos);
                scannersWithPos.push(s1);
            }
        }
    }

    ps = scanners.map(s => s.pos);
    let d = (p1, p2) => (Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) + Math.abs(p1.z - p2.z))
    let max = 0;
    for(let i = 0; i < ps.length; i++) {
        for(let j = i; j < ps.length; j++) {
            let dp = d(ps[i], ps[j]);
            if (dp > max) {
                max = dp;
            }
        }
    }
    console.log('max distance: '.green(), max);

    console.log(Object.keys(grid));
    console.log('Number of beacons: '.green(), Object.keys(grid).length - 1); // -1 for dimensions
    console.log('grid size',
        grid.dimensions.max_x - grid.dimensions.min_x
        +grid.dimensions.max_y - grid.dimensions.min_y
        +grid.dimensions.max_z - grid.dimensions.min_z);
}

const N_OVERLAP = 12;
let file = process.argv[2] || 'input';
p(`19-${file}.txt`, solve);