// file reading
const fs = require('fs');
require('./aoc');

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

function parseLine(line) {
    let nums = line.match(/-?(\d+)/g).map(x => +x);
    return {
        on: line[1] === 'n', // o[n] or o[f]f
        x1: nums[0], x2: nums[1],
        y1: nums[2], y2: nums[3],
        z1: nums[4], z2: nums[5]
     }
}

// part 1
function parseCuboid(line, grid) {
    let nums = line.match(/-?(\d+)/g).map(x => +x);
    if (nums[0] < BOUNDS.min) return grid;
    if (nums[0] > BOUNDS.max) return grid;

    let val = line[1] === 'n'; // o[n] or o[f]f
    console.log(line, val, nums);

    for(let x = nums[0]; x <= nums[1]; x++) {
        for(let y = nums[2]; y <= nums[3]; y++) {
            for(let z = nums[4]; z <= nums[5]; z++) {
                grid[`${x}_${y}_${z}`] = val;
            }
        }
    }
    return grid;
}

// part 2
function intersectLine(a1, a2, b1, b2) {
    let p1, p2;
    if (a1 >= b1 && a1 <= b2) { p1 = a1; }
    else if (b1 >= a1 && b1 <= a2) { p1 = b1; }

    if (a2 >= b1 && a2 <= b2) { p2 = a2; }
    else if (b2 >= a1 && b2 <= a2) { p2 = b2; }

    return [p1, p2];
}

function size(a) {
    // +1, because (1,1, 1,1, 1,1) has size 1
    return Math.abs(1 + a.x2 - a.x1) * Math.abs(1 + a.y2 - a.y1) * Math.abs(1 + a.z2 - a.z1);
}

function getOverlap(a, b) {
    let overlap = [
        intersectLine(a.x1, a.x2, b.x1, b.x2),
        intersectLine(a.y1, a.y2, b.y1, b.y2),
        intersectLine(a.z1, a.z2, b.z1, b.z2)
    ];

    if (overlap.some(o => isNaN(o[0]))) { return null; }

    return { on: b.on,
        x1: overlap[0][0], x2: overlap[0][1],
        y1: overlap[1][0], y2: overlap[1][1],
        z1: overlap[2][0], z2: overlap[2][1],
    };
}

function removeOverlap(head, tail) {
    const overlapWithHead = b => getOverlap(head, b);
    const id = x => x;
    let overlappingCuboids = tail.map(overlapWithHead).filter(id);

    // if the same part is overlapped twice, we should only remove it once!
    // therefore, we remove anything overlapping the overlap...
    return overlappingCuboids.reduce((sum, cuboid, i) => sum - removeOverlap(cuboid, overlappingCuboids.slice(i+1)), size(head));
}

function solve(lines) {
    let instructions = lines.map(parseLine);
    return instructions.reduce((sum, instruction, i) => instruction.on ? sum + removeOverlap(instruction, instructions.slice(i + 1)) : sum, 0)
}

let file = process.argv[2] || 'input';
p(`22-${file}.txt`, solve);