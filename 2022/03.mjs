import {readAndSolve} from '../aoc.mjs'

// binary: parseInt(number, 2)
/* grids:
var grid = [];
grid.dimensions = {
    x: 100,
    y: 100
}
grid.setCell(x, y, value) or setCellWrapped

var grid = {}
grid.setCell3d(x, y, z, n);
grid.getCell3d(x, y, z, defaultValue);
*/

const isLower = (c) => c.toLowerCase() === c;
const prio = (c) => c.charCodeAt() + (isLower(c) ? - 'a'.charCodeAt() + 1 : - 'A'.charCodeAt() + 27)
function solve(lines) {
    let packs = [];
    let group = [];
    let sum = 0;
    for(let i = 0; i < lines.length; i++) {
        if (i % 3 == 0 && i > 0) {
            // work
            let common = packs[0].intersect(packs[1]).intersect(packs[2]).unique();
            // console.log({common, packs})
            sum += common.map(prio).sum();
            packs = [lines[i]];
        }
        else {
            packs.push(lines[i]);
        }
    }
    let common = packs[0].intersect(packs[1]).intersect(packs[2]).unique();
    // console.log({common, packs})
    sum += common.map(prio).sum();
    return sum;
    for(let line of lines) {
        let left = Array.from(line.substring(0, line.length / 2));
        let right = Array.from(line.substring(line.length / 2));
        let common = left.intersect(right);
        // console.log({ items: common.unique(), prio: common.map(prio)})
        packs.push(common.unique().map(c => prio(c)));
    }

    return packs.flat().sum()
};

readAndSolve(process.argv[2] || '03.input', solve);
// readAndSolve(process.argv[2] || '03 copy.input', solve);