// file reading
const fs = require('fs');
require('./aoc.js')
const Data = require('./heap.js')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}


// Helpers
function createGrid(lines) {
    const grid = {};
    for(let y = 0; y < lines.length; y++) {
        for(let x = 0; x < lines[y].length; x++) {
            grid.setCell(x, y, +lines[y][x]);
        }
    }

    grid.max_y = lines.length - 1;
    grid.max_x = lines[0].length - 1;

    return grid;
}

function createRepeatedGrid(lines, n) {
    const grid = {};
    width = lines[0].length;
    height = lines.length;
    for(let xx = 0; xx < n; xx++) {
        for(let yy = 0; yy < n; yy++) {
            for(let y = 0; y < lines.length; y++) {
                for(let x = 0; x < lines[y].length; x++) {
                    let risk = (+lines[y][x] + xx + yy);
                    grid.setCell(
                        x + width * xx,
                        y + height * yy,
                        risk < 10 ? risk : (risk % 10 + 1))
                }
            }
        }
    }


    grid.max_y = lines.length * 5 - 1;
    grid.max_x = lines[0].length * 5 - 1;

    return grid;
}

function navigate(grid) {
    let q = [];
    let seen = {};
    let next;
    q.push({ x: 0, y: 0, distance: 0, crumbs: [1] })
    while(next = q.shift()) {
        if (next.x == grid.max_x && next.y == grid.max_y) {
            // console.log({crumbs: next.crumbs})
            return next.distance;
        }

        if (seen.getCell(next.x, next.y)) {
            continue;
        }

        seen.setCell(next.x, next.y, true);

        let neighbours = [
            { dx: -1, dy:  0 },
            { dx:  1, dy:  0 },
            { dx:  0, dy: -1 },
            { dx:  0, dy:  1 },
        ]

        for(let n of neighbours) {
            let x = next.x + n.dx;
            let y = next.y + n.dy;
            if(grid.getCell(x, y) === undefined || seen.getCell(x, y))
                continue;

            // Keep track of the crumbs is slower, because of copying
            // But very useful when checking the example!
            q.push({x, y, distance: next.distance + grid.getCell(x, y)}) //, crumbs: [...next.crumbs, grid.getCell(x, y)]})
        }

        // When using a regular queue, we need to sort it so the smallest distances are considered first
        // It would be more efficient to use a min-heap.
        q.sort((a,b) => a.distance - b.distance);
    }
}

// solvers
function solve(lines) {
    const grid = createRepeatedGrid(lines, 5);
    const risk = navigate(grid);
    return risk;
}


// Just as slow/memory consuming as solve/1

// p('15-input.txt', solve);
p('15-input.txt', solve);