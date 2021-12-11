// file reading
fs = require('fs')
require('./aoc.js')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// Helpers
let flashes = 0;

function print(grid, highlight = true) {
    console.log('----------');
    for(let y = 0; y < grid.dim; y++) {
        let row = '';
        for(let x = 0; x < grid.dim; x++) {
            if (grid.get(x, y) == 0 && highlight) {
                row += "\x1b[33m0\x1b[0m" //highlight 0s to make it easier for us
            }
            else {
                row += grid.get(x, y);
            }
        }
        console.log(row);
    }
    console.log('----------');
}

function increase(grid, x, y, n) {
    grid.set(x, y, grid.get(x, y, 0) + n);
    if (grid.get(x, y) === 10) { //only flash if it just reached 10, any increase above are irrelevant.
        flashes++;
        tagNeighbours(grid, x, y);
    }
}

function initialIncrease(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            increase(grid, x, y, 1);
        }
    }
}

function resetToZero(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.get(x, y) > 9)
                grid.set(x, y, 0);
        }
    }
}

function tagNeighbours(grid, x, y) {
    for(let dx = -1; dx <= 1; dx++) {
        for(let dy = -1; dy <= 1; dy++) {
            if (dx == 0 && dy == 0) {
                continue; // don't tag ourselves
            }
            if (x + dx >= grid.dim || y + dy >= grid.dim
                || x + dx < 0 || y + dy < 0) {
                    continue; // out of bounds
                }
                increase(grid, x + dx, y + dy, 1);
        }
    }
}

function step(grid) {
    initialIncrease(grid);
    resetToZero(grid);
}

function allZero(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.get(x, y) !== 0) {
                return false;
            }
        }
    }
    return true;
}

// solvers
function solve1(lines) {
    const grid = {};
    for(let y = 0; y < lines.length; y++) {
        let octos = lines[y].split('').map(x => +x);
        for(let x = 0; x < octos.length; x++) {
            grid.set(x, y, octos[x]);
        }
    }
    grid.dim = lines.length;
    print(grid);
    for(let i = 1; i <= 50000; i++) {
        console.log('step', i);
        step(grid);
        print(grid);
        if (allZero(grid)) {
            return {i, flashes};
        }
    }

    return {flashes};
}

p('11-input.txt', solve1);
// p('11-input.txt', solve1);
