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

Object.prototype.get = function(x, y) { return this[`${x}_${y}`]; };
Object.prototype.set = function(x, y, n) { this[`${x}_${y}`] = n; };
Object.prototype.clone = function() { return JSON.parse(JSON.stringify(this)); };

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
    grid.set(x, y, (grid.get(x, y) || 0) + n);
    if (grid.get(x, y) > 9) {
        flashes++;
        grid.set(x, y, -1);
    }
}

function initialIncrease(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            increase(grid, x, y, 1, true);
        }
    }
}

function resetToZero(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.get(x, y) < 0)
                grid.set(x, y, 0);
        }
    }
}

function getEnergyFromNeighbours(grid) {
    let increases = {};
    let hasIncreases = false;
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.get(x, y) <= 0)  // skip those who are flashing
                continue;

            let n = 0;
            for(let dx = -1; dx <= 1; dx++) {
                for(let dy = -1; dy <= 1; dy++) {
                    if (dx == 0 && dy == 0) {
                        continue; // don't tag ourselves
                    }
                    if (grid.get(x + dx, y + dy) === -1) {
                        n++;
                        hasIncreases = true;
                    }
                }
            }
            increases.set(x, y, n);
        }
    }
    resetToZero(grid);
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            let n = increases.get(x, y);
            if (n) {
                increase(grid, x, y, n);
            }
        }
    }
    return hasIncreases;
}

function step(grid) {
    // Initial lighting step
    initialIncrease(grid);
    let hasIncrease = true;
    while (hasIncrease) {
        hasIncrease = getEnergyFromNeighbours(grid);
    }
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
    for(let i = 1; i <= 100; i++) {
        console.log('step', i);
        step(grid);
        print(grid);
        if (allZero(grid)) {
            return i;
        }
    }

    return flashes;
}

p('11-example.txt', solve1);
// p('11-input.txt', solve1);
