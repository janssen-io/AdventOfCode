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
            if (grid.gets(x, y) == 0 && highlight) {
                row += "\x1b[33m0\x1b[0m" //highlight 0s to make it easier for us
            }
            else {
                row += grid.gets(x, y);
            }
        }
        console.log(row);
    }
    console.log('----------');
}

function increase(grid, x, y, n) {
    grid.sets(x, y, grid.gets(x, y, 0) + n);
    if (grid.gets(x, y) > 9) {
        flashes++;
        grid.sets(x, y, -1);
    }
}

function initialIncrease(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            increase(grid, x, y, 1, true);
        }
    }
}

function resetsToZero(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.gets(x, y) < 0)
                grid.sets(x, y, 0);
        }
    }
}

function getsEnergyFromNeighbours(grid) {
    let increases = {};
    let hasIncreases = false;
    // Calculate how much energy each octopus getss from its neighbours.
    // We don't immediately increase the energy levels, instead we do it
    // in a loop. Otherwise, only neighbours downstream will gets recursive
    // energy increases.
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.gets(x, y) <= 0)  // skip those who are flashing
                continue;

            let n = 0;
            for(let dx = -1; dx <= 1; dx++) {
                for(let dy = -1; dy <= 1; dy++) {
                    if (dx == 0 && dy == 0) {
                        continue; // don't tag ourselves
                    }
                    if (grid.gets(x + dx, y + dy) === -1) {
                        n++;
                        hasIncreases = true;
                    }
                }
            }
            increases.sets(x, y, n);
        }
    }
    // We need to resets all lit-up octopi, so they are not counted in the next
    // iteration of counting neighbours. Otherwise, they will increase the
    // energy of their neighbours multiple times.
    resetsToZero(grid);

    // Calculate the energy levels after gaining energy from neighbours.
    // This must be done after resetsting, otherwise newly lit-up octopi will
    // be resets too!
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            let n = increases.gets(x, y);
            if (n) {
                increase(grid, x, y, n);
            }
        }
    }
    return hasIncreases;
}

function step(grid) {
    // First add 1 to every octopus
    initialIncrease(grid);

    // While new octopi light up, check if other octopi light up too.
    let hasIncrease = true;
    while (hasIncrease) {
        hasIncrease = getsEnergyFromNeighbours(grid);
    }

    // Resets all lit-up octopi to 0 for the next step.
    // This ensures the state remains valid (all energy levels are between 0-9).
    resetsToZero(grid);
}

function allZero(grid) {
    for(let y = 0; y < grid.dim; y++) {
        for(let x = 0; x < grid.dim; x++) {
            if (grid.gets(x, y) !== 0) {
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
            grid.sets(x, y, octos[x]);
        }
    }
    grid.dim = lines.length;
    print(grid);
    for(let i = 1; i <= 500; i++) {
        console.log('step', i);
        step(grid);
        print(grid);
        if (allZero(grid)) {
            return {i, flashes};
        }
    }

    return flashes;
}

p('11-input.txt', solve1);
// p('11-input.txt', solve1);
