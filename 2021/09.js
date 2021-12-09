const { create } = require('domain');

// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

Array.prototype.sum = function() {
    return this.reduce((p,n) => +p + +n);
}

Array.prototype.product = function() {
    return this.reduce((p,n) => +p * +n);
}

// Some comparison helpers, so we don't need to duplicate the checks for undefined.
const isSmaller = (a, b) => {
    return typeof b == 'undefined' || a < b;
}

// Here we also need to check that the number isn't 9.
// As per the description, those don't count as part of a basin.
// (else everything would be one large basin)
const isHigher = (a, b) => {
    return (typeof a != 'undefined')
        && a != 9
        && (typeof b == 'undefined' || a > b);
}

// solvers
function solve1(lines) {
    let lows = [];
    let grid = lines.map(line => line.split('').map(x => +x));
    for (let y = 0; y < grid.length; y++) {
        for(let x = 0; x < grid[y].length; x++) {
            let n = (grid[y - 1] || [])[x];
            let s = (grid[y + 1] || [])[x];
            let e = grid[y][x + 1];
            let w = grid[y][x - 1];

            if ([n,e,s,w].every(d => isSmaller(grid[y][x], d))) {
                lows.push(grid[y][x] + 1);
            }
        }
    }
    return  {lows, s: lows.sum() };
}

function getBasin(grid, low, basin) {
    const x = low.x;
    const y = low.y;
    let pos = (grid[low.y] || [])[low.x];
    basin.push(grid[low.y][low.x]);

    // Set visited positions to 9, so they are no longer considered.
    grid[low.y][low.x] = 9;

    let n = (grid[y - 1] || [])[x];
    let s = (grid[y + 1] || [])[x];
    let e = grid[y][x + 1];
    let w = grid[y][x - 1];

    // getBasin has 'side-effects'
    // It mutates the basin and grid passed as arguments.
    if (isHigher(n, pos)) {
        getBasin(grid, {x, y: y - 1}, basin)
    };

    if (isHigher(s, pos)) {
        getBasin(grid, {x, y: y + 1}, basin)
    };

    if (isHigher(e, pos)) {
        getBasin(grid, {x: x + 1, y}, basin)
    };

    if (isHigher(w, pos)) {
        getBasin(grid, {x: x - 1, y}, basin)
    };

    return basin;
}

function solve2(lines) {
    let lows = [];
    let basins = [];
    let grid = lines.map(line => line.split('').map(x => +x));
    for (let y = 0; y < grid.length; y++) {
        for(let x = 0; x < grid[y].length; x++) {
            let n = (grid[y - 1] || [])[x];
            let s = (grid[y + 1] || [])[x];
            let e = grid[y][x + 1];
            let w = grid[y][x - 1];

            if ([n,e,s,w].every(d => isSmaller(grid[y][x], d))) {
                lows.push({x, y});
            }
        }
    }

    basins = lows.map(low => getBasin(grid, low, []));

    basins = basins
        // For some reason a 9 appeared in one of my basins,
        // Filtering it out here is faster than bug hunting and it worked. ¯\_(ツ)_/¯
        .map(b => b.filter(x => x != 9))
        .sort((a, b) => a.length > b.length ? -1 : 1);

    biggest = basins.map(b => b.length).splice(0,3);

    return  { biggest, s: biggest.product() };
}

// p('09-example.txt', solve2);
p('09-input.txt', solve2);
// p('09-input.txt', solve1);
