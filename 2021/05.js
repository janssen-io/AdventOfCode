// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// helper functions
const parseLine = l => {
    var parts = l.split(' ');
    var fst = parts[0].split(',').map(x => +x);
    var snd = parts[2].split(',').map(x => +x);

    var l = { x1: fst[0], x2: snd[0], y1: fst[1], y2: snd[1] }
    if (l.x1 > l.x2 || l.y1 > l.y2) {
        return {
            x1: snd[0], y1: snd[1],
            x2: fst[0], y2: fst[1]
        }
    }
    return l;
}

const isH = l => l.y1 === l.y2;
const isV = l => l.x1 === l.x2;

const drawH = (grid, line) => {
    for(let x = line.x1; x <= line.x2; x++) {
        grid[x] = grid[x] || [];
        grid[x][line.y1] = (grid[x][line.y1] || 0) + 1;
    }
    return grid;
}

const drawV = (grid, line) => {
    console.log(line);
    for(let y = line.y1; y <= line.y2; y++) {
        grid[line.x1] = grid[line.x1] || [];
        grid[line.x1][y] = (grid[line.x1][y] || 0) + 1;
    }
    return grid;
}

const drawD = (grid, line) => {
    var yd = line.y1 > line.y2 ? -1 : 1;
    var xd = line.x1 > line.x2 ? -1 : 1;

    var cmp = _y => yd > 0
        ? (_y <= line.y2)
        : (_y >= line.y2);

    var x = line.x1;
    for(let y = line.y1; cmp(y); y += yd) {
        grid[x] = grid[x] || [];
        grid[x][y] = (grid[x][y] || 0) + 1;
        x += xd;
    }
    return grid;
}

const print = row => {
    let s = "";
    for(let i = 0; i < row.length; i++) {
        if (row[i] > 0 ) {
            s += row[i];
        }
        else {
            s += ".";
        }
    }
    console.log(s);
}

// solvers
function solve1(lines) {
    var straights = lines.map(parseLine); //.filter(l => isV(l) || isH(l) );
    var points = [];
    var grid = [];
    for(let i = 0; i < straights.length; i++) {
        let line = straights[i];
        if (isH(line))
            grid = drawH(grid, straights[i]);
        else if (isV(line))
            grid = drawV(grid, straights[i]);
        else
            grid = drawD(grid, straights[i]);
    }

    for (let x in grid) {
        for (let y in grid[x]) {
            if (grid[x][y] >= 2) {
                points.push(`${x},${y}`);
            }
        }
    }
    return { answer: points.length, points }
}

// p('05-example.txt', solve1);
p('05-input.txt', solve1);
