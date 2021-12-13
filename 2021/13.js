// file reading
const fs = require('fs')
require('./aoc.js')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// Helpers
function print(grid, readable = false) {
    console.log('Grid', {max_x: grid.max_x, max_y: grid.max_y})
    for(let y = 0; y <= grid.max_y; y++) {
        let row = '';
        for(let x = 0; x <= grid.max_x; x++) {
            let value = grid.getCell(x, y, readable ? ' ' : '.');
            if (value == '#')
                value = value.yellow();
            if (readable && x % 5 == 4)
                row += value.repeat(4);
            row += value;
        }
        console.log(row);
    }
}

function foldAlongX(grid, num) {
    const newGrid = {};
    newGrid.max_x = num - 1;
    newGrid.max_y = grid.max_y;
    for (let y = 0; y < grid.max_y + 1; y++) {
        let i = num;
        for(let x = 0; x < num; x++) {
            if(x == num - i) {
                let originalValue = grid.getCell(num - i, y);
                newGrid.setCell(num - i, y, grid.getCell(num + i, y, originalValue));
                i--;
            }
            else {
                newGrid.setCell(x, y, grid.getCell(x, y));
            }
        }
    }
    return newGrid;
}

function foldAlongY(grid, num) {
    const newGrid = {};
    newGrid.max_x = grid.max_x;
    newGrid.max_y = num - 1;
    for(let x = 0; x < grid.max_x + 1; x++) {
        let i = num;
        for (let y = 0; y < num; y++) {
            if(y == num - i) {
                let originalValue = grid.getCell(x, num - i);
                newGrid.setCell(x, num - i,
                    grid.getCell(x, num + i, originalValue));
                i--;
            }
            else {
                newGrid.setCell(x, y, grid.getCell(x, y));
            }
        }
    }
    return newGrid;
}

function count(grid) {
    return Object.values(grid).filter(v => v == '#').length;
}

function applyFolds(i, lines, grid) {
    let fold, axis, num;
    for(let j = i; j < lines.length; j++) {
        let line = lines[j];
        [_, _, fold] = line.split(' ');
        [axis, num] = fold.split('=');

        if (axis == 'y') {
            grid = foldAlongY(grid, +num);
        }
        else {
            grid = foldAlongX(grid, +num);
        }
    }

    return grid;
}

// solvers
function solve(lines) {
    let result = {};

    const grid = {};
    let x, y, line;
    let max_x = 0, max_y = 0;
    let i = 0;
    while(line = lines[i]) {
        [x, y] = line.split(',').map(x => +x);
        if (x > max_x)
            max_x = x;
        if (y > max_y)
            max_y = y;
        grid.setCell(x, y, '#');
        i++;
    }
    grid.max_x = max_x; grid.max_y = max_y;
    result = applyFolds(i + 1, lines, grid)
    print(result, true)
    return count(result)
}

// p('13-example.txt', solve);
p('13-input.txt', solve);