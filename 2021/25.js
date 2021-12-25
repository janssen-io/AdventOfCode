// file reading
const fs = require('fs');
require('./aoc');

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, 'answer: '.green(), solver(lines));
    });
}

function parseGrid(lines) {
    let grid = [];
    grid.dimensions = {
        x : lines[0].length,
        y : lines.length,
    }
    for (let y = 0; y < lines.length; y++) {
        for (let x = 0; x < lines[y].length; x++) {
            grid.setCell(x, y, lines[y][x]);
        }
    }
    return grid;
}

const EAST = '>';
const SOUTH = 'v';
const FREE = '.';

function step(grid) {
    let newGrid = [...grid];
    newGrid.dimensions = grid.dimensions;
    for(let y = 0; y < newGrid.dimensions.y; y++) {
        for(let x = 0; x < newGrid.dimensions.x; x++) {
            if (grid.getCell(x, y) == EAST && grid.getCellWrapped(x + 1, y) == FREE) {
                newGrid.setCell(x, y, FREE);
                newGrid.setCellWrapped(x + 1, y, EAST);
            }
        }
    }

    let newNewGrid = [...newGrid];
    newNewGrid.dimensions = newGrid.dimensions;
    for(let y = 0; y < newGrid.dimensions.y; y++) {
        for(let x = 0; x < newGrid.dimensions.x; x++) {
            if (newGrid.getCell(x, y) == SOUTH && newGrid.getCellWrapped(x, y + 1) == FREE) {
                newNewGrid.setCell(x, y, FREE);
                newNewGrid.setCellWrapped(x, y + 1, SOUTH);
            }
        }
    }
    return newNewGrid;
}

function print(grid) {
    for(let y = 0; y < grid.dimensions.y; y++) {
        let row = '';
        for(let x = 0; x < grid.dimensions.x; x++) {
            let cell = grid.getCell(x, y);
            row += cell;
        }
        console.log(row);
    }
}

function solve(lines) {
    let grid = parseGrid(lines);
    let i = 1;
    let newGrid = step(grid);
    while(grid.join('') != newGrid.join('')) {
        i++;
        grid = [...newGrid];
        newGrid = step(newGrid);
    }
    return i;
}

let file = process.argv[2] || 'input';
p(`25-${file}.txt`, solve);