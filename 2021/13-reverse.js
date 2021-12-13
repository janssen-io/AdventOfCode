// file reading
const fs = require('fs')
require('./aoc.js')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

const dimaros = [
'######   ###  #     #     #     ######   #######   #####  ',
'#     #   #   ##   ##    # #    #     #  #     #  #     # ',
'#     #   #   # # # #   #   #   #     #  #     #  #       ',
'#     #   #   #  #  #  #     #  ######   #     #   #####  ',
'#     #   #   #     #  #######  #   #    #     #        # ',
'#     #   #   #     #  #     #  #    #   #     #  #     # ',
'######   ###  #     #  #     #  #     #  #######   #####  ',
]

const bitly = [
'                                             #   #####    #####    #####                    #####   #######',
'#####   #  #####       #       #   #       #   #     #  #     #  #     #  ######    ##    #     #  #       ',
'#    #  #    #         #        # #       #          #  #        #        #        #  #         #  #       ',
'#####   #    #         #         #       #      #####   #  ####  #        #####   #    #   #####   ######  ',
'#    #  #    #    ###  #         #      #            #  #     #  #        #       ######        #        # ',
'#    #  #    #    ###  #         #     #       #     #  #     #  #     #  #       #    #  #     #  #     # ',
'#####   #    #    ###  ######    #    #         #####    #####    #####   ######  #    #   #####    #####  '
];

const xmas = [
'#     #  #######  ######   ######   #     #  #     #  #     #     #      #####  ',
'##   ##  #        #     #  #     #   #   #    #   #   ##   ##    # #    #     # ',
'# # # #  #        #     #  #     #    # #      # #    # # # #   #   #   #       ',
'#  #  #  #####    ######   ######      #        #     #  #  #  #     #   #####  ',
'#     #  #        #   #    #   #       #       # #    #     #  #######        # ',
'#     #  #        #    #   #    #      #      #   #   #     #  #     #  #     # ',
'#     #  #######  #     #  #     #     #     #     #  #     #  #     #   #####  ',
];

// Helpers
function print(grid, readable = false) {
    console.log('Grid', {max_x: grid.max_x, max_y: grid.max_y})
    for(let y = 0; y <= grid.max_y; y++) {
        let row = '';
        for(let x = 0; x <= grid.max_x; x++) {
            let value = grid.getCell(x, y, readable ? ' ' : '.');
            if (value == '#')
                value = '#'.yellow();
            if (readable && x % 5 == 4)
                row += value.repeat(4);
            row += value;
        }
        console.log(row);
    }
}

function unfoldAlongX(grid, iteration) {
    const foldSize = Math.ceil(Math.random() * grid.max_x) + 1;
    const newGrid = {};
    newGrid.max_x = grid.max_x + foldSize;
    newGrid.max_y = grid.max_y;

    for(let y = 0; y < grid.max_y + 1; y++) {
        let i = foldSize - 1;
        for(let x = 0; x < grid.max_x + 1; x++) {
            let currentValue = grid.getCell(x, y);
            if (i > 0 && x == grid.max_x - i + 1 && currentValue) {
                let shouldFold = Math.random() >= 0.05;
                let shouldKeep = !shouldFold || Math.random() < iteration / 20;
                if (shouldFold) {
                    newGrid.setCell(grid.max_x + i + 1, y, currentValue);
                }
                if (shouldKeep) {
                    newGrid.setCell(x, y, currentValue);
                }
                i--;
            }
            else {
                newGrid.setCell(x, y, currentValue);
            }
        }
    }
    return { grid: newGrid, fold: grid.max_x + 1}
}

function unfoldAlongY(grid, iteration) {
    const foldSize = Math.ceil(Math.random() * grid.max_y) + 1;
    const newGrid = {};
    newGrid.max_x = grid.max_x;
    newGrid.max_y = grid.max_y + foldSize;

    for(let x = 0; x < grid.max_x + 1; x++) {
        let i = foldSize - 1;
        for(let y = 0; y < grid.max_y + 1; y++) {
            let currentValue = grid.getCell(x, y);
            if (i > 0 && y == grid.max_y - i + 1 && currentValue) {
                let shouldFold = Math.random() >= 0.05;
                let shouldKeep = !shouldFold || Math.random() < iteration / 20;
                if (shouldFold) {
                    newGrid.setCell(x, grid.max_y + i + 1, currentValue);
                }
                if (shouldKeep) {
                    newGrid.setCell(x, y, currentValue);
                }
                i--;
            }
            else {
                newGrid.setCell(x, y, currentValue);
            }
        }
    }
    console.log()
    return { grid: newGrid, fold: grid.max_y + 1}
}

function gridToCoords(grid) {
    let coords = []
    for(let x = 0; x <= grid.max_x; x++){
        for(let y = 0; y <= grid.max_y; y++){
            if (grid.getCell(x, y) == '#') {
                coords.push({x, y});
            }
        }
    }
    return coords;
}

function linesToGrid(lines) {
    const grid = {};
    grid.max_x = lines[0].length - 1;
    grid.max_y = lines.length - 1;
    for(let y = 0; y < lines.length; y++) {
        let line = lines[y];
        for(let x = 0; x < line.length; x++){
            if(line[x] == '#') {
                grid.setCell(x, y, '#');
            }
        }
    }
    return grid;
}

function foldAlongX(grid, num) {
    const newGrid = {};
    newGrid.max_x = num - 1;
    newGrid.max_y = grid.max_y;
    for (let y = 0; y < grid.max_y + 1; y++) {
        let i = num;
        for(let x = 0; x < num; x++) {
            if(x == num - i) {
                let originalValue = grid.getCell(x, y);
                newGrid.setCell(x, y, grid.getCell(num + i, y, originalValue));
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
    newGrid.max_y = num;
    for(let x = 0; x < grid.max_x + 1; x++) {
        let i = num;
        for (let y = 0; y < num; y++) {
            let originalValue = grid.getCell(x, y);
            if(y == num - i) {
                newGrid.setCell(x, num - i,
                    grid.getCell(x, num + i, originalValue));
                i--;
            }
            else {
                newGrid.setCell(x, y, originalValue);
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
function createPuzzle() {
    let grid = linesToGrid(xmas);
    let numberOfFolds = 12;
    let folds = [];
    let fold;
    for(let i = 0; i < numberOfFolds; i++) {
        let foldOverX = Math.random() > 0.45;
        ({ grid, fold } = foldOverX ? unfoldAlongX(grid, i) : unfoldAlongY(grid, i));
        folds.unshift(`fold along ${foldOverX ? 'x=' : 'y='}${fold}`);
    }
    print(grid)

    let coords = gridToCoords(grid).map(c => `${c.x},${c.y}`).shuffle().shuffle();

    let input = coords.concat(['']).concat(folds)
    console.log(input.join("\r\n"))
    solve(input)
    // console.log(input);
}

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
    print(result, false)
    return count(result)
}

// p('13-example.txt', solve);
// p('13-input.txt', solve);
createPuzzle();
