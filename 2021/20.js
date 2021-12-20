// file reading
const fs = require('fs');
require('./aoc');

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

let grid = {};
function parseGrid(lines) {
    for(let y = 0; y < lines.length; y++) {
        for(let x = 0; x < lines[y].length; x++) {
            grid.setCell(x, y, lines[y][x]);
        }
    }
}

const PADDING = 1;
function enhanceGrid(enhancements, step) {
    let newGrid = {}
    for(let y = grid.dimensions.min_y - PADDING; y <= grid.dimensions.max_y + PADDING; y++) {
        for(let x = grid.dimensions.min_x - PADDING; x <= grid.dimensions.max_x + PADDING; x++) {
            let bin = '';
            for(let dy = -1; dy <= 1; dy++) {
                for(let dx = -1; dx <= 1; dx++) {
                    if ((x + dx >= grid.dimensions.min_x && x + dx <= grid.dimensions.max_x
                            && y + dy >= grid.dimensions.min_y && y + dy <= grid.dimensions.max_y)
                        || enhancements[0] === DARK)
                    {
                        bin += grid.getCell(x + dx, y + dy) === LIGHT ? 1 : 0;
                    }
                    else {
                        // algo starts with # and ends with . on index 512 (2^9)
                        // Therefore all infinite pixels keep blinking on/off.
                        // On on steps 0, 2... (being index   0 in the algo)
                        // On on steps 1, 3... (being index 512 in the algo)
                        bin += step % 2;
                    }
                }
            }
            let index = parseInt(bin, 2);
            newGrid.setCell(x, y, enhancements[index]);
        }
    }
    return newGrid;
}

function print(grid) {
    for(let y = grid.dimensions.min_y - PADDING; y <= grid.dimensions.max_y + PADDING; y++) {
        let row = ''
        for(let x = grid.dimensions.min_x - PADDING; x <= grid.dimensions.max_x + PADDING; x++) {
            row += grid.getCell(x, y, DARK);
        }
        console.log(row);
    }
}


const n = +(process.argv[3] || 50);
function solve(lines) {
    let enhancements = lines[0];
    parseGrid(lines.slice(2));

    for(let i = 0; i < n; i++) {
        // if (i % 50 == 0) {
        //     console.log(i);
        // }
        // if (i == 2) {
        //     console.log('Lights after 2 cycles:'.green(), Object.values(grid).filter(x => x === LIGHT).length);
        // }
        // console.clear();
        // print(grid);
        grid = enhanceGrid(enhancements, i);
    }

    console.log(`Lights after ${n} cycles:`.green(), Object.values(grid).filter(x => x === LIGHT).length);
}

const LIGHT = '#';
const DARK  = '.';
let file = process.argv[2] || 'input';
p(`20-${file}.txt`, solve);
