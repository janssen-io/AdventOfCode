import { readAndSolve, range } from '../aoc.mjs'

function readGrid(lines) {
    const max_x = Math.max(...lines.map(l => l.length));
    const grid = {};
    for(let y of range(0, lines.length)) {
        for(let x of range(0, max_x)) {
            grid.setCell(x, y, lines[y][x]);
        }
    }
    return grid;
}

const cardinalDirections = [
    [0, 1],
    [1, 0],
    [0, -1],
    [-1, 0]
]

function isVisibleFromDir(grid, x, y, [dx, dy]) {
    const tree = grid.getCell(x, y);
    let neighbour;
    for(let cx = x + dx; dx != 0 && cx >= 0 && cx <= grid.dimensions.max_x; cx += dx) {
        neighbour = grid.getCell(cx, y);
        if (neighbour >= tree) return false;
    }
    for(let cy = y + dy; dy != 0 && cy >= 0 && cy <= grid.dimensions.max_y; cy += dy) {
        neighbour = grid.getCell(x, cy);
        if (neighbour >= tree) return false;
    }
    return true;
}

function getDistance(grid, x, y, [dx, dy]) {
    const tree = grid.getCell(x, y);
    let neighbour;
    for(let cx = x + dx; dx != 0 && cx >= 0 && cx <= grid.dimensions.max_x; cx += dx) {
        neighbour = grid.getCell(cx, y);
        if (neighbour >= tree) return cx > x ? cx - x : x - cx; // cx + x * dx;
    }
    for(let cy = y + dy; dy != 0 && cy >= 0 && cy <= grid.dimensions.max_y; cy += dy) {
        neighbour = grid.getCell(x, cy);
        if (neighbour >= tree) return cy > y ? cy - y : y - cy; //cy + y * dy;
    }
    if(dx == 1) return grid.dimensions.max_x - x
    if(dx == -1) return x
    if(dy == 1) return grid.dimensions.max_y - y
    if(dy == -1) return y
    throw new Error("", dx, dy);
}

function solve(lines) {
    const grid = readGrid(lines);
    const preview = {};
    let sum = 0;

    // part 1
    for(let x of range(0, grid.dimensions.max_x + 1)) {
        for(let y of range(0, grid.dimensions.max_y + 1)) {
            preview.setCell(x, y, (grid.getCell(x,y) + ''))
            let isVisible = cardinalDirections.map(dir => isVisibleFromDir(grid, x, y, dir)).some(x => x);
            if (isVisible) {
                preview.setCell(x, y, (grid.getCell(x,y) + '').green())
                sum++;
            }
        }
    }

    // part 2
    let views = []
    for(let x of range(0, grid.dimensions.max_x + 1)) {
        for(let y of range(0, grid.dimensions.max_y + 1)) {
            let view = cardinalDirections.map(dir => getDistance(grid, x, y, dir)).product();
            // preview.setCell(x, y, view);
            views.push(view);
        }
    }

    // debug
    for(let y of range(0, grid.dimensions.max_y + 1)) {
        let row = ''
        for(let x of range(0, grid.dimensions.max_x + 1)) {
            // row += (preview.getCell(x, y) + ',').padStart(3, '0');
            row += preview.getCell(x, y);
        }
        console.log(row);
    }
    return {p1: sum, p2: Math.max(...views)};
}

readAndSolve(process.argv[2] || '08.input', solve, '\n');