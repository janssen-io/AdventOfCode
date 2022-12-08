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

    for(let cx = x + dx; dx != 0 && cx >= 0 && cx <= grid.dimensions.max_x; cx += dx) {
        let neighbour = grid.getCell(cx, y);
        if (neighbour >= tree) return false;
    }

    for(let cy = y + dy; dy != 0 && cy >= 0 && cy <= grid.dimensions.max_y; cy += dy) {
        let neighbour = grid.getCell(x, cy);
        if (neighbour >= tree) return false;
    }

    return true;
}

function getDistance(grid, x, y, [dx, dy]) {
    const tree = grid.getCell(x, y);
    let neighbour;

    for(let cx = x + dx; dx != 0 && cx >= 0 && cx <= grid.dimensions.max_x; cx += dx) {
        neighbour = grid.getCell(cx, y);
        if (neighbour >= tree) return (cx - x) * dx;
    }

    for(let cy = y + dy; dy != 0 && cy >= 0 && cy <= grid.dimensions.max_y; cy += dy) {
        neighbour = grid.getCell(x, cy);
        if (neighbour >= tree) return (cy - y) * dy;
    }

    if(dx == -1) return x
    if(dy == -1) return y
    if(dx == 1) return grid.dimensions.max_x - x
    if(dy == 1) return grid.dimensions.max_y - y
}

function solve(lines) {
    const grid = readGrid(lines);

    // p1
    const visibleTrees = grid.map2D(
        (_, x, y) => cardinalDirections
            .map(dir => isVisibleFromDir(grid, x, y, dir))
            .some(x => x))

    var previewGrid = visibleTrees
        .map2D((isVisible, x, y) => ('' + grid.getCell(x, y)).colour(isVisible ? 32 : 31))
        .showGrid('.', 0);
    console.log(previewGrid);

    const p1 = Object
        .values(visibleTrees)
        .filter(x => x === true)
        .length;

    // p2
    const viewingDistances = grid.map2D(
        (_, x, y) => cardinalDirections
            .map(dir => getDistance(grid, x, y, dir))
            .product());

    const p2 = Math.max(...Object
        .values(viewingDistances)
        .filter(x => !isNaN(x)));
        
    // var l = ('' + p2).length
    // var previewGrid = viewingDistances
    //     .map2D(distance => ('' + distance).padStart(l, '0'))
    //     .showGrid();
    // console.log(previewGrid);

    return { p1, p2 };
}

readAndSolve(process.argv[2] || '08.input', solve, '\n');