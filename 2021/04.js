const { getMaxListeners } = require('process');

fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

const getNumber = cell => Object.keys(cell)[0];
const getMark = cell => Object.values(cell)[0];

function mark(board, candidate) {
    for(let i = 0; i < board.length; i++) {
        for(let r = 0; r < board[i].length; r++) {
            if (Object.keys(board[i][r])[0] == candidate) {
                board[i][r][candidate] = true;
            }
        }
    }
}

function getLine(board) {
    for(let i = 0; i < board.length; i++) {
        var row = board[i];
        var ts = row.map(r => getMark(r)).filter(x => x);
        if (ts.length == board.length)
            return row;
    }

    return [];
}

function getUnmarked(board) {
    let us = [];
    for(let i = 0; i < board.length; i++) {
        for(let r = 0; r < board[i].length; r++) {
            if (getMark(board[i][r]) !== true) {
                us.push(getNumber(board[i][r]));
            }
        }
    }
    return us;
}

function solve1(lines) {
    var chosen = lines[0].split(',').map(x => +x);
    var rest = lines.splice(1).filter(x => x != '');

    const boards = [];
    for(let i = 0; i < rest.length; i++) {
        let board = Math.floor(i / 5);
        boards[board] = boards[board] || [];
        boards[board][i%5] = rest[i]
            .split(' ')
            .filter(x => x != '')
            .map(num => { return { [num]: false } });
    }

    let bl = boards.length;
    for(let board = 0; board < bl; board++) {
        let rows = boards[board];
        let columns = [];
        for(let rowi = 0; rowi < rows.length; rowi++) {
            columns[rowi] = rows.map(row => row[rowi]);
        }
        boards.push(columns);
    }

    for(let i = 0; i < chosen.length; i++) {
        let candidate = chosen[i];
        for (let bi = 0; bi < bl; bi++) {
            mark(boards[bi], candidate);
            mark(boards[bi + bl], candidate);

            let row = getLine(boards[bi])
            let column = getLine(boards[bi + bl])
            if (row.length > 0) {
               console.log(candidate, row.map(getNumber))
               var nums = getUnmarked(boards[bi])
               return nums.reduce( (p, n) => p + +n, 0) * candidate;
            }
            if (column.length > 0) {
               console.log(candidate, column.map(getNumber))
               var nums = getUnmarked(boards[bi + bl])
               return nums.reduce( (p, n) => p + +n, 0) * candidate;
            }
        }
    }
    return 'no sol';
}

function solve2(lines) {
    var chosen = lines[0].split(',').map(x => +x);
    var rest = lines.splice(1).filter(x => x != '');

    const boards = [];
    for(let i = 0; i < rest.length; i++) {
        let board = Math.floor(i / 5);
        boards[board] = boards[board] || [];
        boards[board][i%5] = rest[i]
            .split(' ')
            .filter(x => x != '')
            .map(num => { return { [num]: false } });
    }

    let bl = boards.length;
    for(let board = 0; board < bl; board++) {
        let rows = boards[board];
        let columns = [];
        for(let rowi = 0; rowi < rows.length; rowi++) {
            columns[rowi] = rows.map(row => row[rowi]);
        }
        boards.push(columns);
    }

    let solvedBoards = [];
    let sols = [];
    for(let i = 0; i < chosen.length; i++) {
        let candidate = chosen[i];
        for (let bi = 0; bi < bl; bi++) {
            mark(boards[bi], candidate);
            mark(boards[bi + bl], candidate);

            let row = getLine(boards[bi])
            let column = getLine(boards[bi + bl])
            if (row.length > 0 && !solvedBoards.includes(bi)) {
               var nums = getUnmarked(boards[bi])
               sols.push(nums.reduce( (p, n) => p + +n, 0) * candidate);
               solvedBoards.push(bi);
            }
            if (column.length > 0 && !solvedBoards.includes(bi)) {
               var nums = getUnmarked(boards[bi + bl])
               sols.push(nums.reduce( (p, n) => p + +n, 0) * candidate);
               solvedBoards.push(bi);
            }
        }
    }
    return sols[sols.length - 1];
}

p('04-input.txt', solve2);
p('04-example.txt', solve2);
