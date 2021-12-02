fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

function solve1(lines) {
    var res = { h: 0, d: 0};
    for(let line of lines) {
        if (line.startsWith("up")) {
            res['d'] -= +(line[line.length - 1])
        }
        if (line.startsWith("down")) {
            res['d'] += +(line[line.length - 1])
        }
        if (line.startsWith("forward")) {
            res['h'] += +(line[line.length - 1])
        }
    }

    return res['d'] * res['h'];
}

function solve2(lines) {
    var res = { h: 0, d: 0, a: 0};
    for(let line of lines) {
        if (line.startsWith("up")) {
            res['a'] -= +(line[line.length - 1])
        }
        if (line.startsWith("down")) {
            res['a'] += +(line[line.length - 1])
        }
        if (line.startsWith("forward")) {
            res['h'] += +(line[line.length - 1])
            res['d'] += +(line[line.length - 1]) * res['a']
        }
    }

    return res['d'] * res['h'];
}

p('02-input.txt', solve2);
p('02-example.txt', solve2);


// p('02-input.txt', solve2);
// p('02-example.txt', solve2);