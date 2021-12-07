// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

function fuel(ps, pos) {
    let result = 0;
    for(let p of ps) {
        result += gauss(Math.abs(p - pos));
    }
    return result;
}

function gauss(x) {
    return (x * (x + 1)) / 2;
}

// solvers
function solve1(lines) {
    const ps = lines[0].split(',').map(x => +x);
    let min = Math.min.apply(null, ps);
    let max = Math.max.apply(null, ps);

    let smallest = Number.MAX_VALUE;
    let smallesti = 0;
    for (let i = min; i <= max; i++) {
        let fuelreq = fuel(ps, i);
        if (fuelreq < smallest) {
            smallest = fuelreq;
            smallesti = i;
        }
    }

    return {smallesti, smallest};
}

p('07-input.txt', solve1);
// p('07-input.txt', solve1);
