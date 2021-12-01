fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

function solve1(lines) {
    var depths = lines.map(x => +x);
    let count = 0;
    for(let i = 1; i < depths.length; i++) {
        if (depths[i] > depths[i-1]) {
            count++;
        }
    }
    return count;
}

function solve2(lines) {
    return lines
        .map(x => +x)
        .reduce( (windows, _, index, depths) => {
            windows[index] = depths.slice(index, index + 3);
            return windows;
        }, [])
        .map(win => win.reduce((sum, measurement) => sum + measurement))
        .filter((_, i, windows) => windows[i] > windows[i-1])
        .length;
}

p('01-input.txt', solve2);
p('01-example.txt', solve2);

