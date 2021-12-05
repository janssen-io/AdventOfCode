// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// helper functions
const parseLine = l => {
    var parts = l.split(' ');
    var fst = parts[0].split(',').map(x => +x);
    var snd = parts[2].split(',').map(x => +x);

    var l = { x1: fst[0], x2: snd[0], y1: fst[1], y2: snd[1] }
    if (l.x1 > l.x2 || l.y1 > l.y2) {
        return {
            x1: snd[0], y1: snd[1],
            x2: fst[0], y2: fst[1]
        }
    }
    return l;
}

const isOverlapping = (l1, l2) => {
    if (isV(l1) && isV(l2)) {
        return l1.x1 == l2.x1
            && l1.y1 >= l2.y1
            && l1.y1 <= l2.y2;
    }
    if (isH(l1) && isH(l2)) {
        return l1.y1 == l2.y1
            && l1.x1 >= l2.x1
            && l1.x1 <= l2.x2;
    }
    if (isH(l1)) {
        return l2.x1 >= l1.x1
            && l2.x1 <= l1.x2
            && l1.y1 >= l2.y1
            && l1.y1 <= l2.y2;
    }
    if (isV(l1)) {
        return l2.y1 >= l1.y1
            && l2.y1 <= l1.y2
            && l1.x1 >= l2.x1
            && l1.x1 <= l2.x2;
    }
    console.error("Invalid Combo")
    // return (l1[0] > l2[0] && l1[1] < l2[1]);
}

const isH = l => l.y1 === l.y2;
const isV = l => l.x1 === l.x2;

const intersection = (l1, l2) => {
    var points = [];

    if (isV(l1) && isV(l2) && l1.x1 == l2.x1) {
        var x = l1.x1;
        var lower = Math.max(l1.y1, l2.y1);
        var upper = Math.min(l1.y2, l2.y2);
        if (lower > upper) {
            console.log(swapped);
            [lower, upper] = [upper, lower];
        }
        for(let y = lower; y <= upper; y++) {
            points.push([x, y])
        }
    }
    else if(isH(l1) && isH(l2) && l1.y1 == l2.y1) {
        var y = l1.y1;
        var lower = Math.max(l1.x1, l2.x1);
        var upper = Math.min(l1.x2, l2.x2);
        if (lower > upper) {
            console.log(swapped);
            [lower, upper] = [upper, lower];
        }
        for(let x = lower; x <= upper; x++) {
            points.push([x, y])
        }
    }
    else if (isH(l1) && isV(l2)) {
        points.push([l2.x1, l1.y1])
    }
    else if (isV(l1) && isH(l2)) {
        points.push([l1.x1, l2.y1])
    }
    else {
        console.error("overlap but no intersection ??");
    }
    return points;
}

function onlyUnique(value, index, self) {
  return self.indexOf(value) === index;
}

// solvers
function solve1(lines) {
    var straights = lines.map(parseLine).filter(l => isV(l) || isH(l) );
    var points = [];
    for(let i = 0; i < straights.length; i++) {
        for (let j = i + 1; j < straights.length; j++) {
            var l1 = straights[i];
            var l2 =straights[j];
            if (isOverlapping(l1, l2) || isOverlapping(l2, l1)) {
                var overlaps = intersection(l1, l2)
                points = points.concat(overlaps);
            }
        }
    }
    var s = points.map(x => "" + x).filter(onlyUnique);
    return { l: s.length, s: s.sort() }
}

function solve2(lines) {
    return lines;
}


// run
p('05-example.txt', solve1);
p('05-input.txt', solve1);
