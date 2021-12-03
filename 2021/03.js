fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

function solve2(lines) {
   var { most, least } = solve1(lines);

   function p(pos, quality, ls) {
    var bits = solve1(ls)[quality];
    var f = ls.filter(l => l[pos] == bits[pos]);
    return f.length == 1 ? f[0] : p(pos + 1, quality, f);
   }

   var oxygen = p(0, 'most', lines)
   var carbon = p(0, 'least', lines)
   o = parseInt(oxygen, 2);
   c = parseInt(carbon, 2);
   return { most, least, oxygen, carbon, r:  o * c  }
}

function solve1(lines) {
    const bs = {}
    var l = lines[0];
    for(let i = 0; i < l.length; i++) {
        bs[i] = [0, 0]
    }
    for(let i = 0; i < lines.length; i++) {
        const line = lines[i]
        for(let n = 0; n < line.length; n++) {
            const b = line[n]
            bs[n][b]++;
        }
    }

    var most = Object.values(bs).reduce((p, n) => { return p + (n[0] > n[1] ? '0' : '1') }, '');
    var least = Array.from(most).reduce( (p, n) => { return p + '' + Math.abs(n - 1) }, '')

    return { most, least, r: parseInt(most, 2) * parseInt(least, 2) }
}

p('03-input.txt', solve1);
p('03-input.txt', solve2);
