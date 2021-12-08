const { create } = require('domain');

// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

const numSegments = {
    1: 2,
    4: 4,
    7: 3,
    8: 7
}

// solvers
function solve1(lines) {
    var parts = lines.map(line => line.split(' | '));
    var signals = parts.map(delimited => { return { signals: delimited[0].split(' '), output: delimited[1].split(' ')}})
    let result = 0;
    for(let signal of signals) {
        result += signal.output.filter(x => Object.values(numSegments).includes(x.length)).length;
    }
    return result;
}

String.prototype.sort = function() {
    return this.split('').sort().join('');
}

String.prototype.without = function(other) {
    return this.split('').filter(c => !Array.from(other).includes(c));
}

String.prototype.repeat = function(n) {
    if (n <= 0)
        return "";
    else
        return this + this.repeat( n - 1);
}

// Force singletons, so errors are easier to spot.
Array.prototype.single = function() {
    if (this.length != 1) {
        console.error(this);
        throw Error("Array of length " + this.length + " is not of length 1");
    }
    return this[0];
}

function solve2(lines) {
    var parts = lines.map(line => line.split(' | '));
    var entries = parts.map(delimited => { return { signals: delimited[0].split(' '), output: delimited[1].split(' ')}})
    let result = 0;
    for(let entry of entries) {
        let decoded = {};
        let display = {};
        decoded[1] = entry.signals.find(s => s.length == 2).sort();
        decoded[7] = entry.signals.find(s => s.length == 3).sort();
        decoded[4] = entry.signals.find(s => s.length == 4).sort();
        decoded[8] = entry.signals.find(s => s.length == 7).sort();

        decoded[6] = entry.signals
            .filter(s => s.length == 6)
            .filter(s => s.without(decoded[1]).length == 5)
            .single();

        entry.signals = entry.signals.filter(s => s != decoded[6])

        decoded[5] = entry.signals
            .filter(s => s.length == 5)
            .filter(s => s.without(decoded[6]).length == 0)
            .single()

        entry.signals = entry.signals.filter(s => s != decoded[5])

        decoded[3] = entry.signals
            .filter(s => s.length == 5)
            .filter(s => s.without(decoded[1]).length == 3)
            .single()

        entry.signals = entry.signals.filter(s => s != decoded[3])

        display[2] = decoded[8].without(decoded[6]).single();

        decoded[9] = entry.signals
            .filter(s => s.length == 6)
            .filter(s => {
                var x = s.without(decoded[5]);
                var y = display[2];
                return x.length == 1 && x[0] == y;
            })
            .single()

        entry.signals = entry.signals.filter(s => s != decoded[9])

        display[5] = decoded[1].without(display[2]).single();

        decoded[2] = entry.signals
            .filter(s => s.length == 5)
            .single();

        decoded[0] = entry.signals
            .filter(s => s.length == 6)
            .single();


        for(let i = 0; i < 10; i++) {
            decoded[i] = decoded[i].sort();
        }

        var num = entry.output.map(n => {
            let sortedN = n.sort();
            for (let i = 0; i < 10; i++) {
                if(decoded[i] == sortedN) {
                    return i;
                }
            }
        }).join('');

        result += +num;
    }
    return result;
}

p('08-input.txt', solve2);
// p('08-input.txt', solve1);
