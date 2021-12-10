const { create } = require('domain');

// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

Array.prototype.sum = function() {
    return this.reduce((p,n) => +p + +n);
}

Array.prototype.product = function() {
    return this.reduce((p,n) => +p * +n);
}

Array.prototype.without = function(other) {
    return this.filter(c => !Array.from(other).includes(c));
}

Array.prototype.intersect = function(other) {
    return this.filter(c => Array.from(other).includes(c));
}

String.prototype.sort = function() {
    return this.split('').sort().join('');
}

String.prototype.without = function(other) {
    return this.split('').without(other);
}

String.prototype.intersect = function(other) {
    return this.split('').intersect(other);
}

String.prototype.repeat = function(n) {
    if (n <= 0)
        return "";
    else
        return this + this.repeat(n - 1);
}

// solvers
function solve1(lines) {
    let points = 0;

    for(let line of lines) {
        var parts = line.split('');
        var stack = [];
        let corrupt = false;
        for(let bracket of parts) {
            if (corrupt) {
                break;
            }
            switch(bracket) {
                case '(':
                    stack.push(bracket);
                    break;
                case '{':
                    stack.push(bracket);
                    break;
                case '[':
                    stack.push(bracket);
                    break;
                case '<':
                    stack.push(bracket);
                    break;

                case ')':
                    var last = stack.pop();
                    if (last != '(') {
                        points += 3;
                        corrupt = true;
                    }
                    break;
                case '}':
                    var last = stack.pop();
                    if (last != '{') {
                        points += 1197;
                        corrupt = true;
                    }
                    break;
                case ']':
                    var last = stack.pop();
                    if (last != '[') {
                        points += 57;
                        corrupt = true;
                    }
                    break;
                case '>':
                    var last = stack.pop();
                    if (last != '<') {
                        points += 25137;
                        corrupt = true;
                    }
                    break;
            }
        }
    }
    console.log(points)
}

function autocomplete(lines) {
    let scores = [];
    for(let line of lines) {
        let points = 0;
        var parts = line.split('');
        var stack = [];
        let corrupt = false;
        for(let bracket of parts) {
            switch(bracket) {
                case '(':
                    stack.push(bracket);
                    break;
                case '{':
                    stack.push(bracket);
                    break;
                case '[':
                    stack.push(bracket);
                    break;
                case '<':
                    stack.push(bracket);
                    break;

                case ')':
                    var last = stack.pop();
                    break;
                case '}':
                    var last = stack.pop();
                    break;
                case ']':
                    var last = stack.pop();
                    break;
                case '>':
                    var last = stack.pop();
                    break;
            }
        }
        while(stack.length > 0) {
            let next = stack.pop();
            switch(next) {
                case '(':
                    points = points * 5 + 1;
                    break;
                case '{':
                    points = points * 5 + 3;
                    break;
                case '[':
                    points = points * 5 + 2;
                    break;
                case '<':
                    points = points * 5 + 4;
                    break;
            }
        }
        scores.push(points)
    }
    return scores.sort((a,b) => +a - +b);
}

function solve2(lines) {
    let incompletes = [];
    let points = 0;

    for(let line of lines) {
        var parts = line.split('');
        var stack = [];
        let corrupt = false;
        for(let bracket of parts) {
            if (corrupt) {
                break;
            }
            switch(bracket) {
                case '(':
                    stack.push(bracket);
                    break;
                case '{':
                    stack.push(bracket);
                    break;
                case '[':
                    stack.push(bracket);
                    break;
                case '<':
                    stack.push(bracket);
                    break;

                case ')':
                    var last = stack.pop();
                    if (last != '(') {
                        points += 3;
                        corrupt = true;
                    }
                    break;
                case '}':
                    var last = stack.pop();
                    if (last != '{') {
                        points += 1197;
                        corrupt = true;
                    }
                    break;
                case ']':
                    var last = stack.pop();
                    if (last != '[') {
                        points += 57;
                        corrupt = true;
                    }
                    break;
                case '>':
                    var last = stack.pop();
                    if (last != '<') {
                        points += 25137;
                        corrupt = true;
                    }
                    break;
            }
        }
        if (!corrupt) {
            incompletes.push(line);
        }
    }
    let score = autocomplete(incompletes);
    return score[(score.length - 1) / 2];
}

// p('10-example.txt', solve1);
p('10-input.txt', solve2);
