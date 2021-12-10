const { create } = require('domain');

// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

const pairs = { ')': '(', ']': '[', '}': '{', '>': '<' };
const points1 = { ')': 3, ']': 57, '}': 1197, '>': 25137 };
const points2 = { '(': 1, '[': 2, '{': 3, '<': 4 };

// solvers
function solve1(lines) {
    let points = 0;

    let incompleteLines = [];
    for(let line of lines) {
        var parts = line.split('');
        var stack = [];
        for(let bracket of parts) {
            if (Object.values(pairs).includes(bracket)) {
                stack.push(bracket);
            }
            else if (stack.pop() != pairs[bracket]) {
                points += points1[bracket];
                break;
            }
        }

        incompleteLines.push(line);
    }

    return { points };
}

function solve2(lines) {
    let scores = [];

    for(let line of lines) {
        let parts = line.split('');
        let stack = [];
        let points = 0;
        let corrupt = false;
        for(let bracket of parts) {
            if (Object.values(pairs).includes(bracket)) {
                stack.push(bracket);
            }
            else if (stack.pop() != pairs[bracket]) {
                corrupt = true;
                break;
            }
        }
        if (corrupt) {
            continue;
        }

        while(stack.length > 0) {
            points = points * 5 + points2[stack.pop()];
        }

        scores.push(points);
    }

    let score = scores.sort((a,b) => a - b);
    return { score: score[(score.length - 1) / 2] }
}

p('10-input.txt', solve1);
p('10-input.txt', solve2);
