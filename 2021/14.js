// file reading
const fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// Helpers
function step(template, rules) {
    let result = [template[0]];
    for(let i = 0; i < template.length - 1; i++) {
        let a = template[i];
        let b = template[i + 1];
        let pair = a + b;
        let letter = rules[pair];
        if (letter) {
            result.push(letter);
        }
        result.push(b);
    }

    return result;
}

const memo = {};
function mstep(template, rules, n) {
    // Structures repeat themselves, if we have already calculated a structure at a certain depth
    // We can just return the result.
    if (memo[template+n]) { return memo[template+n]; }

    // We should only count the left part, otherwise we count everything double.
    // mstep(AB, [AB -> C], 1)
    // = mstep(AC, ..., 0) + mstep(CB, ..., 0)
    // = { A: 1 } + {C: 1}
    // The last 'molecule' is not counted, this is taken care of in the calling function.
    if (n == 0 ) { return { [template[0]]: 1 } }

    let newTemplate = step(template, rules);
    let left = mstep([newTemplate[0], newTemplate[1]], rules, n - 1);
    let right = mstep([newTemplate[1], newTemplate[2]], rules, n - 1);

    // !! We must copy the result, otherwise we mutate the memo-table too.
    sum = {...right};
    for (let k in left) {
        sum[k] = (sum[k] || 0) + left[k];
    }

    memo[template+n] = sum;
    return sum;
}

// solvers
function solve1(lines) {
    let template = lines[0].split('');
    let rules = {};
    for(let line of lines.splice(2)) {
        let parts = line.split(' -> ');
        rules[parts[0]] = parts[1];
    }

    let newTemplate = template;
    for(let i = 1; i <= size; i++) {
        newTemplate = step(newTemplate, rules);
    }

    let quantities = {};
    for(let letter of newTemplate) {
        quantities[letter] = (quantities[letter] || 0) + 1;
    }

    let max = Math.max(...Object.values(quantities));
    let min = Math.min(...Object.values(quantities));
    console.log({max,min, r: max-min, quantities})
    return max-min;
}


// Just as slow/memory consuming as solve/1
function solvei(lines) {
    let template = lines[0].split('');
    let rules = {};
    for(let line of lines.splice(2)) {
        let parts = line.split(' -> ');
        rules[parts[0]] = parts[1];
    }

    let quantities = {};
    for(let i = 0; i < template.length - 1; i++) {
        let newTemplate = [template[i], template[i+1]];
        for(let j = 1; j <= size; j++) {
            newTemplate = step(newTemplate, rules);
        }
        for(let letter of newTemplate) {
            quantities[letter] = (quantities[letter] || 0) + 1;
        }
    }
    let max = Math.max(...Object.values(quantities));
    let min = Math.min(...Object.values(quantities));
    console.log({max,min, r: max-min})
    return max-min;
}

function memoizedSolve(lines) {
    let template = lines[0].split('');
    let rules = {};
    let quantities = {};
    for(let line of lines.splice(2)) {
        let parts = line.split(' -> ');
        rules[parts[0]] = parts[1];
    }

    // For every pair, calculate the resulting quantities and update the total quantities.
    for(let i = 0; i < template.length - 1; i++) {
        let newTemplate = [template[i], template[i+1]];
        let counts = mstep(newTemplate, rules, size);
        for(let k in counts) {
            quantities[k] = (quantities[k] || 0) + counts[k];
        }
    }

    // The recursive step function only counts all left-sides of pairs
    // This omits exactly 1 count for the last 'molecule' (AB -> ACB = {A: 1, C: 1})
    quantities[template[template.length-1]] = (quantities[template[template.length-1]] || 0) + 1;
    let max = Math.max(...Object.values(quantities));
    let min = Math.min(...Object.values(quantities));

    return max - min;
}

const size = 40;
p('14-input.txt', memoizedSolve);