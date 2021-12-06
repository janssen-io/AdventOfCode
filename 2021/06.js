// file reading
fs = require('fs')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// helper functions
// part 1
// function next(state) {
//     var newState = [];
//     var newFish = []
//     for(let i = 0; i < state.length; i++) {
//         let fish = state[i];
//         if (fish == 0) {
//             newState[i] = 6;
//             newFish.push(8);
//         }
//         else {
//             newState[i] = fish - 1;
//         }
//     }

//     return newState.concat(newFish);
// }

// part2
function next(state) {
    let newFishes = state[0];
    let oldFishes = state.slice(1)
    oldFishes.push(newFishes);
    oldFishes[6] += newFishes;

    return oldFishes;
}

// solvers
function solve1(lines) {
    var state = lines[0].split(',').map(x => +x);
    var fishes = [0,0,0,0,0,0,0,0,0]; // 0 - 8
    for(let fish of state) {
        fishes[fish]++;
    }
    for(var i = 0; i < 256; i++) {
        fishes = next(fishes);
    }

    return {a: fishes.reduce( (p,n) => p+n), state}

}

// p('05-example.txt', solve1);
p('06-input.txt', solve1);
