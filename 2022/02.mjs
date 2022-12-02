import {readAndSolve} from '../aoc.mjs'

function scoreLine(line) {
    const scoreMap = { 'X': 1, 'Y': 2, 'Z': 3 }
    const wins = [ 'A Y', 'B Z', 'C X' ]
    const draws = ['A X', 'B Y', 'C Z']

    let score = scoreMap[line.last()];
    if (wins.includes(line)) score += 6;
    else if (draws.includes(line)) score += 3;
    
    return score;
}

function mutateLine(line) {
    const win = { 'A': 'Y', 'B': 'Z', 'C': 'X', }
    const lose = { 'A': 'Z', 'B': 'X', 'C': 'Y', }
    const draw = { 'A': 'X', 'B': 'Y', 'C': 'Z', }

    const first = line[0];
    let strategy;
    switch (line.last()) {
        case 'X': strategy = lose; break;
        case 'Y': strategy = draw; break;
        case 'Z': strategy = win; break;
    }
    return `${first} ${strategy[first]}`;
}

const solve1 = (lines) => { return lines.map(scoreLine).sum() };
const solve2 = (lines) => { return lines.map(mutateLine).map(scoreLine).sum() };
const solve = (lines) => { return { p1: solve1(lines), p2: solve2(lines) } };

readAndSolve(process.argv[2] || '02.input', solve);