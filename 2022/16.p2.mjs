import { readAndSolve, printAnswer, test, bfs, range, genmap, dfs, assert } from '../aoc.mjs'

import { createMaxHeap, createMinHeap, Heap } from '../heap.mjs';

const CLOSED = 0;
const OPEN = 1;

function parse(line) {
    const rate = line.numbers()[0];
    const words = line.split(' ');
    const valves = words
        .filter(word => /[A-Z]{2}/.test(word))
        .map(word => word.replace(',', ''));
    return {
        name: valves[0],
        rate,
        tunnels: valves.slice(1),
        state: CLOSED,
    }
}

const openValves = state => Object
    .values(state.valves)
    .filter(v => v.state === OPEN)
    .map(v => v.name);

const sumOpenValves = state => Object
    .values(state.valves)
    .filter(v => v.state === OPEN)
    .map(v => v.rate)
    .sum();

// const adjacentValves = state => state.valves[state.currentValve].tunnels;
const adjacentValves = state => {
    try {
        // if (state.currentValve != 'AA') console.log(state.valves[state.currentValve])
        return Object.keys(state.valves[state.currentValve].directions)
    }
    catch (e) {
        console.error(state);
        throw e;
    }
}

function generateStates(state) {
    const nextStates = []
    if (state.timeLeft == 0) return nextStates;

    for (let valve of adjacentValves(state)) {
        // Walk to any closed valve and open it.
        const distance = state.valves[state.currentValve].directions[valve];
        const timeSpent = distance + 1; // + 1 for opening
        if (timeSpent <= state.timeLeft && state.valves[valve] && state.valves[valve].state === CLOSED) {
            const pastPressure = state.pressureReleased;
            const pressureWhileWalking = sumOpenValves(state) * timeSpent;
            // console.log(openValves(state), pressureWhileWalking);
            const valves = state.valves.clone();
            valves[valve].state = OPEN;

            nextStates.push({
                timeLeft: state.timeLeft - distance - 1,
                // pressureReleased: state.pressureReleased,
                pressureReleased: pastPressure + pressureWhileWalking,
                valves: valves,
                currentValve: valve,
                path: [...state.path, valve],
            });
        }
    }
    // No closed valves reachable from current position within time limit
    // Idle and let the pressure do its work. Maybe hug an elephant while we're here.
    if (nextStates > 0) {
        const pastPressure = state.pressureReleased;
        const pressureWhileWalking = sumOpenValves(state) * state.timeLeft;
        nextStates.push({
            timeLeft: 0,
            // pressureReleased: state.pressureReleased,
            pressureReleased: pastPressure + pressureWhileWalking,
            valves: state.valves,
            currentValve: state.currentValve,
            path: state.path,
        });
    }

    return nextStates;

}

function findNextFunctionalValves(valves, start, distances, depth, seen = new Set()) {
    if (seen.has(start)) return distances;
    seen.add(start);

    if (valves[start].rate > 0) {
        const minDistance = Math.min(distances[start] || Number.MAX_VALUE, depth);
        // console.log('distance to', start, 'is', minDistance, distances)
        distances[start] = minDistance;
    }

    return valves[start].tunnels.reduce((acc, tunnel) => {
        return acc.merge(findNextFunctionalValves(valves, tunnel, distances, depth + 1, seen), (a, b) => a < b);
    }, {})
}

function getPartitions(valveNames) {
    return range(0, valveNames.length + 1)
        .map(length => calculatePartitions(valveNames, length))
        .flat();

    function calculatePartitions(valves, length) {
        if (length == 0) { return [[]]; }
        if (valves.length == 0) { return [] }
        const [head, ...tail] = valves;
        const ph = calculatePartitions(tail, length - 1).map(p => [head, ...p])
        const pt = calculatePartitions(tail, length);
        return ph.concat(pt);
    }
}

function setAdjacencyOnValves(valves) {
    for (let [name, valve] of Object.entries(valves)) {
        valves[name].directions = {};
        valves[name].directions = valves[name].tunnels.reduce((acc, tun) => {
            valves[name].directions = acc.merge(findNextFunctionalValves(valves, tun, valves[name].directions, 1, new Set([name])), (a, b) => a < b);
            return valves[name].directions;
        }, {})
    }
}

function removeUnreachable(subset) {
    const reachables = Object.keys(subset);
    // console.log(reachables);
    const result = {}
    for (let [key, item] of Object.entries(subset)) {
        result[key] = item.clone();
        result[key].directions = item.directions.filter((key) => {
            return reachables.includes(key)
        });
    }
    return result;
}

function getInitialStates(valves, valvePartition, time = 30) {
    return generateStates({
        timeLeft: time,
        pressureReleased: 0,
        valves: valvePartition, //.merge({ 'AA': valves['AA'] }),
        currentValve: 'AA',
        path: ['AA']
    }).map(s => { s.valves = valvePartition; /*removeUnreachable(valvePartition);*/ return s; });
}

const solveFor = (lines, time) => {
    const valves = lines.map(parse).reduce((acc, valve) => { acc[valve.name] = valve; return acc; }, {});
    const valveNames = Object.keys(valves);

    setAdjacencyOnValves(valves);

    // const valvesWithout0 = valves.filter((name, valve) => valve.rate > 0);
    // const valveWithout0Names = Object.keys(valvesWithout0);

    const partitions = getPartitions(valveNames);

    const max = {
        p1: 0, p2: 0
    }
    let i = 0;
    const seen = new Set();
    for (const myValveNames of partitions) {
        const yourValveNames = valveNames.without(myValveNames);
        myValveNames.push("AA");
        yourValveNames.push("AA");
        const myValves = valveNames
            .filter(v => myValveNames.includes(v))
            .reduce((acc, n) => { acc[n] = valves[n]; return acc; }, {});

        const yourValves = valveNames
            .filter(v => yourValveNames.includes(v))
            .reduce((acc, n) => { acc[n] = valves[n]; return acc; }, {});

        // partition and its inverse are symmetrical (or should be)
        // prevent elephant from walking my routes
        // cut down search space in half
        if (time == 26) {
            let k = Object.keys(myValves).join(',');
            let h = Object.keys(yourValves).join(',');
            if (seen.has(k) || seen.has(h)) {
                continue;
                // console.log("seen", k, h)
            };
            seen.add(k);
            seen.add(h);
        }

        const isMatch = (state) => state.timeLeft == 0;
        const key = (state) => `T-${state.timeLeft} P:${state.pressureReleased} V:${state.path}`;
        let show = (state) => state.path;
        // show = false;

        const getResult = g => g.map(s => s.state.pressureReleased).numSort(true)[0] || 0
        const result = {} // p1: getResult(p1), p2: getResult(mine) + getResult(yours) };

        // console.log('=== p1 ===');
        const p1States = getInitialStates(valves, myValves, 30)
        const p1 = Array.from(bfs(p1States, generateStates, isMatch, key, show));
        result.p1 = getResult(p1);
        if (result.p1 > max.p1) {
            console.log('p1', p1[0].i, result.p1, p1[0].state.path, myValveNames);
            max.p1 = result.p1
        }
        // console.log('=== p2 ===');
        // const myInitialStates = getInitialStates(valves, myValves, 26);
        // const yourInitialStates = getInitialStates(valves, yourValves, 26);
        // console.log(myInitialStates.valves);
        // console.log(yourInitialStates.valves);
        // const mine = Array.from(bfs(myInitialStates, generateStates, isMatch, key, show));
        // const yours = Array.from(bfs(yourInitialStates, generateStates, isMatch, key, show));
        // result.p2 = getResult(mine) + getResult(yours);
        // console.log(result.p2)
        // if (result.p2 > max.p2) {
        //     console.log('p2', result.p2, mine[0] && mine[0].state.path, myValveNames.length, yourValveNames.length);
        //     max.p2 = result.p2
        // }
    }
    return max;
}

const solve = (lines) => {
    return {
        p1: solveFor(lines, 30).p1,
        p2: solveFor(lines, 26).p2,
    }
}

(async () => {
    let example = await readAndSolve('16.example.input', solve, '\n');
    test('Example p1', 1651, example.answer.p1)
    test('Example p2', 5, example.answer.p2)

    console.log();

    // const puzzle = await readAndSolve('16.input', solve, '\n');
    // console.log(puzzle);
    // printAnswer(puzzle.answer.p1.state.pressureReleased);
})();