import { readAndSolve, printAnswer, test, bfs, search, genmap, dfs } from '../aoc.mjs'

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
        return Object.keys(state.valves[state.currentValve].directions)
    }
    catch (e) {
        console.error(state.valves);
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
        if (timeSpent <= state.timeLeft && state.valves[valve].state === CLOSED) {
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
            })
        }
    }
    // No closed valves reachable from current position within time limit
    // Idle and let the pressure do its work. Maybe hug an elephant while we're here.
    if (nextStates == 0) {
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
    // // open current valve
    // if (state.valves[state.currentValve].state === CLOSED) {
    //     const valves = state.valves.clone();
    //     valves[state.currentValve].state = OPEN;
    //     yield {
    //         timeLeft: state.timeLeft - 1,
    //         pressureReleased: 
    //             state.pressureReleased 
    //             + sumOpenValves(state) + valves[state.currentValve].rate,
    //             // valves[state.currentValve].rate * (state.timeLeft - 1),
    //         valves,
    //         currentValve: state.currentValve,
    //         path: [...state.path, OPEN],
    //     }
    // }
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
const solveFor = (lines) => {
    const valves = lines.map(parse).reduce((acc, valve) => {
        acc[valve.name] = valve;
        return acc;
    }, {});

    for (let [name, valve] of Object.entries(valves)) {
        // console.log('Checking tunnels for', name);
        valves[name].directions = {};
        valves[name].directions = valves[name].tunnels.reduce((acc, tun) => {
            valves[name].directions = acc.merge(findNextFunctionalValves(valves, tun, valves[name].directions, 1, new Set([name])), (a, b) => a < b);
            return valves[name].directions;
        }, {})
    }
    // return valves;
    const valvesWithout0 = valves.filter((name, valve) => valve.rate > 0);
    // return valvesWithout0;
    // console.log(valves)
    // const initialStates = Object.entries(valves['AA'].directions).map(([tun, distance]) => {
    //     return {
    //         timeLeft: 30 - distance,
    //         pressureReleased: 0,
    //         valves: valvesWithout0,
    //         currentValve: tun,
    //         path: ['AA', tun]
    //     }
    // });
    // console.log(valvesWithout0)
    const initialStates = generateStates({
        timeLeft: 30,
        pressureReleased: 0,
        valves: valves,
        currentValve: 'AA',
        path: ['AA']
    })

    const isMatch = (state) => {
        return state.timeLeft == 0
    }

    const key = (state) => {
        return `T-${state.timeLeft} P:${state.pressureReleased} V:${state.path}`;
    }

    let show = (state) => state.path;
    // show = false;

    const minRate = Math.min(...Object.values(valves).map(v => v.rate).filter(r => r > 0));
    const maxRate = Math.max(...Object.values(valves).map(v => v.rate));
    const maxFlow = Object.values(valves).map(v => v.rate).sum();
    const medianRate = (() => {
        const rates = Object.values(valves).map(v => v.rate); //.filter(r => r > 0);
        if (rates.length % 2 == 0) {
            return (rates[rates.length / 2] + [rates.length / 2 - 1]) / 2
        }
        return (rates[Math.floor(rates.length / 2)]);
    })();
    const averageRate = (() => {
        const rates = Object.values(valves).map(v => v.rate); //.filter(r => r > 0);
        return rates.sum() / rates.length;
    })();

    const cmp = (a, b) => {
        if (a == null) return false
        if (b == null) return false;
        function cost(s) {
            const current = s.pressureReleased + (s.timeLeft - 1) * sumOpenValves(s);
            // return (30 * maxFlow - current)
            // return (s.timeLeft * maxFlow - (s.timeLeft * sumOpenValves(s)))
            // return maxFlow - sumOpenValves(s);
            const target = s.timeLeft * maxFlow;
            return target - s.timeLeft * sumOpenValves(s);
            if (s.path.last() === OPEN) {
                let c = s.pressureReleased + s.timeLeft * sumOpenValves(s);
                return c;
            }
            else {
                let c = s.pressureReleased + sumOpenValves(s) + (s.timeLeft - 1) * (sumOpenValves(s) + s.valves[s.currentValve].rate)
                return c;
            }
        }
        // if (a.timeLeft == b.timeLeft) return b.pressureReleased > a.pressureReleased;
        // return (b.pressureReleased + b.timeLeft * sumOpenValves(b)) > (a.pressureReleased + a.timeLeft * sumOpenValves(a))
        return cost(a) > cost(b);
    };

    const heap = new Heap(state => state || null, cmp);
    for (let init of initialStates) {
        heap.push(init);
    }

    // let r = search(heap, generateStates, isMatch, key, show, Heap.prototype.shift)
    let r = bfs(initialStates, generateStates, isMatch, key, show);
    let x = [];
    let max = -1;
    let rr = Array.from(r);
    // console.log(rr.length);
    return rr.map(s => s.state.pressureReleased).numSort().reverse().slice(0,50);
    // for (let rr of rr) {
    //     // console.log(rr.state.pressureReleased);
    //     if (rr.state.pressureReleased > max) {
    //         max = rr.state.pressureReleased;
    //         x = rr.state;
    //     }
    // }
    // return max;
    return { max, x }
    // console.log(r.next().value.state);
    // for(let i = 0; i < 10; i ++) {
    //     x.push(r.next().value.state.pressureReleased)
    // }
    return x;
}

const solveFor2 = (lines) => {
}

const solve = (lines) => {
    return {
        p1: solveFor(lines),
        // p2: solveFor2(lines, max),
    }
}

(async () => {
    let example = await readAndSolve('16.example.input', solve, '\n');
    test('Example p1', 1651, example.answer.p1)
    test('Example p2', 5, example.answer.p2)

    console.log();

    const puzzle = await readAndSolve('16.input', solve, '\n');
    console.log(puzzle);
    // printAnswer(puzzle.answer.p1.state.pressureReleased);
})();