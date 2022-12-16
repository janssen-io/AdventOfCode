import { readAndSolve, printAnswer, test, bfs, range, search } from '../aoc.mjs'
import { Heap } from '../heap.mjs';

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
        if (timeSpent <= state.timeLeft && state.valves[valve] && state.valves[valve].state === CLOSED) {
            const pastPressure = state.pressureReleased;
            const pressureWhileWalking = sumOpenValves(state) * timeSpent;
            const valves = state.valves.clone();
            valves[valve].state = OPEN;

            nextStates.push({
                timeLeft: state.timeLeft - timeSpent,
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
            pressureReleased: pastPressure + pressureWhileWalking,
            valves: state.valves,
            currentValve: state.currentValve,
            path: state.path,
        });
    }
    return nextStates;
}

let MAX_FLOW;
const solveFor = (lines) => {
    const valves = parseValves(lines);
    const valveNames = Object.keys(valves);

    // for heap search
    MAX_FLOW = Object.values(valves).map(v => v.rate).sum();

    const valvesWithout0 = valves.filter((name, valve) => valve.rate > 0);
    const valveWithout0Names = Object.keys(valvesWithout0);

    console.log(Object.assign(valvesWithout0, {"AA": valves["AA"]}));
    // return;

    const p1Solution = scorePartition([...valveWithout0Names], valveNames, valves, 30);
    const p1Score = p1Solution.state.pressureReleased;

    let max = { p1: p1Score, p2: 0 };

    const partitions = getPartitions(valveWithout0Names);

    // p2: work partitions are mirrored, so only check half of them.
    let i = 0;
    // BFS over partition + mirrored partition
    // const partitionCount = Math.ceil(partitions.length / 2);
    // for (const myNames of partitions.slice(0, partitionCount)) {
        // const t = new Date();
        // const yourNames = valveWithout0Names.without(myNames);
        // const myScore = scorePartition(myNames, valveNames, valves, 26);
        // const yourScore = scorePartition(yourNames, valveNames, valves, 26);
        // if (myScore + yourScore > max.p2) max.p2 = myScore + yourScore;
        // console.log({ t: new Date() - t, i: `${++i}/${partitionCount}`, m: max.p2 });

    // BFS over all partition, then search for pair that doesn't turn on same valves
    const partitionScores = {};
    const partitionPaths = {};
    for (const myNames of partitions) {
        const t = new Date();
        const partSolution = scorePartition(myNames, valveNames, valves, 26);
        const partScore = partSolution.state.pressureReleased
        partitionScores[myNames.join(',')] = partScore;
        partitionPaths[myNames.join(',')] = partSolution.state.path;
        console.log({ t: new Date() - t, i: `${++i}/${partitions.length}`, score: partScore, path: partSolution.state.path });
    }
    for(let a of partitions) {
        const a_parts = a.split(',').flat();
        for (let b of partitions) {
            const b_parts = b.split(',').flat();
            if (a_parts.intersect(b_parts).length > 1) {
                continue;
            }
            // console.log(a_parts, b_parts)
            
            if (partitionScores[a] + partitionScores[b] >= max.p2) {
                max.p2 = partitionScores[a] + partitionScores[b]; 
                console.log('m:', max.p2, partitionPaths[a], partitionPaths[b])
            }
        }
    }

    // max.p2 += 2;
    return max
}

const solve = (lines) => {
    return solveFor(lines);
}

(async () => {
    let example = await readAndSolve('16.example.input', solve, '\n');
    test('Example p1', 1651, example.answer.p1)
    test('Example p2', 1707, example.answer.p2)

    console.log();

    // Too low: 1622
    // const puzzle = await readAndSolve('16.input', solve, '\n');
    // console.log(puzzle);
    // printAnswer(puzzle.answer.p1.state.pressureReleased);
})();

function scorePartition(myNames, valveNames, valves, time = 30) {
    const isMatch = (state) => state.timeLeft == 0;
    const key = (state) => `T-${state.timeLeft} P:${state.pressureReleased} V:${state.path}`;
    let show = (state) => state;
    // show = false;
    if (myNames.length !== 3 || myNames.intersect(["JJ", "BB", "CC"]).length !== 3)
        show = false;
        // throw new Error(myNames);
    else
        console.log({myNames})
    // console.log('!!', myNames, myNames.intersect(["JJ", "BB", "CC"]), typeof(show), show)

    myNames.push("AA");
    const myValves = valveNames
        .filter(v => myNames.includes(v))
        .reduce((acc, n) => { acc[n] = valves[n]; return acc; }, {});

    const myInit = generateStates({
        timeLeft: time,
        pressureReleased: 0,
        valves: myValves,
        currentValve: 'AA',
        path: ['AA']
    });

    // let heap = new Heap(x => x, cmp);
    // for (let init of myInit)
    //     heap.push(init);
    // const mySolution = Array.from(search(heap, generateStates, isMatch, key, show, Heap.prototype.shift));

    const sort = undefined;
    const mySolution = Array.from(bfs(myInit, generateStates, isMatch, key, sort, show));

    const myScore = mySolution.numSortBy(s => s.state.pressureReleased, true)[0];
    return myScore;
}

function parseValves(lines) {
    const valves = lines.map(parse).reduce((acc, valve) => {
        acc[valve.name] = valve;
        return acc;
    }, {});
    setDirections(valves);
    return valves;
}

function setDirections(valves) {
    for (let [name, valve] of Object.entries(valves)) {
        valves[name].directions = {};
        valves[name].directions = valves[name].tunnels.reduce((acc, tun) => {
            valves[name].directions = acc.merge(findNextFunctionalValves(valves, tun, valves[name].directions, 1, new Set([name])), (a, b) => a < b);
            return valves[name].directions;
        }, {});
    }
}

function findNextFunctionalValves(valves, start, distances, depth, seen = new Set()) {
    if (seen.has(start)) return distances;
    seen.add(start);

    if (valves[start].rate > 0) {
        const minDistance = Math.min(distances[start] || Number.MAX_VALUE, depth);
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

const cmp = (a, b) => {
    if (a == null) return false
    if (b == null) return false;
    function cost(s) {
        const target = s.timeLeft * MAX_FLOW;
        return target - s.timeLeft * sumOpenValves(s);
    }
    return cost(a) > cost(b);
};