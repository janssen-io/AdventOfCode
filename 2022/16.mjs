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
    let t = new Date();
    console.log(` === PARSE (${new Date() - t}) ===`)
    const valves = parseValves(lines);
    const valveNames = Object.keys(valves);

    // for heap search
    MAX_FLOW = Object.values(valves).map(v => v.rate).sum();

    console.log(` === PRUNE (${new Date() - t}) ===`)
    const valvesWithout0 = valves.filter((name, valve) => valve.rate > 0);
    const valveWithout0Names = Object.keys(valvesWithout0);

    // console.log(Object.assign(valvesWithout0, { "AA": valves["AA"] }));
    // return;

    console.log(` === P1 (${new Date() - t}) ===`)
    let p1Score;
    // const p1Solution = scorePartition([...valveWithout0Names], valveNames, valves, 30);
    // p1Score = p1Solution.state.pressureReleased;

    let max = { p1: p1Score, p2: 0 };

    console.log(` === P2 (${new Date() - t}) ===`)
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
    console.log(` === SCORE (${new Date() - t}) ===`)
    
    // optimal performance is probably somewhere in the middle,
    // dividing the work between me and my 🐘-buddy.
    const half = Math.floor(valveWithout0Names.length / 2);
    const searchPartitions = partitions.filter(l => l.length > half - 1 && l.length < half + 1);
    for (const myNames of searchPartitions) {
        const partSolution = scorePartition(myNames, valveNames, valves, 26);
        const partScore = partSolution.state.pressureReleased
        partitionScores[myNames.join(',')] = partScore;
        partitionPaths[myNames.join(',')] = partSolution.state.path;
        i++;
        if (i % 100 == 0) {
            console.log({ t: new Date() - t, i: `${i}/${searchPartitions.length}`, score: partScore, path: partSolution.state.path });
        }
    }
    console.log(` === PAIR (${new Date() - t}) ===`)
    console.log()
    for (let i = 0; i < searchPartitions.length; i++) {
        const a_parts = searchPartitions[i];
        const a = a_parts.join(',')
        for (let j = i; j < searchPartitions.length; j++) {
            const b_parts = searchPartitions[j].split(',').flat();
            const b = b_parts.join(',')
            // if they share valves, it's not valid
            if (a_parts.intersect(b_parts).length > 1) {
                continue;
            }

            if (partitionScores[a] + partitionScores[b] > max.p2) {
                max.p2 = partitionScores[a] + partitionScores[b];
                console.log('new max', {t: new Date() - t, max: max.p2, pathA: partitionPaths[a], pathB: partitionPaths[b] });
            }
            if (i % 1_000 == 0) {
                console.log({t: new Date() - t, i, max: max.p2, pathA: partitionPaths[a], pathB: partitionPaths[b] });
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

    // console.log();

    const puzzle = await readAndSolve('16.input', solve, '\n');
    console.log(puzzle);
    printAnswer(puzzle.answer);
})();

function scorePartition(myNames, valveNames, valves, time = 30) {
    const isMatch = (state) => state.timeLeft == 0;
    const key = (state) => `T-${state.timeLeft} P:${state.pressureReleased} V:${state.path}`;
    let show = (state) => state;
    show = false;

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

    // Heap with current cost heuristic seems to perform worse than regular BFS.
    let heap = new Heap(x => x, cmp);
    for (let init of myInit)
        heap.push(init);
    const mySolution = Array.from(search(heap, generateStates, isMatch, key, show, Heap.prototype.shift));

    // const mySolution = Array.from(bfs(myInit, generateStates, isMatch, key, show));

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
    const adjacencies = computeAdjacencyListUndirected(
        valves,
        v => v.tunnels,
        (origin, target) => valves[target].rate == 0);

    for (const [name, valve] of Object.entries(valves)) {
        valve.directions = adjacencies[name];
    }
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

// Graph: [node]: { [key]: [nodes] }
function computeAdjacencyListUndirected(graph, selector = v => v.vertices, skipWhen = (origin, target) => false) {
    const distances = {};
    const nodes = Object.keys(graph);
    for (const node of nodes) {
        distances[node] = {};
        for (const other of nodes) {
            if (skipWhen(node, other)) continue;
            if (node == other) continue;
            distances[node][other] = findPath(graph, selector, node, other);
        }
    }
    return distances;
}

function findPath(graph, selector, origin, target) {
    const init = { current: origin, distance: 0 };
    const isMatch = s => s.current == target;
    const key = s => s.current;
    const genState = function* (state) {
        // console.log(graph, state.current)
        for (const next of selector(graph[state.current])) {
            yield {
                current: next,
                distance: state.distance + 1
            }
        }
    }

    const routes = bfs([init], genState, isMatch, key, undefined, false);
    return routes.next().value.state.distance;
}