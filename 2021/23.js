require('./aoc.js');

const SENTINEL = '.';
const expected = ['A', 'B', 'C', 'D'];
const init = {
    buckets: [
        ['B', 'A'],
        ['C', 'D'],
        ['D', 'B'],
        ['A', 'C']
    ],
    bucketSize: 2,
    energyConsumed: 0,
    corridor: [SENTINEL].repeat(7),
    steps: 0
}

// for testing purposes
const done = {
    buckets: [
        ['A', 'A'],
        ['B', 'B'],
        ['C', 'C'],
        ['D', 'D']
    ],
    bucketSize: 2,
    energyConsumed: 0,
    corridor: [SENTINEL].repeat(7),
    steps: 0
}

const init2 = {
    buckets: [
        ['B', 'D', 'D', 'A'],
        ['C', 'B', 'C', 'D'],
        ['D', 'A', 'B', 'B'],
        ['A', 'C', 'A', 'C']
    ],
    bucketSize: 4,
    energyConsumed: 0,
    corridor: [SENTINEL].repeat(7),
    steps: 0
}
/*
01  2  3  4  56
  B0 B1 B2 B3
*/

function range(a, b) {
    if (a > b) [a, b] = [b, a];
    const r = [];
    for(let i = a; i <= b; i++) r.push(i);
    return r;
}

function slotsBetween(slot, bucket) {
    if (slot - 1 <= bucket) {
        // slot + 1, because we don't want to check the given slot itself
        // bucket + 1, because that's the first slot left of the bucket
        return slot - 1 == bucket ? [] : range(slot + 1, bucket + 1);
    } else {
        // bucket + 2, because that is the first slot right of the bucket
        // slot - 1, because we don't want to check the given slot itself
        return bucket + 2 > slot - 1 ? [] : range(bucket + 2, slot - 1);
    }
}

const isPathFree = (state, slot, bucket) => slotsBetween(slot, bucket).every(i => state.corridor[i] == SENTINEL);
const isBucketEmpty = (state, bucket) => !state.buckets[bucket].length;
const isBucketFull  = (state, bucket) => state.buckets[bucket].length == state.bucketSize;
const isSlotTaken = (state, slot) => state.corridor[slot] != SENTINEL;

function moveOut(state, bucket, slot) {
    if (isBucketEmpty(state, bucket)) throw new Error('Bucket is empty');
    if (!isPathFree(state, slot, bucket)) throw new Error('Path is blocked');

    const newState = state.clone();

    const corridorDistance = 2 * (slotsBetween(slot, bucket).length + 1);
    const extra = slot == 0 || slot == 6 ? -1 : 0;
    const bucketDistance = state.bucketSize - state.buckets[bucket].length;

    const pod = newState.buckets[bucket].pop();
    newState.energyConsumed += (corridorDistance + extra + bucketDistance) * (10 ** expected.indexOf(pod));
    newState.corridor[slot] = pod; // pop from the newState, so the buckets are updated in the right state
    newState.steps++;
    return newState;
}

function moveIn(state, slot, bucket) {
    if (isBucketFull(state, bucket)) throw new Error('Bucket is full');
    if (!isPathFree(state, slot, bucket)) throw new Error('Path is blocked');

    const newState = state.clone();

    const corridorDistance = 2 * (slotsBetween(slot, bucket).length + 1);
    const extra = slot == 0 || slot == 6 ? -1 : 0;
    const bucketDistance = Math.max(0, state.bucketSize - state.buckets[bucket].length - 1);
    const pod = newState.corridor[slot];

    newState.energyConsumed += (corridorDistance + extra + bucketDistance) * (10 ** expected.indexOf(pod));
    newState.buckets[bucket].push(pod);
    newState.corridor[slot] = SENTINEL;
    newState.steps++;
    return newState;
}

function genStates(state) {
    const states = [];
    for(let slot = 0; slot < 7; slot++) {
        let pod = state.corridor[slot];
        if (pod == SENTINEL) continue;

        for(let bucket = 0; bucket < 4; bucket++) {
            let goal = expected[bucket];
            if (isBucketFull(state, bucket)) continue;      // bucket is full, can't put pods in
            if (!isPathFree(state, slot, bucket)) continue; // can't reach bucket, path is blocked
            if (!state.buckets[bucket].every(p => p == goal)) continue;  // don't move things on top of pods that still need to move out
            if(pod != goal) continue;                       // the pod does not belong in this bucket

            states.push(moveIn(state, slot, bucket));
        }
    }

    // Prefer moving pods into their buckets.
    if (states.length > 0) return states;

    for(let slot = 0; slot < 7; slot++) {
        for(let bucket = 0; bucket < 4; bucket++) {
            let goal = expected[bucket];
            if (isBucketEmpty(state, bucket)) continue;      // nothing to move out
            if (isSlotTaken(state, slot)) continue;          // can't move onto slot
            if (!isPathFree(state, slot, bucket)) continue;  // can't move, path is blocked
            if (state.buckets[bucket].every(p => p == goal)) continue;  // everything in the bucket should be

            states.push(moveOut(state, bucket, slot));
        }
    }

    return states;
}

function isSorted(state) {
    for(let i = 0; i < expected.length; i++) {
        if (state.buckets[i].length != state.bucketSize
            || state.buckets[i].some(pod => pod != expected[i]))
            {
                return false;
            }
    }
    return true;
}

function show(state) {
    const print = (i) => state.corridor[i] == SENTINEL ? '.' : state.corridor[i];
    let corridorIndices = ' ' + '01 2 3 4 56' + ' ';
    let ceiling = '┌' + '─'.repeat(11) + '┐';
    let corridor = '│' + print(0) + [1,2,3,4,5].map(print).join('.') + print(6) + '│';
    let buckets = [];
    let lastIndex = state.bucketSize - 1;
    for(let i = 0; i <= lastIndex; i++) {
        let row = [];
        for(let j = 0; j < state.buckets.length; j++) {
            let pod = state.buckets[j][lastIndex - i] || ' ';
            pod = expected[j] == pod ? ('' + pod).green() : ('' + pod).red();
            row.push(pod);
        }
        buckets.push(row);
    }
    let topRow = '└─┐' + [' '].repeat(buckets[0].length).join('┬') + '┌─┘';
    let nextRows = buckets.slice(0, buckets.length).map(b =>
                 '  │' + b.join('│') + '│').join("\n");
    let lastRow ='  └' + '─'.repeat(state.buckets.length * 2 - 1) + '┘';
    let bucketIndices = '   ' + '0 1 2 3';

    return [corridorIndices, ceiling, corridor, topRow, nextRows, lastRow, bucketIndices].join("\n");
}

function solve(initialState, maxSteps) {
    const seen = {};
    const key = state => `${state.corridor.join('')}_${state.buckets.map(b => b.join('')).join(' ')}`;

    let q = [initialState];
    const sols = [];

    let i = 0;
    while(q.length) {
        if (maxSteps && i > maxSteps) {
            return;
        }
        i++;
        const state = q.pop(); // pop = DFS, shift = BFS
        if (maxSteps) {
            console.log('─'.repeat(13));
            console.log({ sols: sols.length, energy: state.energyConsumed, steps: state.steps, q: q.length });
            console.log(show(state));
        }
        if(i % 100000 == 0) {
            console.log(key(state));
            console.log(i, sols.length, state.steps, q.length);
        }
        seen[key(state)] = state.energyConsumed;
        for(let nextState of genStates(state)) {
            if (isSorted(nextState)) {
                sols.push(nextState.energyConsumed);
                continue;
            }

            let k = key(nextState);
            if (seen[k] && seen[k] <= nextState.energyConsumed) {
                continue;
            }
            q.push(nextState);
        }
        q = q.sort((a,b) => a.energyConsumed - b.energyConsumed); // for priority queue
    }

    return sols.sort();
}

let steps = process.argv[2];
let s = solve(init, steps);
console.log(s);
