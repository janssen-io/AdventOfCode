import { readAndSolve, printAnswer, test, search, range, assert, bfs, dfs } from '../aoc.mjs'
import { Heap } from '../heap.mjs'

function parse(line) {
    const data = line.numbers();
    return {
        id: data[0],
        maxCost: {
            ore: Math.max(data[1], data[2], data[3], data[5]),
            clay: data[4],
            obsidian: data[6],
        },
        robots: {
            ore: { ore: data[1] },
            clay: { ore: data[2] },
            obsidian: { ore: data[3], clay: data[4], },
            geode: { ore: data[5], obsidian: data[6], }
        }
    }
}

const canBuild = (robot, rocks) => Object
    .entries(robot)
    .every(([resource, requirement]) => rocks[resource] >= requirement);

function build(type, robot, state) {
    state.robots[type]++;
    for (const [resource, requirement] of Object.entries(robot)) {
        state.rocks[resource] -= requirement;
    }
    // state.path.push(`${state.time}|${robot.type}`);

    // post condition
    assert(
        () => Object.values(state.rocks).every(inventory => inventory >= 0),
        "Overspent after building :" + JSON.stringify(robot));

    return state;
}

let rejected = 0;
let skipped = 0;
let times = new Set();
let maxGeode = {};

let geodeOne = {};

function nextStates(blueprint, timeLimit, heap) {
    return function (state) {
        const next = [];
        maxGeode = Math.max(maxGeode, state.rocks.geode);
        // if (state.rocks.geode < maxGeode && !canBuild(blueprint.robots.geode, state.rocks)) {
        //     return next;
        // }
        // If we can build a geode-cracker, then only do that this step.
        // Greed is good?
        // if (canBuild(blueprint.robots.geode, state.rocks)) {
        //     next.push(build('geode', blueprint.robots.geode, state.clone()));
        // }
        // else {
        for (const type of ['geode', 'obsidian', 'clay', 'ore']) {
            // for (const type of ['geode', 'obsidian', 'clay', 'ore']) {
            // don't make more robots than we can spend in one turn
            if (type !== 'geode' && blueprint.maxCost[type] <= state.robots[type]) continue;
            if (!canBuild(blueprint.robots[type], state.rocks)) continue;

            // if we could have built this last turn, we should have. Instead we are saving for something bigger.
            if (canBuild(blueprint.robots[type], {
                ore: state.rocks.ore - state.robots.ore,
                clay: state.rocks.clay - state.robots.clay,
                obsidian: state.rocks.obsidian - state.robots.obsidian,
            })) continue;

            next.push(build(type, blueprint.robots[type], state.clone()));
        }
        // doing nothing is also a valid next state
        next.push(state.clone());
        // }

        for (const n of next) {
            n.time++;

            // Add resources equal to the number of robots in the current state.
            // Newly built robots only produce resources in the next step.
            for (const [type, count] of Object.entries(state.robots)) {
                n.rocks[type] += count;
            }
        }

        maxGeode = Math.max(maxGeode, ...next.map(s => s.rocks.geode));
        return next;
    }
}

function simulate(blueprint, timeLimit) {
    console.log('sim:', blueprint.id);
    maxGeode = 0;
    times.clear();
    skipped = 0; rejected = 0;
    const sw = new Date();

    const isMatch = s => s.time == timeLimit;
    // How much of everything can we still make if we would just idle
    // const getKey = s => JSON.stringify(remainingCapacity(blueprint, timeLimit, s));
    const getKey = s => JSON.stringify(s);
    let show = s => s;
    show = false

    const init = {
        rocks: Object.keys(blueprint.robots).reduce((acc, type) => {
            acc[type] = 0;
            return acc;
        }, {}),
        robots: Object.keys(blueprint.robots).reduce((acc, type) => {
            acc[type] = 0;
            return acc;
        }, {}),
        time: 0
    }


    init.robots.ore = 1;

    Array.from(bfs([init], nextStates(blueprint, timeLimit), isMatch, getKey, show));
    const top = maxGeode;
    console.log({ id: blueprint.id, top, total: total = total + blueprint.id * top, t: new Date() - sw });
    return top;
}

let total;
const solveFor = (lines, timeLimit) => {
    total = 0;
    if (timeLimit == 24)
        return lines
            .map(parse)
            .map(bp => simulate(bp, timeLimit))
            .map((q, i) => q * (i + 1))
            .sum();
    else if (timeLimit == 32)
        return lines
            .map(parse)
            .slice(0, 3)
            .map(bp => simulate(bp, timeLimit))
            .product();

    throw 'invalid timelimit'
}

const solve = (lines) => {
    return {
        p1: solveFor(lines, 24),
        p2: solveFor(lines, 32),
    }
}

(async () => {
    let example = await readAndSolve('19.example.input', solve, '\n');
    test('Example p1', 33, example.answer.p1)
    test('Example p2', undefined, example.answer.p2)
    assert(() => 33 == example.answer.p1, "p1")

    // 1760 -- too low
    // 2259, 2264 -- wrong
    // const puzzle = await readAndSolve('19.input', solve, '\n');
    // printAnswer(puzzle);
})();
