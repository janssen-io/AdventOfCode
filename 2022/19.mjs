import { readAndSolve, printAnswer, test, search, range, assert, bfs, dfs } from '../aoc.mjs'

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

let times = new Set();
let maxGeode = 0;
let maxGeodeT = {};
function nextStates(blueprint) {
    console.log({ bp: blueprint.robots, cost: blueprint.maxCost })
    return function (state) {
        const next = [];
        maxGeodeT[state.time] = maxGeodeT[state.time] || 0;
        maxGeodeT[state.time] = Math.max(maxGeodeT[state.time], state.rocks.geode);
        maxGeode = Math.max(maxGeode, state.rocks.geode);

        if (state.time > 0 && state.rocks.geode < maxGeodeT[state.time]) return next;

        // save up for something
        next.push(state.clone());

        if (canBuild(blueprint.robots.geode, state.rocks)) {
            const stateWithNewRobot = build('geode', blueprint.robots.geode, state.clone());
            next.push(stateWithNewRobot);
        }
        else if(canBuild(blueprint.robots.obsidian, state.rocks)){
            const stateWithNewRobot = build('obsidian', blueprint.robots.obsidian, state.clone());
            next.push(stateWithNewRobot);
        }
        else
        // or build something if we can
        for (const type of ['clay', 'ore']) {
            // for (const type of ['geode', 'obsidian', 'clay', 'ore']) {
            // don't make more robots than we can spend in one turn
            // if (type !== 'geode' && blueprint.maxCost[type] <= state.robots[type]) continue;
            if (!canBuild(blueprint.robots[type], state.rocks)) continue;

            // if we could have built this last turn, we should have. 
            // instead we are saving for something bigger.
            if (canBuild(blueprint.robots[type], {
                ore: state.rocks.ore - state.robots.ore,
                clay: state.rocks.clay - state.robots.clay,
                obsidian: state.rocks.obsidian - state.robots.obsidian,
            })) continue;

            const stateWithNewRobot = build(type, blueprint.robots[type], state.clone());
            next.push(stateWithNewRobot);
        }

        for (const n of next) {
            n.time++;

            // Add resources equal to the number of robots in the current state.
            // Newly built robots only produce resources in the next step.
            for (const [type, count] of Object.entries(state.robots)) {
                n.rocks[type] += count;
            }
        }

        // maxGeode = Math.max(maxGeode, ...next.map(s => s.rocks.geode));
        // return next;
        return next.filter(x => x.rocks.geode >= maxGeodeT[state.time]);
    }
}

function simulate(blueprint, timeLimit) {
    maxGeode = 0;
    maxGeodeT = {};
    times.clear();

    console.log('sim:', blueprint.id);
    const isMatch = s => s.time == timeLimit;
    // How much of everything can we still make if we would just idle
    // const getKey = s => JSON.stringify(remainingCapacity(blueprint, timeLimit, s));
    const getKey = s => JSON.stringify([s.time,s.rocks,s.robots]);
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

    const solutions = Array.from(bfs([init], nextStates(blueprint, timeLimit), isMatch, getKey, show));
    const top = solutions.numSortBy(true, s => s.state.rocks.geode)[0]?.state.rocks.geode || 0;
    // console.log({ top, maxGeode, maxGeodeT });
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
        // p1: solveFor(lines, 24),
        p2: solveFor(lines, 32),
    }
}

(async () => {
    // let example = await readAndSolve('19.example.input', solve, '\n');
    // test('Example p1', 1 * 9 + 2 * 12, example.answer.p1)
    // test('Example p2', 56 * 62, example.answer.p2)
    // assert(() => 33 == example.answer.p1, "p1")

    // 1760, 3570 (17*7*30)-- too low
    const puzzle = await readAndSolve('19.input', solve, '\n');
    printAnswer(puzzle);
})();
