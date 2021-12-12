// file reading
const fs = require('fs')
require('./aoc.js')

function p(input, solver) {
    fs.readFile(input, 'utf8', function(error, data) {
        var lines = data.trim().split('\n');
        console.log(input, solver(lines));
    });
}

// Helpers
function build(lines) {
    let graph = {}
    for(let line of lines) {
        const p = line.split('-');
        graph[p[0]] = graph[p[0]] || [];
        graph[p[0]].push(p[1]);

        // The graph is bidirected, if a->b then also b->a
        graph[p[1]] = graph[p[1]] || [];
        graph[p[1]].push(p[0]);
    }
    return graph;
}

class Breadcrumb {
    constructor(node, previous = [], smallCounts = {}) {
        this.name = node;
        this.crumbs = [...previous];
        this.isBig = node.toUpperCase() === node;
        this.smallCounts = smallCounts.clone();
    }

    add(name) {
        this.crumbs.push(name);
        if (name === name.toLowerCase()) {
            this.smallCounts[name] = (this.smallCounts[name] || 0) + 1;
        }
    }
}

// solvers
function solve(lines) {
    const caves = build(lines);
    const todo = [new Breadcrumb('start', [])];
    const paths = [];

    while(todo.length) {
        let next = todo.pop();

        if (next.name === 'end') {
            // Not strictly necessary, but nicer for debugging
            // next.crumbs.push('end');
            paths.push(next.crumbs);
            continue;
        }

        if (next.crumbs.includes(next.name) && !next.isBig) {
            if (!isPart2 || (next.name == 'start'))
                continue;

            // The old solution was not very efficient:
            // let hasMultipleSmall = next.crumbs
            //             .filter(c => c.isLower())
            //             .map(c => next.crumbs.filter(cc => cc === c).length)
            //             .filter(l => l > 1).length
            if (Object.values(next.smallCounts).some(count => count === 2))
                continue;
        }

        // Add ourselves to the bread crumb of the next iteration
        // If we don't do this, the path won't extend.
        // If we do this earlier, then the stop checks above are wrong.
        next.add(next.name);
        for(let to of caves[next.name]) {
            todo.push(new Breadcrumb(to, next.crumbs, next.smallCounts))
        }
    }

    // return { l: paths.length, paths };
    return { l: paths.length };
}

const isPart2 = true;
p('12-input.txt', solve);