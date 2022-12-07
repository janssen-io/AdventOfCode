import { readAndSolve, range } from '../aoc.mjs'

function addNode(filetree, node, size = 0) {
    filetree.children = filetree.children || {};
    if (size == 0) {
        filetree.children[node] = { name: node, size: 0, children: [] }
    }
    else {
        filetree.children[node] = { name: node, parent: filetree.name, size, children: [] };
    }
}

function getSubtree(tree, path) {
    return path.reduce((subtree, dir) => {
        return subtree.children[dir];
    }, tree);
}

function computeSize(tree) {
    if (tree.size > 0) {
        return tree.size;
    }
    if (typeof(tree.children) == 'object' && Object.keys(tree.children).length) {
        Object.values(tree.children).map(computeSize);
        tree.size = Object.values(tree.children).reduce((sum, child) => sum + child.size, 0);
    }
    else {
        console.debug('🎄tree', tree)
    }
}

function sumIf(tree, fn, sum = 0) {
    if (tree.children && Object.keys(tree.children).length) {
        const _ = Object.values(tree.children).map(t => sumIf(t, fn, 0));
        if (fn(tree)) {
            // console.log(tree.name);
        }
        return _.sum() + (fn(tree) ? tree.size : 0)
    }
    return [];
}
function findDir(tree, fn, result) {
    if (tree.children && Object.keys(tree.children).length) {
        const _ = Object.values(tree.children).map(t => findDir(t, fn, 0));
        if (fn(tree)) {
            // console.log(tree.name);
        }
        return _.concat(fn(tree) ? tree.size : 0).flat()
    }
    return 0;
}

function solve(lines) {
    const path = [];
    const dirTree = lines.skipWhile(l => l == "$ cd /").reduce((tree, line) => {
        const parts = line.trim().split(' ');
        if (parts[0] == "$" && parts[1] == "cd") {
            if (parts[2] == '..')
                path.pop();
            else
                path.push(parts.last());
        }
        else if(parts[0] == "dir") {
            addNode(getSubtree(tree, path), parts[1], 0)
        }
        else if(!isNaN(parts[0])) {
            addNode(getSubtree(tree, path), parts[1], +parts[0]);
        }
        else if (parts[0] == "$" && parts[1] == "ls") {}
        else {
            throw("Unparsed expression: " + line)
        }
        return tree;
    }, { name: '/', size: 0})

    computeSize(dirTree);
    const p1 = sumIf(dirTree, node => node.size <= 100_000);

    // p2:
    const freeSpace = 70_000_000 - dirTree.size;
    const requiredSpace = 30_000_000 - freeSpace 
    const p2 = Math.min(...findDir(dirTree, node => node.size >= requiredSpace).filter(x => x > 0));
    return { p1, p2 }
}

readAndSolve(process.argv[2] || '07.input', solve, '\n');