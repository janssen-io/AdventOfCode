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

function walkTree(tree, select, combine, mzero) {
    if(!tree.children || !Object.keys(tree.children).length) return mzero;

    const subresult = Object
        .values(tree.children)
        .map(t => walkTree(t, select, combine, mzero));

    return combine(subresult, select(tree));
}

function sumIf(tree, requirement) {
    return walkTree(
        tree,
        node => requirement(node) ? node.size : 0,
        (sub, current) => sub.sum() + current,
        0);
}

function getDirectorySizes(tree, requirement) {
    return walkTree(
        tree,
        node => requirement(node) ? node.size : [],
        (sub, current) => sub.concat(current).flat(),
        []);
}

function solve(lines) {
    // prepare tree
    const path = [];
    const dirTree = lines
        .slice(1)
        .reduce((tree, line) => {
            const parts = line.trim().split(' ');
            if (parts[0] == "$") {
                if (parts[1] == "cd") {
                if (parts[2] == '..') path.pop();
                    else path.push(parts.last());
                }
                else if (parts[1] == "ls") {} // nothing to do
            }
            else if(parts[0] == "dir") {
                addNode(getSubtree(tree, path), parts[1], 0)
            }
            else if(!isNaN(parts[0])) {
                addNode(getSubtree(tree, path), parts[1], +parts[0]);
            }
            else {
                throw("Unparsed expression: " + line)
            }
            return tree;
        }, { name: '/', size: 0});

    computeSize(dirTree);

    // p1
    const p1 = sumIf(dirTree, node => node.size <= 100_000);

    // p2:
    const freeSpace = 70_000_000 - dirTree.size;
    const requiredSpace = 30_000_000 - freeSpace 
    const p2 = Math.min(...getDirectorySizes(dirTree, node => node.size >= requiredSpace));
    return { p1, p2 }
}

readAndSolve(process.argv[2] || '07.input', solve, '\n');