import { readAndSolve, range } from '../aoc.mjs'

function solveFor(lines, ropeLength) {
    const gridT = {};
    const rope = range(0, ropeLength).map(_ => { return { x: 0, y: 0 } });

    gridT.setCell(0, 0, '#');

    for (let line of lines) {
        const [dir, num] = line.split(' ');

        // break down the move (e.g., R 5) into one-step pieces
        for (let remainingMoves = num; remainingMoves > 0; remainingMoves--) {
            // move the head first
            switch (dir) {
                case 'U': rope[0].y++; break;
                case 'D': rope[0].y--; break;
                case 'R': rope[0].x++; break;
                case 'L': rope[0].x--; break;
            }

            // move the tail piece by piece
            for (let headIndex = 0; headIndex < rope.length - 1; headIndex++) {
                const H = rope[headIndex];
                const T = rope[headIndex + 1];
                if (Math.abs(H.y - T.y) > 1 || Math.abs(H.x - T.x) > 1) {
                    if (H.x > T.x) { T.x++; } // T is left
                    if (H.x < T.x) { T.x--; } // T is right
                    if (H.y < T.y) { T.y--; } // T is up
                    if (H.y > T.y) { T.y++; } // T is down
                }
            }
            gridT.setCell(rope.last().x, -rope.last().y, '#')
        }
    }
    return Object.values(gridT).filter(x => x === '#').length;
}

const solve = (lines) => {
    return { p1: solveFor(lines, 2), p2: solveFor(lines, 10) }
}

readAndSolve(process.argv[2] || '09.input', solve, '\n');