import { readAndSolve } from '../aoc.mjs'

function move({ x, y }, [dx, dy]) {
    return { x: x + dx, y: y + dy };
}
function solve(lines) {
    let gridT = { dimensions: { max_x: 6, max_y: 6, min_x: -6, min_y: -6}};
    let H = { x: 0, y: 0 };
    let T = { x: 0, y: 0 };

    gridT.setCell(0, 0, '#');

    for (let line of lines) {
        let [dir, num] = line.split(' ');

        for (let i = 0; i < num; i++) {
            // T is hidden
            if (H.x == T.x && H.y == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 0])
                        break;
                    case 'D':
                        T = move(T, [0, 0])
                        break;
                    case 'R':
                        T = move(T, [0, 0])
                        break;
                    case 'L':
                        T = move(T, [0, 0])
                        break;
                }
            }
            // T is left
            else if (H.x - 1 == T.x && H.y == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 0])
                        break;
                    case 'D':
                        T = move(T, [0, 0])
                        break;
                    case 'R':
                        T = move(T, [1, 0])
                        break;
                    case 'L':
                        T = move(T, [0, 0])
                        break;
                }
            }
            // T is right
            else if (H.x + 1 == T.x && H.y == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 0])
                        break;
                    case 'D':
                        T = move(T, [0, 0])
                        break;
                    case 'R':
                        T = move(T, [0, 0])
                        break;
                    case 'L':
                        T = move(T, [-1, 0])
                        break;
                }
            }
            // T is up
            else if (H.x == T.x && H.y + 1 == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 0])
                        break;
                    case 'D':
                        T = move(T, [0, -1])
                        break;
                    case 'R':
                        T = move(T, [0, 0])
                        break;
                    case 'L':
                        T = move(T, [0, 0])
                        break;
                }
            }
            // T is down
            else if (H.x == T.x && H.y - 1 == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 1])
                        break;
                    case 'D':
                        T = move(T, [0, 0])
                        break;
                    case 'R':
                        T = move(T, [0, 0])
                        break;
                    case 'L':
                        T = move(T, [0, 0])
                        break;
                }
            }
            // T is left down
            else if (H.x - 1 == T.x && H.y - 1 == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [1, 1])
                        break;
                    case 'D':
                        T = move(T, [0, 0])
                        break;
                    case 'R':
                        T = move(T, [1, 1])
                        break;
                    case 'L':
                        T = move(T, [0, 0])
                        break;
                }
            }
            // T is right down
            else if (H.x + 1 == T.x && H.y - 1 == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [-1, 1])
                        break;
                    case 'D':
                        T = move(T, [0, 0])
                        break;
                    case 'R':
                        T = move(T, [0, 0])
                        break;
                    case 'L':
                        T = move(T, [-1, 1])
                        break;
                }
            }
            // T is left up
            else if (H.x - 1 == T.x && H.y + 1 == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 0])
                        break;
                    case 'D':
                        T = move(T, [1, -1])
                        break;
                    case 'R':
                        T = move(T, [1, -1])
                        break;
                    case 'L':
                        T = move(T, [0, 0])
                        break;
                }
            }
            // T is right up
            else if (H.x + 1 == T.x && H.y + 1 == T.y) {
                switch (dir) {
                    case 'U':
                        T = move(T, [0, 0])
                        break;
                    case 'D':
                        T = move(T, [-1, -1])
                        break;
                    case 'R':
                        T = move(T, [0, 0])
                        break;
                    case 'L':
                        T = move(T, [-1, -1])
                        break;
                }
            }
            switch (dir) {
                case 'U':
                    H = move(H, [0, 1])
                    break;
                case 'D':
                    H = move(H, [0, -1])
                    break;
                case 'R':
                    H = move(H, [1, 0])
                    break;
                case 'L':
                    H = move(H, [-1, 0])
                    break;
            }
            // gridT.setCell(H.x, -H.y, 'H')
            gridT.setCell(T.x, -T.y, '#')
            // console.log();
            // console.log(gridT.showGrid('.'))
        }
    }

    // console.log(gridT)
    return Object.values(gridT).filter(x => x === '#').length;

}


readAndSolve(process.argv[2] || '09.input', solve, '\n');