import { readAndSolve, range } from '../aoc.mjs'

function move({ x, y }, [dx, dy]) {
    return { x: x + dx, y: y + dy };
}

function solve(lines) {
    let gridT = { dimensions: { max_x: 6, max_y: 6, min_x: -6, min_y: -6}};
    let Head = { x: 0, y: 0 };
    let p1 = [{x: 0, y: 0}]
    let p2 = range(0, 9).map(_ => { return { x: 0, y: 0 } });

    gridT.setCell(0, 0, '#');
    let snake = [Head, ...p2];

    for (let line of lines) {
        let [dir, num] = line.split(' ');

        for (let remainingMoves = num; remainingMoves > 0; remainingMoves--) {
            switch (dir) {
                case 'U':
                    snake[0] = move(snake[0], [0, 1])
                    break;
                case 'D':
                    snake[0] = move(snake[0], [0, -1])
                    break;
                case 'R':
                    snake[0] = move(snake[0], [1, 0])
                    break;
                case 'L':
                    snake[0] = move(snake[0], [-1, 0])
                    break;
            }
            for(let headIndex = 0; headIndex < snake.length - 1; headIndex++) {
                const H = snake[headIndex]
                let T = snake[headIndex+1]
                if (Math.abs(H.y - T.y) > 1 || Math.abs(H.x - T.x) > 1) {
                    // T is left
                    if (H.x > T.x) { T.x += 1 }
                    // T is right
                    if (H.x < T.x) { T.x -= 1 }
                    // T is up
                    if (H.y < T.y) { T.y -= 1 }
                    // T is down
                    if (H.y > T.y) { T.y += 1 }
                    snake[headIndex + 1] = T;
                }
            }
            gridT.setCell(snake.last().x, -snake.last().y, '#')
        }
    }
    return Object.values(gridT).filter(x => x === '#').length;
}


readAndSolve(process.argv[2] || '09.input', solve, '\n');