import { readAndSolve, range } from '../aoc.mjs'

class Display {
    constructor(width, height, pixel = '█') {
        this.pixel = pixel;
        this.cycle = 0; // even though it starts at 1, we draw in position 0
        this.x = 1;
        this.display = {
            dimensions: {
                min_x: 0, min_y: 0,
                max_x: width, max_y: height
            }
        };
        this.draw();
    }

    noop() {
        this.cycle += 1;
        this.draw();
    }

    add(number) {
        this.cycle += 1;
        this.draw();

        this.cycle += 1;
        this.x += number;
        this.draw();
    }

    draw() {
        if (Math.abs((this.cycle % 40) - this.x) <= 1) {
            this.display.setCell(this.cycle % 40, Math.floor(this.cycle / 40), this.pixel)
        }
    }

    show() {
        return this.display.showGrid(' ');
    }
}

const solve = (lines) => {
    const instructions = lines.map(line => line.split(' '));
    const display = new Display(39, 5);

    for (let instruction of instructions) {
        if (instruction[0] == 'noop') {
            display.noop();
        }
        else if (instruction[0] == 'addx') {
            display.add(+instruction[1]);
        }
    }
    return display.show();
}

readAndSolve(process.argv[2] || '10.input', solve, '\n');