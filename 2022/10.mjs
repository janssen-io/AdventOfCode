import { readAndSolve, range } from '../aoc.mjs'

class Display {
    constructor(width, height, debug, pixel = '█') {
        this.debug = (typeof(debug) === 'function') ? debug : () => {};
        this.pixel = pixel;
        this.cycle = 0; // even though it starts at 1, we draw in position 0
        this.x = 1;
        this.display = {
            dimensions: {
                min_x: 0, min_y: 0,
                max_x: width, max_y: height
            }
        };
        this._draw();
    }

    noop() {
        this._tick();
        this._draw();
    }

    add(number) {
        this._tick();
        this._draw();

        this._tick();
        this.x += number;
        this._draw();
    }

    _tick() {
        this.cycle++;
        this.debug(this.cycle, this.x);
    }

    _draw() {
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

    const readCycles = [20,60,100,140,180,220];
    const readValues = [];

    function debug(clock, value) {
        if (readCycles.includes(clock)) {
            readValues.push(value * clock);
        }
    }

    const display = new Display(39, 5, debug);

    for (let instruction of instructions) {
        if (instruction[0] == 'noop') {
            display.noop();
        }
        else if (instruction[0] == 'addx') {
            display.add(+instruction[1]);
        }
    }
    
    let result = `${'p1: '.red()}${readValues.sum()}\r\n${'p2: '.red()}\r\n`;
    result += display.show();
    return result;
}

readAndSolve(process.argv[2] || '10.input', solve, '\n');