import { readAndSolve, printAnswer, test, assert } from '../aoc.mjs'

function solveFor(lines) {
    // Lesson from yesterday: don't use global variables.
    // If we need them today, let's try to only scope everything
    // to the `solveFor` method. That is, use local functions
    // or pass all dependencies explicitly.
    // This ensures that every call to `solveFor` uses 'fresh'
    // variables.

    const digits = {
        '2': 2,
        '1': 1,
        '0': 0,
        '-': -1,
        '=': -2,
    }
    const snafus = {
        '2': '2',
        '1': '1',
        '0': '0',
        '-1': '-',
        '-2': '=',
    }

    const inverseSnafu = {
        '2': '=',
        '1': '-',
        '0': '0',
        '-': '1',
        '=': '2',
    }

    function snafuToDec(line) {
        const parts = line.split('').reverse(); // LSB first
        let value = 0;
        for (let i = 0; i < parts.length; i++) {
            const digit = digits[parts[i]];
            value = value + digit * Math.pow(5, i);
            // console.log({digit, value})
        }
        return value;
    }

    const negate = (snafu) => snafu.split('').map(s => inverseSnafu[s]).join('');

    /*
    13
    -25 (1) = -12

    12
    -10 (2) = 2

    2 (2)

    2==

    

    */
    function decToSnafu(dec, depth = 0) {
        let remainder = dec;
        let sum = 0;
        let answer = ''
        if (depth > 10) throw "stopped";
        let e = 30;
        for (; e >= 0; e--) {
            const maxWithoutThisDigit = Math.floor(Math.pow(5, e) / 2)

            if (remainder == 0) {
                answer += '0'; // just finish the loop adding zeroes. 
            }
            else if (remainder < 0) {
                const lastQuints = negate(decToSnafu(remainder * - 1, depth + 1));

                // Make sure to actually fill in the trimmed 0s between the current answer and the last quints.
                return answer.replace(/^0+/, '') + '0'.repeat(e - lastQuints.length + 1) + lastQuints;
            }
            else if (remainder <= maxWithoutThisDigit) {
                answer += '0'
            }
            else if (Math.abs(remainder - Math.pow(5, e)) <= maxWithoutThisDigit) {
                answer += '1'
                const value = 1 * Math.pow(5, e);
                sum += value;
                remainder -= value;
            }
            else {
                answer += '2'
                const value = 2 * Math.pow(5, e);
                sum += value;
                remainder -= value;
            }
        }

        // Trim quints that literally don't add value.
        const snafuString = answer.replace(/^0+/, '');
        return snafuString;
    }

    const p1_10 = lines.map(snafuToDec).sum();
    // for (let i = 0; i < 10_000; i++) {
    //     const p1_10 = i;
        const p1 = decToSnafu(p1_10);
        console.log(p1, snafuToDec(p1), p1_10);
        assert(() => snafuToDec(p1) == p1_10, 'Invalid conversion')
    // }
    return p1;
}

const solve = (lines) => {
    return {
        p1: solveFor(lines)
    }
}

(async () => {
    const day = 25;
    let example = await readAndSolve(`${day}.example.input`, solve, '\n');
    test('Example p1', undefined, example.answer.p1)

    const puzzle = await readAndSolve(`${day}.input`, solve, '\n');
    printAnswer(puzzle);
})();
