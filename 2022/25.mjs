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
        }
        return value;
    }

    const negate = (snafu) => snafu.split('').map(s => inverseSnafu[s]).join('');

    function decToSnafu(dec) {
        let remainder = dec;
        let answer = ''
        for (let e = 30; e >= 0; e--) {
            const maxWithoutThisDigit = Math.floor(Math.pow(5, e) / 2)

            // fill in the remaining 0s
            // e + 1, because there is also a digit for e = 0
            if (remainder == 0) {
                answer = answer + '0'.repeat(e + 1);
                break;
            }

            if (remainder < 0) {
                const lastQuints = negate(decToSnafu(remainder * - 1));

                // Make sure to actually fill in the trimmed 0s between the current answer and the last quints.
                answer = answer + '0'.repeat(e - lastQuints.length + 1) + lastQuints;
                break;
            }

            // Check which smallest value can reduce the remainder, so that
            // the remaining digits can still sum up to the result.
            for(let i = 0; i <= 2; i++) {
                if (Math.abs(remainder - i * Math.pow(5, e)) <= maxWithoutThisDigit) {
                    answer += i
                    const value = i * Math.pow(5, e);
                    remainder -= value;
                    break;
                }
            }
        }

        // Trim leading zeroes otherwise they mess up the recursive result.
        const snafuString = answer.replace(/^0+/, '');
        return snafuString;
    }

    const p1_10 = lines.map(snafuToDec).sum();
    const p1 = decToSnafu(p1_10);
    console.log(p1, snafuToDec(p1), p1_10);
    assert(() => snafuToDec(p1) == p1_10, 'Invalid conversion')
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
