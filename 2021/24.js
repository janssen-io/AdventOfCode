require('./aoc');

var z0 = 0;

// important numbers from the input
// z is the n in: div z n
// x is the n in: add x n (before eql x w)
// y is the n in: add y n (after add y w)
let formulas = [
    { z:  1, x:  12, y:  7, }, // 0
    { z:  1, x:  13, y:  8, }, // 1
    { z:  1, x:  13, y: 10, }, // 2
    { z: 26, x:  -2, y:  4, }, // 3
    { z: 26, x: -10, y:  4, }, // 4
    { z:  1, x:  13, y:  6, }, // 5
    { z: 26, x: -14, y: 11, }, // 6
    { z: 26, x:  -5, y: 13, }, // 7
    { z:  1, x:  15, y:  1, }, // 8
    { z:  1, x:  15, y:  8, }, // 9
    { z: 26, x: -14, y:  4, }, // 10
    { z:  1, x:  10, y: 13, }, // 11
    { z: 26, x: -14, y:  4, }, // 12
    { z: 26, x:  -5, y: 14, }, // 13
]

// The algorithm from the input
function apply(data, z, depth = 1, crumbs = []) {
    if (depth == data.length) {
        // console.log({z, depth, crumbs});
        return { z, crumbs };
    }
    let expected = (z % 26) + data[depth].x;
    z = Math.floor(z / data[depth].z);
    // Check if we can reduce the number so we end up with 0 at the end.
    if (expected > 0 && expected < 10) {
        let r = apply(data, z, depth + 1, [...crumbs, expected]);
        return r;
    }
    if (data[depth].x < 10) {
        return;
        // we should be able to reduce the number, but couldn't.
        // Our serial number must be smaller.
    }

    // Impossible to reduce the number as the equation (x + n = w) is not solvable for n > 9
    // so we must find the smallest number for which z is still 0 in the end
    for(let w = 1; w < 10; w++) {
        let z_new = 26 * z + data[depth].y + w;
        let result = apply(data, z_new, depth + 1, [...crumbs, w]);

        if (result && result.z == 0) {
            return result;
        }
    }
}

console.log('Serial number: '.green(), apply(formulas, z0, 0, []).crumbs.join(''))
