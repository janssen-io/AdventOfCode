const combinations = [];
for(let i = 0; i < 3 ** 3; i++) {
    combinations.push(i.toString(3).padStart(3, 0).split('').map(trit => +trit + 1)); //1, 2, 3
}

const memo = {};

function countWinners(p1, p2, s1, s2, comb)  {
    let p = (p1 + comb[0] + comb[1] + comb[2]) % 10;
    let s = s1 + (p || 10);

    if (s >= 21) {
        return [1, 0];
    }

    let key = `${p}/${s}-${p2}/${s2}`;
    let memoized = memo[key];
    if (memoized != undefined) {
        return memoized;
    }

    let result = [0,0]
    for(let comb of combinations) {
        let winnerCount = countWinners(p2, p, s2, s, comb);
        result[0] += winnerCount[1];
        result[1] += winnerCount[0];
    }

    memo[key] = result;
    return result;
}

function solve() {
    let result = [0,0]
    for(let comb of combinations) {
        let winnerCount = countWinners(1, 2, 0, 0, comb);
        result[0] += winnerCount[1];
        result[1] += winnerCount[0];
    }

    console.log(Math.max(...result));
    return { result };
}

solve();