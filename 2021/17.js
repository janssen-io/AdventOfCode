function step(velocity, position) {
    let newPosition = {
        x: position.x + velocity.x,
        y: position.y + velocity.y
    }

    const delta_x = (velocity.x > 0) * -1 + (velocity.x < 0) * 1;

    let newVelocity = {
        x: velocity.x + delta_x,
        y: velocity.y - 1,
    }

    return { newVelocity, newPosition };
}

const between = (x, a, b) => x >= a && x <= b;

function simulate(position, velocity) {
    const positions = [];
    while(position.x <= input.x_max && position.y >= input.y_min) {
        if (between(position.x, input.x_min, input.x_max)
            && between(position.y, input.y_min, input.y_max))
        {
            return positions.reduce((highest, pos) => pos.y > highest ? pos.y : highest, Number.MIN_VALUE);
        }

        let {newVelocity, newPosition } = step(velocity, position);
        velocity = newVelocity;
        position = newPosition;
        positions.push(position);
    }
}

let input = { x_min :  70, x_max:   96, y_min: -179, y_max: -124 };
// input = { x_min :  20, x_max:   30, y_min:  -10, y_max:   -5 };

var position = {x: 0, y: 0} ;
let results = []
for(let x = 0; x <= input.x_max; x++) {
    // if(x % 10 == 0) console.log({x});
    for(let y = input.y_min; y < 1000; y++) {
        results.push(simulate(position, {x, y}));
    }
}
console.log('Highest', results.sort((a,b) => b-a)[0]);
console.log('All pos', results.filter(x => x).length);
