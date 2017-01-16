/* TODO figure out error with module system*/

/* Euler's number to 31 decimal places*/
let e = 2.7182818284590452353602874713527;

/*
 * Dot product of two matrices
 * (array (array float), array (array float)) => array (array float)
 */
let dot = fun (x, y) => {
    let rec rowxcol = fun (x, y, i, j, k) => {
        switch (k) {
            | 0 => (Array.get(Array.get x i) 0) *. (Array.get(Array.get y 0) j);
            | n => (Array.get(Array.get x i) n) *. (Array.get(Array.get y n) j)
                +. rowxcol(x, y, i, j, n-1);
        };
    };
    let height = Array.length x;
    let length = Array.length(Array.get y 0);
    let m = Array.make_matrix height length 0.;
    for i in (0) to (height - 1) {
        for j in (0) to (length - 1) {
            Array.set(Array.get m i) j (rowxcol (x, y, i, j, (Array.length y) - 1));
        };
    };
    m;
};

/*
 * Transpose a matrix
 * array (array 'a) => array (array 'a)
 */
let transpose = fun (x) => {
    let height = Array.length x;
    let length = Array.length(Array.get x 0);
    let m = Array.make_matrix length height (Array.get (Array.get x 0) 0);
    for i in (0) to (length - 1) {
        for j in (0) to (height - 1) {
            Array.set(Array.get m i) j (Array.get(Array.get x j) i);
        };
    };
    m;
};

/*
 * Euclidean distance between two n dimensional points.
 * (list float, list float) => float
 */
let distance = fun (x, y) => {
    let rec sumsquared = fun (x, y) => {
        switch (x, y) {
            | ([], []) => 0.;
            | ([headx, ...tailx], [heady, ...taily]) =>
                (headx -. heady) *. (headx -. heady) +. sumsquared(tailx, taily);
            | _ => failwith "Input lists should be of equal length"; /* TODO error */
        };
    };
    let sqsum = sumsquared(x, y);
    sqrt(sqsum);
};

/*
 * derivative of tanh
 * tanh is a built in function
 * float => float
 */
let tanhPrime = fun (x) => {
    let tanhx = tanh(x);
    1. -. (tanhx *. tanhx);
};

/*TODO how on earth does exponentiation work in reason :(*/
let sigmoid = fun (x) => {
    /*1. /. (1. +. (e ** -x));*/
};
