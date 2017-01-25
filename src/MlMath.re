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
 * returns a new matrix without a certain row and column
 * (array (array float), int, int) => array (array float)
 */
let remove_row_and_col = fun (x, row, col) => {
    /* rows == cols since this is for a square matrix*/
    let rows_and_cols = Array.length x;
    let new_matrix = Array.make_matrix (rows_and_cols - 1) (rows_and_cols - 1) 0.;

    /*
     * iterates through a matrix and filters out a row and column
     * (int, int, int, int) => array (array float)
     */
    let rec filter_row_col = fun (i, j, new_i, new_j) => {
        switch (i == row, j == col) {
            | (false, false) =>
                {
                    Array.set (Array.get new_matrix new_i) new_j
                        (Array.get (Array.get x i) j);
                    switch (i == rows_and_cols - 1, j == rows_and_cols - 1) {
                        | (true, true) => new_matrix;
                        | (_, true) => filter_row_col(i + 1, 0, new_i + 1, 0);
                        | _ => filter_row_col(i, j + 1, new_i, new_j + 1);
                    };
                };
            | (true, true) =>
                switch (i == rows_and_cols - 1, j == rows_and_cols - 1) {
                    | (true, true) => new_matrix;
                    | (_, true) => filter_row_col(i + 1, 0, new_i, 0);
                    | _ => filter_row_col(i, j + 1, new_i, new_j);
                };
            | (true, false) =>
                switch (i == rows_and_cols - 1, j == rows_and_cols - 1) {
                    | (true, true) => new_matrix;
                    | (_, true) => filter_row_col(i + 1, 0, new_i, 0);
                    | _ => filter_row_col(i, j + 1, new_i, new_j + 1);
                };
            | (false, true) =>
                switch (i == rows_and_cols - 1, j == rows_and_cols - 1) {
                    | (true, true) => new_matrix;
                    | (_, true) => filter_row_col(i + 1, 0, new_i, 0);
                    | _ => filter_row_col(i, j + 1, new_i, new_j);
                };
        };

    };
    filter_row_col(0, 0, 0, 0);
};

/*
 * calculates the determinant of a matrix
 * array (array float) => float
 */
let rec determinant = fun (x) => {
        
    switch (Array.length x, Array.length (Array.get x 0)) {
        | (1, 1) => (Array.get (Array.get x 0) 0);
        | (2, 2) => ((Array.get (Array.get x 0) 0) *. (Array.get (Array.get x 1) 1))
            -. ((Array.get (Array.get x 1) 0) *. (Array.get (Array.get x 0) 1));
        | (n, m) =>
            switch (n == m) {
                | false => failwith "Determinant is only defined for a square matrix."
                | true =>
                    {
                        let determinants = Array.mapi
                            (fun i a => {
                                switch (i mod 2 == 0) {
                                    | true => a *. determinant(remove_row_and_col(x, 0, i));
                                    | false => -. a *. determinant(remove_row_and_col(x, 0, i));
                                };
                            }) (Array.get x 0);
                        Array.fold_left (fun a b => {a +. b;}) 0. determinants;
                    };
            };
    };
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
 * returns the inverse of a matrix
 * array (array float) => array (array float)
 */
let inverse = fun (x) => {

    /*
     * returns the matrix of minors
     * array (array float) => array (array float)
     */
    let minors = fun (x) => {
        Array.mapi (fun i el => {
            Array.mapi (fun j el_2 => {
                determinant(remove_row_and_col(x, i, j));
            }) el;
        }) x;
    };
    
    /* 
     * returns the cofactor matrix
     * array (array float) => array (array float)
     */
    let cofactors = fun (x) => {
        Array.mapi (fun i el => {
            Array.mapi (fun j el_2 => {
                switch (((i mod 2 == 0) && (j mod 2 != 0)) || ((i mod 2 != 0) && (j mod 2 == 0))) {
                    | true => el_2 *. -1.;
                    | false => el_2;
                };
            }) el;
        }) x;
    };

    let d = determinant(x);
    let new_x = transpose(cofactors(minors(x)));

    Array.map (fun el => {
        Array.map (fun el_2 => {
            el_2 /. d;
        }) el;
    }) new_x;
    
};

/*
 * Deep compare for two matrices
 * (array (array 'a), array (array 'a)) => bool
 */
let matrix_equals = fun (x, y) => {
    /*
     * Assuming the length and height of the matrices are equal,
     * this does the element wise comparison
     * (array (array 'a), array (array 'a), int, int, int, int) => bool
     */
    let rec matrix_equals_help = fun (x, y, length, height, i, j) => {
        let elem_x = Array.get (Array.get x i) j;
        let elem_y = Array.get (Array.get y i) j;
        switch (elem_x == elem_y) {
            | true =>
                switch (height == i + 1, length == j + 1) {
                    | (true, true) => true;
                    | (false, true) => matrix_equals_help(x, y, length, height, i + 1, 0);
                    | _ => matrix_equals_help(x, y, length, height, i, j + 1);
                };
            | false => false;
        };
    };

    let heightx = Array.length x;
    let lengthx = Array.length(Array.get x 0);
    let heighty = Array.length y;
    let lengthy = Array.length(Array.get y 0);
    switch (heightx == heighty, lengthx == lengthy) {
        | (true, true) => matrix_equals_help(x, y, lengthx, heightx, 0, 0);
        | _ => false;
    };
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
            | _ => failwith "Input lists should be of equal length"; 
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
