
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
 * calculates the determinant of a matrix
 * array (array float) => float
 */
let rec determinant = fun (x) => {
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
 * Deep compare for two arrays
 * (array 'a, array 'a) => bool
 */
let array_equals = fun (x, y) => {
    /*
     * this does the element wise comparison
     * (list a', list a') => bool
     */
    let rec array_equals_help = fun (x, y) => {
        switch (x, y) {
            | ([], []) => true;
            | ([headx, ...tailx], [heady, ...taily]) => (headx == heady) && array_equals_help(tailx, taily);
            | _ => false;
        };
    };
    array_equals_help(Array.to_list x, Array.to_list y);
};

/*
 * runs linear regression prediction
 * (array float, array float) => float
 */
let predict = fun (x, theta) => {
    let new_x = Array.append x (Array.make 1 1.);
    let rec regression_sum = fun (x, theta) => {
        switch (x, theta) {
            | ([], []) => 0.;
            | ([headx, ...tailx], [heady, ...taily]) =>
                (headx *. heady) +. regression_sum(tailx, taily);
            | _ => failwith "Input lists should be of equal length";
        };
    };
    regression_sum(Array.to_list new_x, Array.to_list theta);
};

/*
 * trains using x (input matrix) and y (expected vals) and returns the theta vector
 * if try_closed_formula is passed in as true it will attempt to train using ((X^T * X)^-1) * X^T * Y
 *  if the determinant of dot(transpose x, x) == 0 then it will use the iterative method
 *  only set try_closed_formula if you are fairly certain it is possible since determinant is a costly operation
 * if use_bgd is passed in as true we will use batch gradient descent otherwise we will use 
 *  stochastic gradient descent. Batch gradient descent is slower but more accurate and stochastic 
 *  is the opposite, faster but less accurate. Keep in mind stochastic gradient descent is typically
 *  accurate enough for most use cases. 
 */
let train = fun (x, y, alpha, try_closed_formula, use_bgd) => {

    /*
     * runs batch gradient descent on the training set
     * returns theta vector
     * (array (array float), array float, float) => array float
     */
    let batch_gradient_descent = fun (x, y, alpha) => {
        let theta = Array.make (Array.length (Array.get x 0)) 0.;
        let prev_theta = Array.make (Array.length theta) 1.;
        while (not(array_equals(theta, prev_theta))) {
            /* copy theta into prev_theta
             * and takes a training step
             */
            for i in (0) to ((Array.length theta) - 1) {
                Array.set prev_theta i (Array.get theta i);
                let errors = Array.mapi 
                    (fun j el_x => {((Array.get y j) -. predict(el_x, theta)) *. (Array.get el_x i)}) x;
                let total_err = Array.fold_left
                    (fun a b => {a +. b}) 0. errors;
                Array.set theta i ((Array.get theta i) +. (alpha *. total_err)); 
            }; 
        };
        theta;
    };
    
    /*
     * runs stochastic gradient descent on the training set
     * returns theta vector
     * (array (array float), array float, float) => array float
     */
    let stochastic_gradient_descent = fun (x, y, alpha) => {

        /*
         * Basically python's xrange
         */
        let xrange n => Stream.from 
            (fun i => {
                switch (i > n) {
                    | true => None;
                    | false => Some i;
                };
            });
        let theta = Array.make (Array.length (Array.get x 0)) 0.;
        let prev_theta = Array.make (Array.length theta) 1.;
        let j_iter = xrange max_int;
        while (not(array_equals(theta, prev_theta))) {
            for i in (0) to ((Array.length theta) - 1) {
                Array.set prev_theta i (Array.get theta i);
                let j = Stream.next j_iter;
                let error = ((Array.get y j) -. predict(Array.get x j, theta)) *. Array.get (Array.get x j) i;
                Array.set theta i ((Array.get theta i) +. (alpha *. error));
            };
        };
        theta;
    };

    /*1 is interted into into x because 1 * some multiplier 
    will be the constant in our regression function*/
    let new_x = Array.append x (Array.make 1 1.);
    let xT = transpose(x);
    switch (try_closed_formula) {
        | true => 
            switch (determinant(dot(xT, new_x))) {
                | 0. => /*iterative*/
                    switch (use_bgd) {
                        | true => batch_gradient_descent(x, y, alpha);
                        | false => stochastic_gradient_descent(x, y, alpha);
                    };
                | _  => batch_gradient_descent(x, y, alpha);
            };
        | false => /*iterative*/
            switch (use_bgd) {
                | true => batch_gradient_descent(x, y, alpha);
                | false => stochastic_gradient_descent(x, y, alpha);
            };
    };
       
};
