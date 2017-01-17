/*
 * Euclidean distance between two n dimensional points.
 * (array float, array float) => float
 */
let distance = fun (x, y) => {
    let rec sumsquared = fun (x, y) => {
        switch (x, y) {
            | ([], []) => 0.;
            | ([headx, ...tailx], [heady, ...taily]) =>
                (headx -. heady) *. (headx -. heady) +. sumsquared(tailx, taily);
            | _ => 0.;
        };
    };
    let sqsum = sumsquared(Array.to_list x, Array.to_list y);
    sqrt(sqsum);
};

/*
 * Takes in an Array of n dimensiona data and clusters it into k clusters.
 * (array (array float), int) => array (array (array float))
 */
let cluster = fun (x, k) => {

    /*
     * Finds the smallest and largest value per dimension
     * array (array 'a) => (array 'a, array 'a)
     */
    let dimensional_min_max = fun (x) => {
        let dimensional_mins = Array.copy(Array.get x 0);
        let dimensional_maxs = Array.copy(Array.get x 0);
        for i in (1) to ((Array.length x) - 1) {
            for j in (0) to ((Array.length dimensional_mins) - 1) {
                Array.set dimensional_mins j
                    (min (Array.get dimensional_mins j) (Array.get (Array.get x i) j));
                Array.set dimensional_maxs j
                    (max (Array.get dimensional_maxs j) (Array.get (Array.get x i) j));
            };
        };
        (dimensional_mins, dimensional_maxs);
    };

    /*
     * this creates centroids randomly near the data
     * (array (array float), int, int) => array (array float)
     */
    let init_centroids = fun (x, k, num_dimensions) => {
        let centroids = Array.make_matrix k num_dimensions 0.;
        let (mins, maxs) = dimensional_min_max(x);
        for i in (0) to (k - 1) {
            for j in (0) to (num_dimensions - 1) {
                Array.set (Array.get centroids i) j
                    ((Random.float (Array.get maxs j)) +. (Array.get mins j));
            };
        };
        centroids;
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
     * finds the minimum value and minimum index of a list
     * (list 'a, 'a, int, int) => int
     */
    let rec find_min_index = fun (x, min, pos, i) => {
        switch (x) {
            | [] => pos;
            | [elem] =>
                switch (elem > min) {
                    | true => pos;
                    | false => i;
                };
            | [head, ...tail] =>
                switch (head > min) {
                    | true => find_min_index (tail, min, pos, i + 1);
                    | false => find_min_index (tail, head, i, i + 1);
                };
        };
    };

    /*
     * finds the average value of a certain dimension
     * (array (array (array float)), int, int) => array float
     */
    let rec dimensional_average = fun (clusters, i, num_dimensions) => {
        /*
         * finds the sum of a certain dimension j in a cluster
         * (list (array float), int) => float
         */
        let rec dimensional_average_sum = fun (cluster, j) => {
            switch (cluster) {
                | [] => 0.;
                | [head, ...tail] =>
                    (Array.get head j) +. dimensional_average_sum(tail, j);
            };
        };

        let cluster = Array.to_list (Array.get clusters i);
        let averages = Array.make_float num_dimensions;
        let cluster_size = Array.length (Array.get clusters i);
        Array.mapi
            (fun j x => {dimensional_average_sum(cluster, j) /. float_of_int(cluster_size)})
            averages;
    };

    /* number of dimentions for every point*/
    let num_dimensions = Array.length(Array.get x 0);
    /* clusters is initialized to an empty float 3d array*/
    let clusters = Array.make_matrix k 0 (Array.make_float 0);
    let centroids = init_centroids(x, k, num_dimensions);
    let prev_centroids = Array.make_matrix k num_dimensions 0.;

    while (not(matrix_equals(centroids, prev_centroids))) {
        /* prev_centroids = centroids
         * clear clusters
         */
        for i in (0) to (k - 1) {
            Array.set prev_centroids i (Array.copy (Array.get centroids i));
            Array.set clusters i (Array.make_matrix 0 0 0.);
        };

        /* clustering*/
        let height = Array.length x;
        for i in (0) to (height - 1) {
            let distances = Array.map (fun a => distance(Array.get x i, a)) centroids;
            let min_ind = find_min_index(Array.to_list distances, max_float, 0, 0);
            Array.set clusters min_ind
                (Array.append (Array.get clusters min_ind) (Array.make 1 (Array.get x i)));
        };

        /* adjust centroids*/
        for i in (0) to (k - 1) {
            Array.set centroids i (dimensional_average(clusters, i, num_dimensions));
        };
    };
    clusters;
};
