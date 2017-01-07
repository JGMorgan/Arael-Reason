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

    /* number of dimentions for every point*/
    let num_dimensions = Array.length(Array.get x 0);
    let clusters = Array.make_matrix k 0 (Array.make_float 0);
    let centroids = Array.make_matrix k num_dimensions 0.;
    let (mins, maxs) = dimensional_min_max(x);
    clusters;
};
