/*
 * runs linear regression prediction
 * (array float, array float) => float
 */
let predict = fun (x, y) => {
    let new_x = Array.append x (Array.make 1 1.);
    let rec regression_sum = fun (x, y) => {
        switch (x, y) {
            | ([], []) => 0.;
            | ([headx, ...tailx], [heady, ...taily]) =>
                (headx *. heady) +. regression_sum(tailx, taily);
            | _ => failwith "Input lists should be of equal length";
        };
    };
    regression_sum(Array.to_list new_x, Array.to_list y);
};

let train = fun (x, y, alpha) => {

};
