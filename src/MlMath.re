let rec transpose = fun (x) => {
    switch (x) {
        | [] => [];
        | [[], ...tail] => transpose(tail);
        | [[head, ...tail], ...tail2d] =>
            [[head, ...List.rev(List.rev_map(List.hd) (tail2d))],
            ...transpose([tail, ...List.rev(List.rev_map(List.tl)(tail2d))])];
    };
};

let distance = fun (x, y) => {
    let rec sumsquared = fun (x, y) => {
        switch (x, y) {
            | ([], []) => 0;
            | ([headx, ...tailx], [heady, ...taily]) =>
                (headx - heady) * (headx - heady) + sumsquared(tailx, taily);
            | _ => 0;
        };
    };
    let sqsum = float_of_int(sumsquared(x, y));
    sqrt(sqsum);
};

print_float(distance([1,2,3], [3,2,1]))
