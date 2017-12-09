let explode = (s) => {
  let rec _explode = (i, acc) => {
    let acc = [String.sub(s, i, 1), ...acc];
    switch i {
    | 0 => acc
    | _ => _explode(i - 1, acc)
    }
  };
  switch (String.length(s)) {
  | 0 => []
  | n => _explode(n - 1, [])
  }
};

let filterWithIndexAndListAndLength = (f, l) =>
  List.mapi((i, a) => f(i, a, l, List.length(l)) ? Some(a) : None, l)
  |> List.filter(
       (a) =>
         switch a {
         | Some(_) => true
         | None => false
         }
     )
  |> List.map(
       (a) =>
         switch a {
         | Some(a) => a
         | None => assert false
         }
     );

/*
   [1, 1, 2, 2] => [1, 2] => 3
   [91212129] => [9] => 9
   Filter list to elements where i + 1 is the same as i. When i = length - 1, compare to 0.
 */
let solution = (input) =>
  List.map(int_of_string, explode(input))
  |> filterWithIndexAndListAndLength(
       (i, n, ns, length) => i < length - 1 ? n === List.nth(ns, i + 1) : n === List.nth(ns, 0)
     )
  |> List.fold_left((acc, n) => acc + n, 0)
  |> string_of_int;