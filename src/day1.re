open StringUtils;

let boolOfOptional = (a) =>
  switch a {
  | Some(_) => true
  | None => false
  };

let unwrap = (a) =>
  switch a {
  | Some(a) => a
  | None => assert false
  };

let filterWithIndexAndListAndLength = (f, l) =>
  List.mapi((i, a) => f(i, a, l, List.length(l)) ? Some(a) : None, l)
  |> List.filter(boolOfOptional)
  |> List.map(unwrap);

/*
   [1, 1, 2, 2] => [1, 2] => 3
   [91212129] => [9] => 9
   Filter list to elements where i + 1 is the same as i. When i = length - 1, compare to 0.
 */
let solution = (input) =>
  input
  |> split("")
  |> List.map(int_of_string)
  |> filterWithIndexAndListAndLength(
       (i, n, ns, length) => i < length - 1 ? n === List.nth(ns, i + 1) : n === List.nth(ns, 0)
     )
  |> List.fold_left((+), 0)
  |> string_of_int;