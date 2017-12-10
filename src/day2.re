open StringUtils;

/*
 * OCaml compare function is polymorphic and compiler warns if we use it with
 * List.sort, so make a compareInt function to use instead :O
 */
let compareInt = (x, y) =>
  switch (x - y) {
  | n when n > 0 => 1
  | n when n < 0 => (-1)
  | 0 => 0
  | _ => assert false /* this is impossible */
  };

let solution = (grid) => {
  let splitOnSpaces = (str) => split(" ", str);
  let mapStringToInt = (row) => List.map(int_of_string, row);
  let sortAscending = (row) => List.sort(compareInt, row);
  let lastMinusFirst = (row) => List.hd(List.rev(row)) - List.hd(row);
  let sum = (list) => List.fold_left((+), 0, list);
  grid
  |> split("\n")
  |> List.map((row) => row |> splitOnSpaces |> mapStringToInt |> sortAscending |> lastMinusFirst)
  |> sum
};