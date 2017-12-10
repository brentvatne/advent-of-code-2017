open StringUtils;

let solution = (grid) => {
  let splitOnSpaces = (str) => split(" ", str);
  let stringToInt = (row) => List.map(int_of_string, row);
  let sortAscending = (row) => List.sort(compare, row);
  let lastMinusFirst = (row) => List.hd(List.rev(row)) - List.hd(row);
  let sum = (list) => List.fold_left((+), 0, list);
  grid
  |> split("\n")
  |> List.map(row => row |> splitOnSpaces |> stringToInt |> sortAscending |> lastMinusFirst)
  |> sum
};