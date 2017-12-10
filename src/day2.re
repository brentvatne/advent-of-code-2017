open StringUtils;

let solution = (grid) => {
  let splitAndMakeInt = (str) =>
    str |> split(" ") |> List.map(int_of_string) |> List.sort(compare);
  let lastMinusFirst = (row) => List.nth(row, List.length(row) - 1) - List.hd(row);
  let sum = (list) => List.fold_left((acc, x) => acc + x, 0, list);
  grid |> split("\n") |> List.map(splitAndMakeInt) |> List.map(lastMinusFirst) |> sum
};