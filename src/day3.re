let blocksInSquare = (squareNumber) =>
  switch squareNumber {
  | 0 => 1
  | n => 2 + 2 * (n - 1) + 6 * n
  };

type squareProperties = {
  squareNumber: int,
  initialBlock: int
};

let findSquare = (block) => {
  let rec _findSquareNumber = (firstBlockInSquare, currSquareNumber) => {
    let lastBlockInSquare = firstBlockInSquare + blocksInSquare(currSquareNumber);
    lastBlockInSquare > block ?
      {squareNumber: currSquareNumber, initialBlock: firstBlockInSquare} :
      _findSquareNumber(lastBlockInSquare, currSquareNumber + 1)
  };
  _findSquareNumber(1, 0)
};

type directions =
  | Right
  | Up
  | Left
  | Down;

let findMoveDirection = (block) => {
  let {squareNumber, initialBlock} = findSquare(block);
  switch (block - initialBlock) {
  | 0 => Right
  | i when i <= 1 + (squareNumber - 1) * 2 => Up
  | i when i <= 1 + (squareNumber - 1) * 2 + 2 * squareNumber => Left
  | i when i <= 1 + (squareNumber - 1) * 2 + 4 * squareNumber => Down
  | i when i <= 1 + (squareNumber - 1) * 2 + 6 * squareNumber => Right
  | _ => assert false
  }
};

/*
 * Do not actually use this outside of this context! It only barely works :P
 * */
let range = (~min, ~max) => {
  let rec _range = (x, acc) =>
    if (x === max) {
      List.rev(acc)
    } else {
      _range(x + 1, [x, ...acc])
    };
  _range(min, [])
};

let findCoords = (block) =>
  if (block === 1) {
    (0, 0)
  } else {
    let {squareNumber, initialBlock} = findSquare(block);
    let initialPoint = (squareNumber, squareNumber - 1);
    List.fold_left(
      ((x, y), block) =>
        switch (findMoveDirection(block + 1)) {
        | Right => (x + 1, y)
        | Left => (x - 1, y)
        | Down => (x, y + 1)
        | Up => (x, y - 1)
        },
      initialPoint,
      range(~min=initialBlock, ~max=block)
    )
  };

let abs = (x) => x < 0 ? x * (-1) : x;

let absSum = ((a, b)) => abs(a) + abs(b);

let solution = (input) => input |> findCoords |> absSum;