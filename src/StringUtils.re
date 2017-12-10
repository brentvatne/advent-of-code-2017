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

let split = (delimiter, s) =>
  if (String.length(delimiter) === 0) {
    explode(s)
  } else {
    let result =
      List.fold_right(
        (c, (acc, segment)) =>
          switch c {
          | s when s === delimiter =>
            String.length(segment) > 0 ? ([segment, ...acc], "") : (acc, "")
          | _ => (acc, c ++ segment)
          },
        explode(s),
        ([], "")
      );
    switch result {
    | (words, "") => words
    | (words, segment) => [segment, ...words]
    }
  };