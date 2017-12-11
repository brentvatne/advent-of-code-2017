open Jest;

open Expect;

describe(
  "Day3",
  () => {
    test("first case", () => expect(Day3.solution(1)) |> toBe(0));
    test("second case", () => expect(Day3.solution(12)) |> toBe(3));
    test("third case", () => expect(Day3.solution(23)) |> toBe(2));
    test("fourth case", () => expect(Day3.solution(1024)) |> toBe(31));
    test("puzzle input", () => expect(Day3.solution(368078)) |> toBe(371))
  }
);