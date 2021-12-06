using Aoc.Console;
using Aoc.Library;

var puzzles = new PuzzleCollection(2021);
var runner = new Runner("../../../2021", puzzles);

var arguments = new Arguments(args);
(int year, int day) = arguments.GetPuzzleNumber(DateTime.Now);

Console.WriteLine("Input");
(string a, string b) = runner.Solve(year, day, "input");
Console.WriteLine(a);
Console.WriteLine(b);

Console.WriteLine();

Console.WriteLine("Example");
(a, b) = runner.Solve(year, day, "example");
Console.WriteLine(a);
Console.WriteLine(b);

