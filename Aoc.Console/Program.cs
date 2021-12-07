using Aoc.Console;
using Aoc.Library;
using Aoc.Library.IO;
using Aoc.Library.Puzzles;

var puzzles = new PuzzleCollection();
puzzles.AddAssembly(2021, nameof(Aoc2021));

var input = InputReaderFactory.CreateFileReader("../../../../2021");
var runner = new Runner(input, Console.Out, puzzles);

var arguments = new Arguments(args);
var number = arguments.GetPuzzleNumber(DateTime.Now);

_ = runner.SolveExamplePuzzle(number);
_ = runner.SolvePersonalPuzzle(number);
