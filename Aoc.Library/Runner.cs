using Aoc.Library.IO;
using Aoc.Library.Puzzles;

namespace Aoc.Library;

public class Runner
{
	private readonly IReadInput _inputReader;
	private readonly TextWriter _output;
	private readonly IProvidePuzzles _puzzles;

	public Runner(IReadInput inputReader, TextWriter output, IProvidePuzzles puzzles)
	{
		_inputReader = inputReader;
		_output = output;
		_puzzles = puzzles;
	}
	
	public (string, string) SolveExamplePuzzle(PuzzleNumber puzzleNumber)
	{
		var input = _inputReader.ReadExample(puzzleNumber);
		return SolvePuzzle(puzzleNumber, input, "Example");
	}


	public (string, string) SolvePersonalPuzzle(PuzzleNumber puzzleNumber)
	{
		var input = _inputReader.ReadExample(puzzleNumber);
		return SolvePuzzle(puzzleNumber, input, "Personal");

	}
	private (string, string) SolvePuzzle(PuzzleNumber puzzleNumber, string[] input, string kind)
	{
		var puzzle = _puzzles.Get(puzzleNumber);
		
		_output.Write($"({puzzleNumber.Year}) Solving Puzzle {puzzleNumber.Day}, {kind}, Part A...");
		string answerA = puzzle.SolvePartA(input);

		_output.WriteLine($"\r({puzzleNumber.Year}) Solved Puzzle {puzzleNumber.Day}, {kind}, Part A:   ");
		_output.WriteLine(answerA);
		_output.WriteLine();

		_output.Write($"({puzzleNumber.Year}) Solving Puzzle {puzzleNumber.Day}, {kind}, Part B...");
		string answerB = puzzle.SolvePartB(input);
		_output.WriteLine($"\r({puzzleNumber.Year}) Solved Puzzle {puzzleNumber.Day}, {kind}, Part B:   ");
		_output.WriteLine(answerB);
		_output.WriteLine();

		return (answerA, answerB);
	}
}