using System.Diagnostics;

namespace Aoc.Library;

public class Runner
{
	private readonly string _inputPath;
	private readonly ICollectPuzzles _puzzles;

	public Runner(string inputPath, ICollectPuzzles puzzles)
	{
		_inputPath = inputPath;
		_puzzles = puzzles;
	}
	
	
	public (string, string) Benchmark(int year, int day, string type, out long ticks)
	{
		var input = ReadLines(day, type);
		var puzzle = _puzzles.Get(year, day);
		
		var sw = new Stopwatch();
		sw.Start();
		string answerA = puzzle.SolvePartA(input);
		string answerB = puzzle.SolvePartB(input);
		ticks = sw.ElapsedMilliseconds;

		return (answerA, answerB);
	}
	
	public (string, string) Solve(int year, int day, string type)
	{
		var input = ReadLines(day, type);
		var puzzle = _puzzles.Get(year, day);
		
		string answerA = puzzle.SolvePartA(input);
		string answerB = puzzle.SolvePartB(input);

		return (answerA, answerB);
	}

	private string[] ReadLines(int day, string type)
	{
		using var file = new StreamReader($"{_inputPath}/{day:D2}-{type}.txt");
		var input = file.ReadToEnd().Split();
		return input;
	}
}