using Aoc.Library;

namespace Aoc.Console;

public class PuzzleCollection : ICollectPuzzles
{
	private readonly Dictionary<(int, int), Puzzle> _puzzles = new ();

	public PuzzleCollection(params int[] years)
	{
		foreach (var year in years)
			this.AddYear(year);
	}
	
	public void Add(int year, Puzzle instance) => _puzzles.Add((year,instance.Day), instance);
	public Puzzle Get(int year, int day) => _puzzles[(year, day)];
}