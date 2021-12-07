using Aoc.Library;
using Aoc.Library.Puzzles;

namespace Aoc.Console;

public class PuzzleCollection : ICollectPuzzles, IProvidePuzzles
{
	private readonly Dictionary<(int, int), Puzzle> _puzzles = new ();

	public void Add(int year, Puzzle instance) => _puzzles.Add((year,instance.Day), instance);
	public Puzzle Get(PuzzleNumber puzzle) => _puzzles[(puzzle.Year, puzzle.Day)];
}