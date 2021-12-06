using System.Reflection;

namespace Aoc.Library;

public interface ICollectPuzzles
{
	void Add(int year, Puzzle instance);
	Puzzle Get(int year, int day);
}

// ReSharper disable once InconsistentNaming
public static class ICollectPuzzlesExtensions
{
	public static void AddYear(this ICollectPuzzles collection, int year)
	{
		var puzzles = Assembly.Load("Aoc" + year)
			.GetTypes()
			.Where(t => t.IsAssignableTo(typeof(Puzzle)));

		foreach (var puzzleType in puzzles)
		{
			var puzzle = Activator.CreateInstance(puzzleType) as Puzzle;
			if (puzzle == null)
				throw new TypeLoadException("Cannot instance puzzle " + puzzleType.Name);

			collection.Add(year, puzzle);
		}
	}
}
