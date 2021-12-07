using System.Reflection;

namespace Aoc.Library.Puzzles;

public interface ICollectPuzzles
{
	void Add(int year, Puzzle instance);
}

// ReSharper disable once InconsistentNaming
public static class ICollectPuzzlesExtensions
{
	public static void AddAssembly(this ICollectPuzzles collection, int year, string assemblyName)
	{
		var puzzles = Assembly.Load(assemblyName)
			.GetTypes()
			.Where(t => t.IsAssignableTo(typeof(Puzzle)));

		foreach (var puzzleType in puzzles)
		{
			if (Activator.CreateInstance(puzzleType) is not Puzzle puzzle)
				throw new TypeLoadException("Cannot instance puzzle " + puzzleType.Name);

			collection.Add(year, puzzle);
		}
	}
}
