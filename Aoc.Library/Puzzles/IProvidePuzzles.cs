namespace Aoc.Library.Puzzles;

public interface IProvidePuzzles
{
	Puzzle Get(PuzzleNumber puzzleNumber);
}