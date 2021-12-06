namespace Aoc.Library;

public abstract class Puzzle
{
	public abstract int Day { get; }
	public abstract string SolvePartA(string[] input);
	public abstract string SolvePartB(string[] input);
}