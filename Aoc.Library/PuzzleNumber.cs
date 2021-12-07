namespace Aoc.Library;

public struct PuzzleNumber
{
	public int Year { get; }
	public int Day { get; }

	public PuzzleNumber(int year, int day)
	{
		Year = year;
		Day = day;
	}
}