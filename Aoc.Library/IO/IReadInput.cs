namespace Aoc.Library.IO;

public interface IReadInput
{
	string[] ReadPersonalInput(PuzzleNumber puzzleNumber);
	string[] ReadExample(PuzzleNumber puzzleNumber);
}