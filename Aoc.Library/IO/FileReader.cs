namespace Aoc.Library.IO;

public class FileReader : IReadInput
{
	private readonly string _inputDirectoryPath;

	public FileReader(string inputDirectoryPath)
	{
		_inputDirectoryPath = inputDirectoryPath;
	}
	
	public string[] ReadPersonalInput(PuzzleNumber puzzleNumber)
	{
		using var stream = new StreamReader(Path.Join(_inputDirectoryPath, $"{puzzleNumber.Day:D2}-input.txt"));
		return stream.ReadToEnd().Split();
	}
	
	public string[] ReadExample(PuzzleNumber puzzleNumber)
	{
		using var stream = new StreamReader(Path.Join(_inputDirectoryPath, $"{puzzleNumber.Day:D2}-example.txt"));
		return stream.ReadToEnd().Split();
	}
}