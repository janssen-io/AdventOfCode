namespace Aoc.Library.IO;

public class TextReader : IReadInput
{
	private readonly System.IO.TextReader _reader;
	
	public TextReader(System.IO.TextReader reader)
	{
		_reader = reader;
	}
	
	public string[] ReadPersonalInput(PuzzleNumber puzzleNumber)
	{
		return _reader.ReadToEnd().Split();
	}
	
	public string[] ReadExample(PuzzleNumber puzzleNumber)
	{
		return _reader.ReadToEnd().Split();
	}
}