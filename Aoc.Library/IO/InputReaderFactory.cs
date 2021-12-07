namespace Aoc.Library.IO;

public static class InputReaderFactory
{
	public static IReadInput CreateConsoleReader()
	{
		return new TextReader(Console.In);
	}
	
	public static IReadInput CreateFileReader(string inputDirectoryPath)
	{
		return new FileReader(inputDirectoryPath);
	}
}