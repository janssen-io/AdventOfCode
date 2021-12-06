namespace Aoc.Library;

public class Arguments
{
	private readonly string[] _args;

	public Arguments(string[] args)
	{
		_args = args;
	}
	public (int year, int day) GetPuzzleNumber(DateTime defaultNumber)
	{
		if (_args.Length != 2 || !int.TryParse(_args[0], out int year) || !int.TryParse(_args[1], out int day))
		{
			day = defaultNumber.Day;
			year = defaultNumber.Year;
		}

		return (year, day);
	}
}