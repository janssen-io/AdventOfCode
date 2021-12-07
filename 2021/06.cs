using Aoc.Library.Puzzles;

namespace Aoc2021;

public class Day06 : Puzzle
{
	public override int Day => 6;
	
	private Dictionary<int, decimal> _lifetimes = new();
	
	private decimal PopulationCount => _lifetimes.Values.Sum();
	public override string SolvePartA(string[] input)
	{
		Initialize(input[0].Split(',').Select(int.Parse).ToArray());
		Simulate(80);
		return PopulationCount.ToString();
	}
	
	public override string SolvePartB(string[] input)
	{
		Initialize(input[0].Split(',').Select(int.Parse).ToArray());
		Simulate(256);
		return PopulationCount.ToString();
	}

	private void Initialize(int[] fishes)
	{
		for (int i = 0; i < 9; i++)
			_lifetimes[i] = fishes.Count(fish => fish == i);
	}
	
	private void Cycle()
	{
		var babies = _lifetimes[0];
		for (int i = 0; i < 8; i++)
		{
			_lifetimes[i] = _lifetimes[i + 1];
		}

		_lifetimes[6] += babies;
		_lifetimes[8] = babies;
	}

	private void Simulate(int days)
	{
		for (int i = 0; i < days; i++)
			Cycle();
	}
}