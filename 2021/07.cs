using Aoc.Library.Puzzles;

namespace Aoc2021;

public class Day07 : Puzzle
{
	public override int Day => 7;
	
	public override string SolvePartA(string[] input)
	{
		return Solve(input, CalculateFuel);
	}
	
	public override string SolvePartB(string[] input)
	{
		return Solve(input, CalculateGaussianFuel);
	}

	private static string Solve(string[] input, Func<int[], int, int> fuelCalculator)
	{
		var crabs = input[0].Split(',').Select(int.Parse).ToArray();

		var leastFuel = int.MaxValue;
		for (var i = crabs.Min(); i <= crabs.Max(); i++)
		{
			var fuel = fuelCalculator(crabs, i);
			if (fuel < leastFuel)
			{
				leastFuel = fuel;
			}
		}

		return leastFuel.ToString();
	}

	private int CalculateFuel(int[] crabs, int position) =>
		crabs.Aggregate(0, (subtotal, crab) => subtotal + Math.Abs(crab - position));
	
	private int CalculateGaussianFuel(int[] crabs, int position) =>
		crabs.Aggregate(0, (subtotal, crab) => subtotal + Gauss(Math.Abs(crab - position)));

	private int Gauss(int n) => n * (n + 1) / 2;
}