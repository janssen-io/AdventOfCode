using Aoc.Library.Puzzles;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aoc2021;
public class Day14 : Puzzle
{
    private Dictionary<(int, char, char), Dictionary<char, long>> memo = new();
    private Dictionary<(char, char), char> rules = new();

    public override int Day => 14;

    public override string SolvePartA(string[] input)
    {
        string polymer = input[0];
        for(var i = 2; i < input.Length; i++)
        {
            var parts = input[i].Split(" -> ");
            rules.Add((parts[0][0], parts[0][1]), parts[1][0]);
        }

        for(int i = 0; i < 10; i++)
        {
            polymer = Step(polymer);
        }

        var quantities = new Dictionary<char, long>();
        for (int i = 0; i < polymer.Length; i++)
        {
            var letter = polymer[i];
            quantities[letter] = quantities.GetOrDefault(letter, 0) + 1;
        }

        var counts = quantities.Values.OrderBy(x => x).ToArray();
        return (counts[^1] - counts[0]).ToString();
    }

    public override string SolvePartB(string[] input)
    {
        string polymer = input[0];
        for(var i = 2; i < input.Length; i++)
        {
            var parts = input[i].Split(" -> ");
            rules[(parts[0][0], parts[0][1])] = parts[1][0];
        }

        var quantities = new Dictionary<char, long>();
        for(int i = 0; i < polymer.Length - 1; i++)
        {
            var subcounts = MemoStep(polymer[i], polymer[i + 1], 40);
            quantities.SumInto(subcounts);
        }

        quantities[polymer[^1]] = quantities.GetOrDefault(polymer[^1], 0) + 1;

        var counts = quantities.Values.OrderBy(x => x).ToArray();
        return (counts[^1] - counts[0]).ToString();
    }

    private string Step(string template)
    {
        var polymer = new StringBuilder(3);
        polymer.Append(template[0]);
        for (var i = 0; i < template.Length - 1; i++)
        {
            var a = template[i];
            var b = template[i + 1];
            if(rules.TryGetValue((a, b), out char letter))
            {
                polymer.Append(letter);
            }
            polymer.Append(b);
        }

        return polymer.ToString();
    }

    private Dictionary<char, long> MemoStep(char left, char right, int maxIterations)
    {
        // Structures repeat themselves, if we have already calculated a structure at a certain depth
        // We can just return the result.
        var key = (maxIterations, left, right);
        if (memo.TryGetValue(key, out var count))
            return count;

        // We should only count the left part, otherwise we count everything double.
        // mstep(AB, [AB -> C], 1)
        // = mstep(AC, ..., 0) + mstep(CB, ..., 0)
        // = { A: 1 } + {C: 1}
        // The last 'molecule' is not counted, this is taken care of in the calling function.
        if (maxIterations == 0)
            return new() { { left, 1 } };

        if(rules.TryGetValue((left, right), out var middle))
        {
            var rleft = MemoStep(left, middle, maxIterations - 1);
            var rright = MemoStep(middle, right, maxIterations - 1);
            var sum = rleft.SumWith(rright);
            memo.Add(key, sum);
            return sum;
        }

        return new Dictionary<char, long>();

        throw new Exception($"No rule for {left}{right}");
    }
}

public static class DictionaryExtensions
{
    public static TValue GetOrDefault<TKey, TValue>(this Dictionary<TKey, TValue> dict, TKey key, TValue defaultValue)
    {
        if (dict.TryGetValue(key, out TValue? val))
            return val!;

        return defaultValue;
    }

    public static Dictionary<TKey, long> SumWith<TKey>(this Dictionary<TKey, long> a, Dictionary<TKey, long> b)
    {
        var c = new Dictionary<TKey, long>();
        foreach(var kv in a) { c.Add(kv.Key, kv.Value); }
        foreach(var kv in b) { c[kv.Key] = c.GetOrDefault(kv.Key, 0) + kv.Value; }
        return c;
    }

    public static void SumInto<TKey>(this Dictionary<TKey, long> a, Dictionary<TKey, long> b)
    {
        foreach(var kv in b) { a[kv.Key] = a.GetOrDefault(kv.Key, 0) + kv.Value; }
    }
}