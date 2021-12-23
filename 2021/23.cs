using Aoc.Library.Puzzles;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Aoc2021;
public class Day23 : Puzzle
{
    public static int SIZE = 2;

    public override int Day => 23;

    public override string SolvePartA(string[] input)
    {
        Console.WriteLine();
        var sw = new Stopwatch();
        SIZE = 2;
        var init = new State(new Bucket[] {
            new Bucket(SIZE, 'B', 'A'),
            new Bucket(SIZE, 'C', 'D'),
            new Bucket(SIZE, 'D', 'B'),
            new Bucket(SIZE, 'A', 'C')
        });

        //return PlayManually(init);

        sw.Start();
        List<long> solutions = Solve(init, useAssumption: true);
        sw.Stop();
        Console.WriteLine(sw.ElapsedMilliseconds);
        return solutions.First().ToString();
    }

    public override string SolvePartB(string[] input)
    {
        Console.WriteLine();
        var sw = new Stopwatch();
        SIZE = 4;
        var init = new State(new Bucket[] {
            new Bucket(SIZE, 'B', 'D', 'D', 'A'),
            new Bucket(SIZE, 'C', 'B', 'C', 'D'),
            new Bucket(SIZE, 'D', 'A', 'B', 'B'),
            new Bucket(SIZE, 'A', 'C', 'A', 'C')
        });
        sw.Start();
        List<long> solutions = Solve(init);
        sw.Stop();
        Console.WriteLine(sw.ElapsedMilliseconds);
        return solutions.First().ToString();
    }

    private List<long> Solve(State init, bool useAssumption = false)
    {
        const bool isDfs = true;
        var q = new Stack<State>();
        q.Push(init);

        var solutions = new List<long>();

        var seen = new HashSet<string>();
        var i = 0;
        int previousStep = 0;
        while (q.Count > 0)
        {
            i++;
            var state = q.Pop();
            if (i % 100000 == 0) // state.steps > previousStep)
            {
                previousStep = state.steps;
                Console.WriteLine($"{i}, {solutions.Count}, {state.steps}, {q.Count}\r");
            }
            //Console.WriteLine(state);
            seen.Add(state.GetHashKey(isDfs));
            foreach (var move in state.GenMoves(useAssumption))
            {
                var newState = state.Clone();
                move(newState);
                if (IsSorted(newState))
                {
                    solutions.Add(newState.energyConsumed);
                    continue;
                }
                if (!seen.Contains(newState.GetHashKey(isDfs))
                    // max steps for optimal solution cannot exceed it
                    // The rule for move generation should enforce this, but for some reason they don't in DFS.
                    && state.steps <= SIZE * 4 * 2) 
                {
                    q.Push(newState);
                }
            }
        }

        solutions.Sort();
        return solutions;
    }

    private string PlayManually(State state)
    {
        var history = new List<State>();
        while(true)
        {
            Console.WriteLine();
            Console.WriteLine("- - - - - - - - - - - - - - - - - - - -");
            Console.WriteLine(state.ToString());
            if (IsSorted(state))
            {
                Console.WriteLine("Congrats!");
                break;
            }
            Console.WriteLine("Next Step: [q]uit, [p]revious, in <slot> <bucket>, out <bucket> <slot>");
            var answer = Console.ReadLine();
            try
            {
                if (answer == null || answer.Length == 0 || answer[0] == 'q') break;
                else if (answer[0] == 'p')
                {
                    state = history[history.Count - 1];
                    history.RemoveAt(history.Count - 1);
                }
                else if (answer[0] == 'i')
                {
                    history.Add(state);
                    state = state.Clone();
                    var parts = answer.Split(' ');
                    state.MoveIn(int.Parse(parts[2]), int.Parse(parts[1]));
                }
                else if (answer[0] == 'o')
                {
                    history.Add(state);
                    state = state.Clone();
                    var parts = answer.Split(' ');
                    state.MoveOut(int.Parse(parts[1]), int.Parse(parts[2]));
                }
                else
                {
                    Console.WriteLine("Unable to parse answer: " + answer);
                }
            }
            catch (Exception e) {
                Console.WriteLine("Error: " + e.Message);
            };
        }
        return "";
    }



    private class Bucket : IEnumerable<char>
    {
        private Stack<char> pods;
        public readonly int size;

        public Bucket(int size, params char[] pods)
        {
            this.pods = new Stack<char>(pods);
            this.size = size;
        }

        public Bucket Clone() => new(size, pods.Reverse().ToArray());

        public (char, int) Take()
        {
            var distance = this.size - pods.Count;
            var c = pods.Pop();
            return (c, distance);
        }

        public int Put(char c)
        {
            if (this.IsFull()) { throw new InvalidOperationException("Bucket is full"); }

            pods.Push(c);
            var distance = this.size - pods.Count; // 0 for top spot, 1 for bottom
            return distance;
        }

        public bool IsDone(char expected)
        {
            return this.pods.Count == this.size
                && this.pods.All(x => x == expected);
        }

        public bool IsEmpty() => this.pods.Count == 0;
        public bool IsFull() => this.pods.Count == this.size;

        public char Peek()
        {
            return this.pods.Peek();
        }

        public override int GetHashCode()
        {
            return this.pods.GetHashCode();
        }

        public IEnumerator<char> GetEnumerator() => this.pods.GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => this.pods.GetEnumerator();
    }

    private class State
    {
        public Bucket[] buckets = new Bucket[4];
        public long energyConsumed = 0;
        public char[] spaces = new char[7];
        public int steps = 0;

        public State(Bucket[] buckets)
        {
            this.buckets = buckets;
        }

        public IEnumerable<Action<State>> GenMoves(bool useAssumption)
        {
            var expected = new char[] {'A', 'B', 'C', 'D'};

            bool couldMoveIn = false;
            for(var slot = 0; slot <= 6; slot++)
            {
                if (this.spaces[slot] != 0)
                {
                    for(var i = 0; i < this.buckets.Length; i++)
                    {
                        var goal = expected[i];
                        var bucket = this.buckets[i];

                        if (!bucket.IsFull()
                            && this.spaces[slot] == goal  // only move into slots you wanna be
                            && bucket.All(x => x == goal)) // only move if the slot has the right pods
                        {
                            couldMoveIn = true;
                            yield return (State state) => state.MoveIn(i, slot);
                        }
                    }
                }
            }

            // Only move stuff out, if we can't move anything in its place.
            if (couldMoveIn)
            {
                yield break;
            }

            for(var slot = 0; slot <= 6; slot++)
            {
                if (this.spaces[slot] == 0)
                {
                    for(var i = 0; i < this.buckets.Length; i++)
                    {
                        var goal = expected[i];
                        var bucket = this.buckets[i];
                        // Only take things out from buckets that are not done yet.

                        if (!bucket.IsEmpty()
                            && !bucket.IsDone(goal))
                        {
                            var pod = bucket.Peek();
                            if (!useAssumption || pod != 'D' || slot >= 3)
                                yield return (State state) => state.MoveOut(i, slot);
                        }
                    }
                }
            }

        }

        public bool IsBucketDone(int bucket)
        {
            var expected = new char[] {'A', 'B', 'C', 'D'}[bucket];
            return this.buckets[bucket].IsDone(expected);
        }

        public string GetHashKey(bool forDfs = true)
        {
            var builder = new StringBuilder();
            for(var i = 0; i < this.spaces.Length; i++)
            {
                builder.Append(i);
                builder.Append(this.spaces[i] == 0 ? '.' : this.spaces[i]);
            }
            foreach(var bucket in buckets)
            {
                builder.Append(' ');
                builder.Append(new string(bucket.Select(x => x).ToArray()));
            }
            if (forDfs)
                builder.Append(this.energyConsumed);
            return builder.ToString();
        }

        public State Clone()
        {
            var state = new State(buckets.Select(bucket => bucket.Clone()).ToArray());

            state.energyConsumed = this.energyConsumed;
            state.spaces = (char[])this.spaces.Clone();
            state.steps = this.steps;

            return state;
        }

        private int[] Range(int from, int to)
        {
            if (from > to)
            {
                (to, from) = (from, to);
            }

            return Enumerable.Range(from, to - from + 1).ToArray();
        }

        private Dictionary<char, int> price = new()
        {
            { 'A',    1 },
            { 'B',   10 },
            { 'C',  100 },
            { 'D', 1000 },
        };

        public void MoveOut(int bucket, int slot)
        {
            if (this.buckets[bucket].IsEmpty())
            {
                //Console.WriteLine($"Illegal move (bucket empty): bucket: {bucket} slot: {slot}");
                return; // nothing to move
            }

            int[] spacesToMovePast;
            // moving out to the left
            if (slot - 1 <= bucket)
            {
                spacesToMovePast = Range(slot - 1, bucket).Select(x => x + 1).ToArray();
            }
            else
            {
                spacesToMovePast = Range(bucket + 2, slot);
            }

            // if any of the spaces are occupied, then we can't move to our slot
            if (spacesToMovePast.Any(slot => this.spaces[slot] != 0))
            {
                //Console.WriteLine($"Illegal move (path blocked): slot: {spacesToMovePast.First(s => this.spaces[s] != 0)}");
                return;
            }

            int extra = (slot == 0 || slot == 6) ? -1 : 0;

            (var pod, var distance) = this.buckets[bucket].Take();
            this.energyConsumed += price[pod] * (2 * spacesToMovePast.Length + extra + distance);
            this.spaces[slot] = pod;

            this.steps++;
        }

        public void MoveIn(int bucket, int slot)
        {
            if (this.buckets[bucket].IsFull())
            {
                //Console.WriteLine($"Illegal move (bucket full): bucket: {bucket} slot: {slot}");
                return;
            }

            int[] spacesToMovePast;
            // moving in to the left
            if (slot - 1 <= bucket)
            {
                spacesToMovePast = bucket == slot - 1
                    ? new int[0]
                    : Range(slot, bucket).Select(x => x + 1).ToArray();
            }
            else
            {
                spacesToMovePast = bucket + 2 == slot 
                    ? new int[0]
                    : Range(bucket + 2, slot - 1);
            }

            // if any of the spaces are occupied, then we can't move to our slot
            if (spacesToMovePast.Any(slot => this.spaces[slot] != 0))
            {
                //Console.WriteLine($"Illegal move (path blocked): slot: {spacesToMovePast.First(s => this.spaces[s] != 0)}");
                return;
            }

            int extra = (slot == 0 || slot == 6) ? -1 : 0;

            var pod = this.spaces[slot];
            var distance = this.buckets[bucket].Put(pod);
            this.energyConsumed += price[pod] * (2 * (spacesToMovePast.Length + 1) + extra + distance);
            this.spaces[slot] = (char)0;

            this.steps++;
        }

        public override string ToString()
        {
            var text = new StringBuilder();
            var state = this.Clone();
            text.AppendLine($"Energy: {state.energyConsumed}, steps: {state.steps}");
            text.AppendLine($"01 2 3 4 56");
            for(var i = 0; i < 7; i++)
            {
                var c = state.spaces[i];
                if (c == 0) text.Append('.');
                else text.Append(c);
                if (i >= 1 && i <= 4) text.Append('.');
            }
            text.AppendLine();
            for(var i = SIZE - 1; i >= 0; i--)
            {
                text.Append("  ");
                foreach(var bucket in state.buckets)
                {
                    var lastIndex = bucket.Count() - 1;
                    if (i > lastIndex) text.Append(" ");
                    else
                    {
                        (char c, int _) = bucket.Take();
                        text.Append(c);
                    }
                    text.Append(" ");
                }
                text.AppendLine();

            }
            text.AppendLine($"  0 1 2 3");

            return text.ToString();
        }
    }


    private bool IsSorted(State state)
    {
        return state.buckets[0].IsDone('A')
            && state.buckets[1].IsDone('B')
            && state.buckets[2].IsDone('C')
            && state.buckets[3].IsDone('D');
    }

}
