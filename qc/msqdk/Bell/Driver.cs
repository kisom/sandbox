using Microsoft.Quantum.Simulation.Core;
using Microsoft.Quantum.Simulation.Simulators;

namespace Quantum.Bell
{
    class Driver
    {
        static void Main(string[] args)
        {
            using (var sim = new QuantumSimulator()) {
                // Try initial values.
                Result[] initials = new Result[] { Result.Zero, Result.One };
                foreach (Result initial in initials) {
                    var res = BellTest.Run(sim, 1000, initial).Result;
                    var (numZeroes, numOnes, agree) = res;
                    System.Console.WriteLine($"Init:{initial,-4} |0>={numZeroes,-4}, |1>={numOnes,-4}, agree={agree,-4}");
                }
            }
            System.Console.WriteLine("Press any key to continue.");
            System.Console.ReadKey();
        }
    }
}