namespace Quantum.Bell
{
    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Primitive;

    operation Set (desired: Result, q1: Qubit) : ()
    {
        body
        {
            let current = M(q1);

            if (desired != current) {
                X(q1);
            }
        }
    }

    operation BellTest (count: Int, initial: Result) : (Int, Int, Int)
    {
        body {
            mutable numOnes = 0; // by default in Q#, variables are immutable.
                                 // Q# doesn't require type annotations for variables.
            mutable agree = 0;
            
            // using allocates an array of qubits for use in a block of code.
            // all qubits are dynamically allocated and released.
            using (qubits = Qubit[2])
            {
                for (test in 1..count) {
                    Set(initial, qubits[0]);
                    Set(Zero, qubits[1]);

                    H(qubits[0]);
                    CNOT(qubits[0], qubits[1]);
                    let res = M(qubits[0]);

                    if (res == M(qubits[1])) {
                        set agree = agree + 1;
                    }

                    // Count the number of ones we've seen.
                    if (res == One) {
                        set numOnes = numOnes + 1;
                    }
                    Set(Zero, qubits[0]);
                    Set(Zero, qubits[1]);
                }
            }
            return (count-numOnes, numOnes, agree);
        }
    }
}
