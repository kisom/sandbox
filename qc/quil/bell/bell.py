#!/usr/bin/env python3
# bell.py tries to run the same Bell state program demonstrating simple
# entanglement of qubits.
from pyquil.quil import Program
from pyquil.api import QVMConnection
from pyquil.gates import CNOT, H

def main():
    qvm = QVMConnection()

    print('Constructing program.')
    p = Program()
    p.inst(H(0)).inst(CNOT(0, 1)).measure(0, 0).measure(1, 1)
    print('Constructed program:')
    print('--------------------')
    print(p)
    print('--------------------')

    print('\nRunning program on simulator.')
    results = qvm.run(p, trials=1000)
    
    stats = {0: 0, 1: 0, 'agreement': 0}
    for result in results:
        if result[0] == 0:
            stats[0] += 1
        if result[0] == 1:
            stats[1] += 1
        if result[0] == result[1]:
            stats['agreement'] += 1

    print('Results: |0>: {:d} |1>: {:d} agree: {:d}'.format(
        stats[0], stats[1], stats['agreement']))
            
if __name__ == '__main__':
    main()