#!/usr/bin/env python3

X_B = {
    ('B', 's1'): ('X', 'R', 's2'),
    ('B', 's2'): ('B', 'L', 's3'),
    ('X', 's3'): ('B', 'R', 's4'),
    ('B', 's4'): ('B', 'L', 's1')
}

def simulate(instructions):
    # set up initial state

    # note that tape is hard-coded for now, but should be a data structure that
    # can grow in either direction and the state shouldn't be hard-coded. This
    # works for the initial pass at this.
    tape, head, state = ['B', 'B'], 0, 's1'

    # loop: this should be an infinite loop (with a possible halting state in the state table)    
    for _ in range(8):
        print(state + ': ' + ''.join(tape))
        print('    ' + ' ' * head + '^')

        # look up instruction
        (tape[head], head_dir, state) = instructions[(tape[head], state)]

        # apply instruction
        head += 1 if head_dir == 'R' else -1

        # sanity checks
        assert(head_dir in ['R', 'L'])
        assert(head >= 0)
        assert(head < len(tape))

# four lines of actual code that can simulate any CPU

simulate(X_B)