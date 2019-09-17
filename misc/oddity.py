#!/usr/bin/env python3

# see https://www.solipsys.co.uk/new/PowersOfTwoInLexOrder.html?sg28hn
# compare some_stats(9) with some_stats(100). After 9, the standard
# deviation and variance start to rise quickly.

import statistics

def oddities(n):
    ns = []
    for i in range(1, n+1):
        v = 2 ** i
        v = str(v)
        ns.append(v)
    ns.sort()
    return [float(v[0] + '.' + v[1:]) for v in ns]

def evenities(n):
    ns = []
    for i in range(1, n+1):
        v = 10 ** (i / 10.)
        ns.append(v)
    return ns

def differences(n):
    ons = oddities(n)
    ens = evenities(n)
    print(ons)
    print(ens)
    deltas = [abs(ens[i] - ons[i]) for i in range(len(ons))]
    return deltas



def some_stats(n):
    deltas = differences(n)
    print('Min: {}'.format(min(deltas)))
    print('Avg: {}'.format(sum(deltas) / len(deltas)))
    print('Max: {}'.format(max(deltas)))
    print(' Std dev: {}'.format(statistics.stdev(deltas)))
    print('Variance: {}'.format(statistics.variance(deltas)))
