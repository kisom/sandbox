#!/usr/bin/env python3

DAYS = ['M', 'T', 'W', 'R', 'F']

TEST_SET = [
    ('M', 'PDX', 'SEA'),
    ('T', 'PDX', 'SFO'),
    ('T', 'SEA', 'DEN'),
    ('W', 'DEN', 'PDX'),
    ('R', 'PDX', 'DEN'),
    ('F', 'DEN', 'JFK')
]

EXPECTED = [
    [('M', 'PDX', 'SEA'), ('T', 'SEA', 'DEN'), ('W', 'DEN', 'PDX'), ('R', 'PDX', 'DEN'), ('F', 'DEN', 'JFK')],
    [('T', 'PDX', 'SFO')]
]

def self_check():
    print('self_check days: ', end="")
    test_days()

    trip = build_matches(TEST_SET[0], TEST_SET[1:])
    print('leg: {}, trips: {}'.format(TEST_SET[0], trip))

def test_days():
    try:
        assert(next_day('M') == 'T')
        assert(next_day('T') == 'W')
        assert(next_day('W') == 'R')
        assert(next_day('R') == 'F')
        assert(next_day('F') == 'M')
    except:
        print('FAILED')
        raise
    print('OK')

def next_day(day):
    assert(day in DAYS)
    return DAYS[(DAYS.index(day) + 1) % len(DAYS)]

def match_leg(leg1, leg2):
    (day1, origin1, destination1) = leg1
    (day2, origin2, destination2) = leg2
    if day2 == next_day(day1) and origin2 == destination1:
        return True
    return False

def find_next_match(leg, candidates):
    for candidate in candidates:
        if match_leg(leg, candidate):
            return candidate
    return None

def build_matches(leg, candidates):
    trip = []
    cset = set(candidates)
    while True:
        candidate = find_next_match(leg, cset)
        if not candidate:
            return trip, cset
        trip.append(candidate)
        cset.remove(candidate)
        leg = candidate

if __name__ == '__main__':
    self_check()