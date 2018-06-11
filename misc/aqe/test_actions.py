import actions
import kb
import unittest

INITIAL_FACTS = [
    ('is', 'N29EO', 'Plane'),
    ('at', 'N29EO', 'dia'),
    ('is', 'N10IV', 'Plane'),
    ('at', 'N10IV', 'oak'),
    ('is', 'N33FR', 'Plane'),
    ('at', 'N33FR', 'lga'),
    ('is', '1Z12345E0205271688', 'Package'),
    ('at', '1Z12345E0205271688', 'dia'),
    ('is', '1Z12345E6605272234', 'Package'),
    ('at', '1Z12345E6605272234', 'dia'),
    ('is', '1Z12345E0305271640', 'Package'),
    ('at', '1Z12345E0305271640', 'oak'),
    ('is', '1Z12345E1305277940', 'Package'),
    ('at', '1Z12345E1305277940', 'lga'),
    ('is', '1Z12345E6205277936', 'Package'),
    ('at', '1Z12345E6205277936', 'lga'),
    ('is', 'dia', 'Airport'),
    ('is', 'lga', 'Airport'),
    ('is', 'oak', 'Airport'),
]

FLY_POS_PRECONDS = [
    ('is', '?subject', 'Plane'),
    ('is', '?object', 'Airport'),
]

FLY_NEG_PRECONDS = [
    ('at', '?subject', '?object'),
]

FLY_RETRACTIONS = [
    ('at', '?subject', '?current'), 
]

FLY_UPDATES = [
    ('at', '?subject', '?object'),
]

fly = actions.Action(FLY_POS_PRECONDS, FLY_NEG_PRECONDS,
                     FLY_RETRACTIONS, FLY_UPDATES)

class ActionTestSuite(unittest.TestCase):

    def setUp(self):
        self.kb = kb.from_facts(INITIAL_FACTS)

    def test_a_flight(self):
        self.assertTrue(self.kb.ask(('at', 'N10IV', 'oak')))
        self.assertFalse(self.kb.ask(('at', 'N10IV', 'lga')))

        shadow = fly.perform(self.kb, 'N10IV', 'lga')
        self.assertTrue(shadow)

        # Shadow should reflect the updates and retractions.
        self.assertTrue(shadow.ask(('at', 'N10IV', 'lga')))
        self.assertFalse(shadow.ask(('at', 'N10IV', 'oak')))

        # The original shouldn't be touched.
        self.assertTrue(self.kb.ask(('at', 'N10IV', 'oak')))
        self.assertFalse(self.kb.ask(('at', 'N10IV', 'lga')))


