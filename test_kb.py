import copy
import kb
import random
import sample
import unittest


class KnowledgeBaseTestSuite(unittest.TestCase):
    
    def setUp(self):
        self.kb = sample.load()
        
    def test_a_sanity_check(self):
        assert(self.kb.is_consistent())
        for fact in self.kb.__facts__:
            self.assertTrue(self.kb.ask(fact))
        
    def test_tell(self):
        new_fact = ('is', 'berkeley', 'City')
        
        # make sure it's not something we already know
        self.assertFalse(self.kb.ask(new_fact))
        self.kb.tell(new_fact)
        answer = self.kb.ask(new_fact)
        self.assertListEqual(answer, [new_fact,])
        
    def test_inconsistency(self):
        badkb = copy.deepcopy(self.kb)
        badfact = random.choice(badkb.facts())
        relationship, subject, obj = badfact
        
        # muck with subjects part
        badkb.__kb__[relationship]['subjects'][subject].remove(obj)
        with self.assertRaises(kb.Inconsistency):
            badkb.is_consistent()
        
        # muck with objects part
        badkb = copy.deepcopy(self.kb)
        badkb.__kb__[relationship]['objects'][obj].remove(subject)
        with self.assertRaises(kb.Inconsistency):
            badkb.is_consistent()
        
        # muck with facts part
        badkb = copy.deepcopy(self.kb)
        badkb.__facts__.remove(badfact)
        with self.assertRaises(kb.Inconsistency):
            badkb.is_consistent()
            
        # inject false data into the subject
        badkb = copy.deepcopy(self.kb)
        badkb.__kb__[relationship]['subjects'][subject].add('false memory')
        with self.assertRaises(kb.Inconsistency):
            badkb.is_consistent()

        # inject false data into the object
        badkb = copy.deepcopy(self.kb)
        badkb.__kb__[relationship]['objects'][obj].add('false memory')
        with self.assertRaises(kb.Inconsistency):
            badkb.is_consistent()
