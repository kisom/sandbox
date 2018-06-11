import copy
import logging

class Action:

    def __init__(self, pos_precond, neg_precond, retracts, updates):
        self.pos_precond = copy.deepcopy(pos_precond)
        self.neg_precond = copy.deepcopy(neg_precond)
        self.retracts = copy.deepcopy(retracts)
        self.updates = copy.deepcopy(updates)

    def satisfied(self, kb, subject, obj):
        for fact in self.pos_precond:
            if not kb.ask(kb.subst(fact, subject, obj)):
                logging.warning('{} is not valid in the current knowledgebase'.format(fact))
                return False

        for fact in self.neg_precond:
            if kb.ask(kb.subst(fact, subject, obj)):
                logging.warning('{} is valid in the current knowledgebase'.format(fact))
                return False
        return True

    def perform(self, kb, subject, obj):
        if not self.satisfied(kb, subject, obj):
            return None
        kbprime = copy.deepcopy(kb)
        for retraction in self.retracts:
            kbprime.retract(kb.subst(retraction, subject, obj))
        for update in self.updates:
            kbprime.tell(kb.subst(update, subject, obj))
        return kbprime

