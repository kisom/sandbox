"""
AQE: A Query Engine

This is a proof of concept of a baseline query engine for AI work.
"""

class InvalidQuery(Exception):
    pass

class Inconsistency(Exception):
    def __init__(self, fact):
        self.fact = fact
    
    def __str__(self):
        return 'Inconsistency: {}'.format(self.fact)

class KnowledgeBase:
    
    def __init__(self):
        # TODO(kyle): support loading an initial set of facts.
        self.__kb__ = {}
        self.__facts__ = set()
        
    def tell(self, fact):
        relationship, subject, obj = fact
        
        # NB: in the future, these assertions may not need to be true; there
        # might be space in the world for "fuzzy" facts.
        assert(relationship)
        assert(subject)
        assert(obj)
        if relationship not in self.__kb__:
            self.__kb__[relationship] = {'subjects':{}, 'objects': {}}
        
        if subject not in self.__kb__[relationship]['subjects']:
            self.__kb__[relationship]['subjects'][subject] = set()
        self.__kb__[relationship]['subjects'][subject].add(obj)
        
        if obj not in self.__kb__[relationship]['objects']:
            self.__kb__[relationship]['objects'][obj] = set()
        self.__kb__[relationship]['objects'][obj].add(subject)
        self.__facts__.add(fact)

    def retract(self, fact):
        relationship, subject, obj = fact
        
        # For now, these assertions are required. In the future, it would be
        # interesting to say something to the effect of "forget everything you
        # know about X".
        assert(relationship)
        assert(subject)
        assert(obj)
        
        # TODO(kyle): answer existential question: if I delete all the objects
        # from a subject (or vice versa), should that subject/object be kept or
        # removed entirely? This is the difference between "I have no concept
        # of X" and "I am aware that X exists but I don't know anything about it".
        # For now, I'm electing to keep the entry.
        #
        # Similarly, if the relationship is empty, we could make the argument
        # for removing it --- at the expense of now saying that we have no
        # concept of this relationship.
        try:
            self.__kb__[relationship]['subjects'][subject].remove(obj)
            self.__kb__[relationship]['objects'][obj].remove(subject)
            self.__facts__.remove(fact)
        except KeyError:
            # Being told to forget something about something you don't know
            # isn't an error.
            pass
        pass

    def ask(self, fact):
        relationship, subject, obj = fact
        
        # A future milestone will remove this requirement to support free
        # variables.
        assert(relationship)
        
        if relationship and subject and obj:
            if fact in self.__facts__:
                return [fact,]
            return []

        if relationship and subject:
            return [(relationship, subject, _obj) for _obj
                    in self.__kb__[relationship]['subjects'][subject]]

        if relationship and obj:
            return [(relationship, _subject, obj) for _subject
                    in self.__kb__[relationship]['objects'][obj]]

    def facts(self):
        return list(self.__facts__)
        
    def is_consistent(self):
        try:
            for fact in self.__facts__:
                relationship, subject, obj = fact
                if obj not in self.__kb__[relationship]['subjects'][subject]:
                    raise Inconsistency(fact)
                if subject not in self.__kb__[relationship]['objects'][obj]:
                    raise Inconsistency(fact)

            for relationship, v in self.__kb__.items():
                for subject in v['subjects'].keys():
                    for obj in v['subjects'][subject]:
                        if (relationship, subject, obj) not in self.__facts__:
                            raise Inconsistency(fact)

                for obj in v['objects'].keys():
                    for subject in v['objects'][obj]:
                        if (relationship, subject, obj) not in self.__facts__:
                            raise Inconsistency(fact)
        except KeyError:
            raise Inconsistency(fact)

        return True

    def __len__(self):
        return len(self.__facts__)

    def subst(self, fact, subject, obj):
        relationship, _subject, _obj = fact
        if _subject is None:
            _subject = subject
        if _subject == '?any':
            _subject = None
        elif _subject == '?subject':
            _subject = subject
        elif _subject == '?object':
            _subject = obj

        if _obj is None:
            _obj = obj
        if _obj == '?any':
            _obj = None
        elif _obj == '?subject':
            _obj = subject
        elif _obj == '?object':
            _obj = obj

        if _subject == '?current':
            possibilities = self.ask((relationship, None, _obj))
            assert(len(possibilities) == 1)
            _, _subject, _ = possibilities[0]
        elif _obj == '?current':
            possibilities = self.ask((relationship, subject, None))
            assert(len(possibilities) == 1)
            _, _, _obj = possibilities[0]

        return (relationship, _subject, _obj)


def from_facts(facts):
    kb = KnowledgeBase()
    for fact in facts:
        kb.tell(fact)
    return kb
