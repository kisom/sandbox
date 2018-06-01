## AQE: A Query Engine

This is an implementation of a knowledge base, hacked together in Python
3 (it won't work in Python 2 for reasons of modules) for now to quickly
iterate on ideas.

There are a few key points:

+ A `KnowledgeBase` contains facts.
+ A fact is a tuple: (relationship, subject, object). For example,
  `('is', 'sky', 'blue')`.
+ A `KnowledgeBase` has three core methods: ask, retract, and tell.
+ The `ask` method queries the `KnowledgeBase` to ascertain whether
  a fact is true. Either the subject or the object may be `None`,
  in which case all satisifiable facts are returned.
+ The `retract` method tells the `KnowledgeBase` that the fact is
  no longer true. If it's rainy, we might retract our fact about the
  sky being blue.
+ The `tell` method tells the `KnowledgeBase` that the fact is
  now true. For example, if it's rainy (and we've retracted the previous
  'sky is blue' fact), we might tell the `KnowledgeBase` that
  `('is', 'sky', 'grey')`.
+ A `KnowledgeBase` can also perform substitutions.
+ An action contains positive and negative preconditions, retractions,
  and updates. The positive condition list contains facts that must
  be true for a knowledge base, and the negative condition list contains
  facts that must be false. If these preconditions hold, the retractions
  are applied, followed by the updates.
+ See `test_actions.py` for an example.

### Limitations

+ Singleton facts aren't supported; that is, there is no way to make a
  `KnowledgeBase` assert that there is only one relationship → subject
  mapping. For example, the `KnowledgeBase` will admit that
  `('is', 'shrödingers cat', 'alive')` and
  `('is', 'schrödingers cat', 'dead') are both true simultaneously.

### TODO

+ Rewrite in C++?
