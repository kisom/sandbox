

```python
import actions, kb, sample
```

## AQE: A Query Engine

This is an implementation of a knowledge base, hacked together in Python
3 (it won't work in Python 2 for reasons of modules) for now to quickly
iterate on ideas.

The `KnowledgeBase` is a repository of facts.


```python
skb = sample.load()
```

A fact is a tuple: (relationship, subject, object). `object` is admittedly a terrible name (and is subject to change) but it's what I came up with and what I'm working with for now.


```python
skb.facts()
```




    [('is', 'cbr600', 'Driver'),
     ('at', 'cbr600', 'oakland'),
     ('at', 'airliner', 'denver'),
     ('is', 'oakland', 'Airport'),
     ('is', 'airliner', 'Flyer'),
     ('is', 'oakland', 'City'),
     ('is', 'trooper', 'Driver'),
     ('is', 'denver', 'City'),
     ('is', 'denver', 'Airport')]



A KB can be told a fact with the `tell` method.


```python
skb.tell(('is', 'san francisco', 'cool'))
```

Similarly, the KB can be told a fact is *not* true with the `retract` method.


```python
skb.retract(('is', 'san francisco', 'cool'))
```

The KB can be queried about the facts it has. There are two types of queries. The first is done with a full fact, and represents the question "Is this fact true?"


```python
print(skb.ask(('is', 'oakland', 'City')))
print(skb.ask(('is', 'cbr600', 'City')))
```

    [('is', 'oakland', 'City')]
    []


A query returns a list of facts; the empty list means no facts were found. This might seem an odd way to represent this first question; an invalid fact is represented by an empty list, or it returns a list of a single fact. The reason for doing it this way is to support the second type of question: "What are the facts for which this query is valid?" This is done by providing a `None` value to *either* the subject or object. (Eventually, I'll get around to adding support for empty relationships too...)


```python
print(skb.ask(('is', None, 'City')))
```

    [('is', 'oakland', 'City'), ('is', 'denver', 'City')]


Another thing the KB can do is provide some basic substution using the `subst` method. It takes a fact template, a subject, and an object, and returns a fact (without making any statement as to the validity of the fact). The subject and object can be one of several values:

+ `None`: the subject or object (depending on which position is `None`) from the arguments is substituted into the fact.
+ `?subject`: substitutes the subject.
+ `?object`: substitutes the object.
+ `?current`: the current value is kept --- this must be used only with singleton facts.
+ `?any`: the value is kept as `None`.

Some examples should clarify this.


```python
print(skb.subst(('is', None, 'City'), 'oakland', None))
print(skb.subst(('at', '?subject', '?current'), 'cbr600', None))
print(skb.subst(('is', '?object', '?subject'), 'oakland', 'City'))
```

    ('is', 'oakland', 'City')
    ('at', 'cbr600', 'oakland')
    ('is', 'City', 'oakland')


To understand `subst`, it's useful to note that it was written to support actions.

Actions are initialised with a positive precondition (facts that must be valid for the action to be performed), a negative precondition (facts that must not be valid for the action to be performed), a set of retractions, and a set of updates.

To illustrate this, here's a small example of airplanes and airports.


```python
airport_kb = kb.from_facts([
    ('is', 'N29EO', 'Plane'),
    ('at', 'N29EO', 'dia'),
    ('is', 'N10IV', 'Plane'),
    ('at', 'N10IV', 'oak'),
    ('is', 'N33FR', 'Plane'),
    ('at', 'N33FR', 'lga'),
    ('is', 'dia', 'Airport'),
    ('is', 'lga', 'Airport'),
    ('is', 'oak', 'Airport'),
])

fly = actions.Action(
    [('is', '?subject', 'Plane'), ('is', '?object', 'Airport')],  # Positive preconditions.
    [('at', '?subject', '?object'),],                             # Negative preconditions.
    [('at', '?subject', '?current'),],                            # Retractions.
    [('at', '?subject', '?object')])                              # Updates.
```

For a `fly` action to be performed, there's a few facts we should make sure are true:

1. The subject of the action is a `Plane`, and
2. The object of the action is an `Airport`.

We should make sure that the subject isn't currently at our target airport.

If these hold, we can perform the action. The retraction says that the subject is no longer at the airport it was at before the action, and the KB is updated to say that the plane is at a new airport.


```python
print('Before flying, is N10IV at LGA? ', airport_kb.ask(('at', 'N10IV', 'lga')))
print('Before flying, is N10IV at OAK? ', airport_kb.ask(('at', 'N10IV', 'oak')))

new_airport_kb = fly.perform(airport_kb, 'N10IV', 'lga')

print('After flying, is N10IV at LGA? ', new_airport_kb.ask(('at', 'N10IV', 'lga')))
print('After flying, is N10IV at OAK? ', new_airport_kb.ask(('at', 'N10IV', 'oak')))
```

    Before flying, is N10IV at LGA?  []
    Before flying, is N10IV at OAK?  [('at', 'N10IV', 'oak')]
    After flying, is N10IV at LGA?  [('at', 'N10IV', 'lga')]
    After flying, is N10IV at OAK?  []


There's more work to be done, but this represents a solid night of putting the plan into action based on what I'd learned from the AI nanodegree. I've got a bigger vision for what I want to do out of this, but it's nice to have a baseline to reason about.
