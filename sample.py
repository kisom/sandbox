import base64
import itertools
import json
import kb
import pickle
import random

FACTS = """
gANdcQAoWAIAAABpc3EBWAgAAABhaXJsaW5lcnECWAUAAABGbHllcnEDh3EEaAFYBwAAAG9ha2xh
bmRxBVgHAAAAQWlycG9ydHEGh3EHaAFoBVgEAAAAQ2l0eXEIh3EJaAFYBgAAAGRlbnZlcnEKaAaH
cQtoAWgKaAiHcQxoAVgGAAAAY2JyNjAwcQ1YBgAAAERyaXZlcnEOh3EPaAFYBwAAAHRyb29wZXJx
EGgOh3ERWAIAAABhdHESaAJoCodxE2gSaA1oBYdxFGUu
"""

def load():
    facts = base64.decodebytes(FACTS.encode('ascii'))
    facts = pickle.loads(facts)
    skb = kb.KnowledgeBase()
    for fact in facts:
        skb.tell(fact)
        
    return skb

def load_facts(corpus_path='data/corpus.json', is_count=1000000):
    facts = set()
    corpus = json.loads(open(corpus_path).read())
    if 'nouns' in corpus and 'adjectives' in corpus:
        perms = list(itertools.product(corpus['nouns'],
            corpus['adjectives']))
        if len(perms) < is_count:
            is_count = len(perms)-1;
        pool = random.choices(perms, k=is_count)
        for noun, adjective in pool:
            facts.add(('is', noun, adjective))

    if 'cities' in corpus:
        for city in corpus['cities']:
            facts.add(('is', city, 'City'))

    return facts

def generate_tail_number():
    letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tailno = 'N' + str(random.randint(10, 99))
    tailno += random.choice(letters)
    tailno += random.choice(letters)
    return tailno
