import random

def generate_tail_number():
    letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tailno = 'N' + random.randint(10, 99) 
    tailno += random.choice(letters)
    tailno += random.choice(letters)
    return tailno
