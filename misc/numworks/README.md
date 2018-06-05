numworks
========

Various programs for my [numworks](https://numworks.com/) calculator.


Echelon
-------

This is a tool to help explore transformations to convert a matrix to
row echelon form.

```python
>>> from echelon import *
>>> m = EMatrix([[1, 1, 1], [3, 2, 1], [2, 1, 2]], [15, 28, 23])
>>> print(m)
 1  1  1  -> 15
 3  2  1  -> 28
 2  1  2  -> 23
>>> m.add(-3, 0, 1)
>>> m.add(-2, 0, 2)
>>> m.scale(-1, 1)
>>> m.add(1, 1, 2)
>>> m.scale(1/2, 2)
>>> print(m)
 1  1  1  -> 15
 0  1  2  -> 17
 0  0  1  ->  5
```
