# # The Meaning of Programs
# Let's define a simple *AST*, with three basic members. An AST node can
# be reducible, like the + in (2 + 2), or not-reducible, like the
# number 2. We'll also reduce from left to right.
class Number:
    def __init__(self, value):
        self.value = int(value)
    def __repr__(self):
        return "#" + str(self.value)
    def is_reducible(self):
        return False

class Add:
    def __init__(self, l, r):
        self.left = l
        self.right = r
    def __repr__(self):
        return repr(self.left) + " + " + repr(self.right)
    def is_reducible(self):
        return True
    def reduce(self, env):
        if self.left.is_reducible():
            return Add(self.left.reduce(env), self.right)
        elif self.right.is_reducible():
            return Add(self.left, self.right.reduce(env))
        else:
            return Number(self.left.value + self.right.value)

class Mult:
    def __init__(self, l, r):
        self.left = l
        self.right = r
    def __repr__(self):
        return repr(self.left) + " * " + repr(self.right)
    def is_reducible(self):
        return True
    def reduce(self, env):
        if self.left.is_reducible():
            return Mult(self.left.reduce(env), self.right)
        elif self.right.is_reducible():
            return Mult(self.left, self.right.reduce(env))
        else:
            return Number(self.left.value * self.right.value)

ast1 = Add(Mult(Number(1), Number(2)),
           Mult(Number(3), Number(4)))

# It would be nice to have a machine that could run these, so we don't
# have to run them by hand.

class Machine:
    def __init__(self, expr, env):
        self.expr = expr
        self.env = env
        self.iters = 0

    def step(self):
        self.iters += 1
        self.expr = self.expr.reduce(self.env)

    def run(self):
        while self.expr.is_reducible():
            print self
            self.step()
        print "execution complete in ", self.iters, " steps."
        return self.expr
    
    def __repr__(self):
        return "(" + str(self.iters) + ") " + repr(self.expr) + "\n" + repr(self.env)

class Boolean:
    def __init__(self, value):
        self.value = value
    def __repr__(self):
        return str(self.value).lower()
    def is_reducible(self):
        return False

class LessThan:
    def __init__(self, l, r):
        self.left = l
        self.right = r
    def __repr__(self):
        return repr(self.left) + " < " + repr(self.right)
    def is_reducible(self):
        return True
    def reduce(self, env):
        if self.left.is_reducible():
            return LessThan(self.left.reduce(env), self.right)
        elif self.right.is_reducible():
            return LessThan(self.left, self.right.reduce(env))
        else:
            return Boolean(self.left.value < self.right.value)

ast2 = LessThan(Number(5), Add(Number(2), Number(2)))
ast2b = LessThan(Add(Number(2), Number(2)), Number(5))

class Variable:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "$" + self.name
    def is_reducible(self):
        return True
    def reduce(self, env):
        return env[self.name]

ast3 = Add(Variable('x'), Variable('y'))
env3 = {'x': Number(3), 'y': Number(4)}
m3 = lambda : Machine(ast3, env3)

class Nop:
    def __init__(self):
        pass
    def __repr__(self):
        return "@NOP"
    def eq(self, st):
        return st.instanceof(Nop)
    def is_reducible(self):
        return False

class Assign:
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr
    def __repr__(self):
        return self.name + " = " + repr(self.expr)
    def is_reducible(self):
        return True

    def reduce(self, env):
        if self.expr.is_reducible():
            return Assign(self.name, self.expr.reduce(env)), env.copy()
        else:
            env[self.name] = self.expr
            return Nop(), env.copy()

stmt4 = Assign("x", Add(Variable("x"), Number(1)))
env4 = {"x": Number(2)}

class Machine2:
    def __init__(self, stmt, env):
        self.stmt = stmt
        self.env = env
        self.iters = 0
    def __repr__(self):
        return "(" + str(self.iters) + ") " + repr(self.stmt) + "\n" + repr(self.env)
    def step(self):
        self.iters += 1
        self.stmt, self.env = self.stmt.reduce(self.env)
        print self

    def run(self):
        while self.stmt.is_reducible():
            self.step()
        print "execution complete in ", self.iters, " steps."
        print self

stmt5 = Assign("x", Add(Variable("x"), Number(1)))
env5 = {"x": Number(2)}
m5 = lambda : Machine2(stmt5, env5)

class If:
    def __init__(self, cond, cons, alt):
        self.cond = cond
        self.cons = cons
        self.alt = alt
    def __repr__(self):
        s = "if |" + repr(self.cond) + "| then |" + repr(self.cons)
        return s + "| else |" + repr(self.alt) + "|"
    def is_reducible(self):
        return True
    def reduce(self, env):
        if self.cond.is_reducible():
            return If(self.cond.reduce(env), self.cons, self.alt), env.copy()
        elif self.cond.value:
            return self.cons, env.copy()
        else:
            return self.alt, env.copy()

stmt6 = If(Variable("x"), Assign("y", Number(1)), Assign("y", Number(2)))
env6 = {"x": Boolean(True)}
m6 = lambda : Machine2(stmt6, env6)

class Sequence:
    def __init__(self, first, second):
        self.first = first
        self.second = second
    def __repr__(self):
        return repr(self.first) + ";" + repr(self.second)
    def is_reducible(self):
        return True
    def reduce(self, env):
        if isinstance(self.first, Nop):
            return self.second, env.copy()
        else:
            first, env = self.first.reduce(env)
            return Sequence(first, self.second), env.copy()

stmt7 = Sequence(Assign("x", Add(Number(1), Number(1))),
                 Assign("y", Add(Variable("x"), Number(3))))
env7 = {}
m7 = lambda : Machine2(stmt7, env7)

class While:
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body
    def __repr__(self):
        return "while |" + repr(self.cond) + "|:\n\t" + repr(self.body)
    def is_reducible(self):
        return True
    def reduce(self, env):
        return If(self.cond, Sequence(self.body, self), Nop()), env.copy()

stmt8 = While(LessThan(Variable("x"), Number(5)),
              Assign("x", Mult(Variable("x"), Number(3))))
env8 = {"x": Number(1)}
m8 = lambda : Machine2(stmt8, env8)
