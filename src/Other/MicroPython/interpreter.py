class Number:
    def __init__(self, value):
        self.value = value

    def interpret(self):
        return self.value


class Add:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def interpret(self):
        return self.left.interpret() + self.right.interpret()


expression = Add(Add(Number(2), Number(3)), Number(4))
value = expression.interpret()
assert value == 9
print("interpreter={}".format(value))
