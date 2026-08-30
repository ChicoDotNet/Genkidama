class Account:
    def __init__(self, balance):
        self.balance = balance

    def deposit(self, amount):
        self.balance += amount

    def withdraw(self, amount):
        self.balance -= amount


class Command:
    def __init__(self, action, amount):
        self.action = action
        self.amount = amount

    def execute(self):
        self.action(self.amount)


account = Account(100)
queue = [Command(account.deposit, 50), Command(account.withdraw, 20)]
for command in queue:
    command.execute()
assert account.balance == 130
print("balance={};commands={}".format(account.balance, len(queue)))
