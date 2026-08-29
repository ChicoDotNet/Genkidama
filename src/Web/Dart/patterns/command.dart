sealed class Command {
  const Command(this.amount);
  final int amount;

  int execute(int balance);
}

final class Deposit extends Command {
  const Deposit(super.amount);

  @override
  int execute(int balance) => balance + amount;
}

final class Withdraw extends Command {
  const Withdraw(super.amount);

  @override
  int execute(int balance) => balance - amount;
}

void main() {
  final queue = <Command>[const Deposit(50), const Withdraw(20)];
  var balance = 100;
  for (final command in queue) {
    balance = command.execute(balance);
  }
  assert(balance == 130);
  assert(queue.length == 2);
  print('balance=$balance;commands=${queue.length}');
}
