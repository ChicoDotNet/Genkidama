function result = command
%COMMAND Encapsulate account operations as queued command values.
account = struct("balance", 100);
commands = [
    struct("name", "deposit", "amount", 50, "execute", @deposit, "undo", @withdraw)
    struct("name", "withdraw", "amount", 20, "execute", @withdraw, "undo", @deposit)
];

queued = strings(1, numel(commands));
executed = strings(1, numel(commands));

for index = 1:numel(commands)
    current = commands(index);
    queued(index) = current.name + "(" + string(current.amount) + ")";
    account = current.execute(account, current.amount);
    executed(index) = current.name;
end

balanceBeforeUndo = account.balance;
lastCommand = commands(end);
account = lastCommand.undo(account, lastCommand.amount);

result = struct( ...
    "queue", strjoin(queued, ">"), ...
    "executed", strjoin(executed, ">"), ...
    "balance", balanceBeforeUndo, ...
    "undone", lastCommand.name + "(" + string(lastCommand.amount) + ")", ...
    "balanceAfterUndo", account.balance);

if nargout == 0
    fprintf( ...
        'queued=%s;executed=%s;balance=%d;undo=%s;balance_after_undo=%d\n', ...
        char(result.queue), ...
        char(result.executed), ...
        result.balance, ...
        char(result.undone), ...
        result.balanceAfterUndo);
end
end

function account = deposit(account, amount)
account.balance = account.balance + amount;
end

function account = withdraw(account, amount)
account.balance = account.balance - amount;
end
