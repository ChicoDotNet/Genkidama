abstract type Command end

struct Deposit <: Command
    amount::Int
end

struct Withdraw <: Command
    amount::Int
end

execute(balance::Int, command::Deposit) = balance + command.amount
execute(balance::Int, command::Withdraw) = balance - command.amount

queue = Command[Deposit(50), Withdraw(20)]
balance = foldl(execute, queue; init = 100)
@assert balance == 130
@assert length(queue) == 2
println("balance=$(balance);commands=$(length(queue))")
