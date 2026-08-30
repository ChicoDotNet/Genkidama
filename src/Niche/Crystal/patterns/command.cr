abstract class Command
  getter amount : Int32

  def initialize(@amount : Int32)
  end

  abstract def execute(balance : Int32) : Int32
end

class Deposit < Command
  def execute(balance : Int32) : Int32
    balance + amount
  end
end

class Withdraw < Command
  def execute(balance : Int32) : Int32
    balance - amount
  end
end

queue = [Deposit.new(50).as(Command), Withdraw.new(20).as(Command)]
balance = 100
queue.each { |command| balance = command.execute(balance) }
raise "Command contract failed" unless balance == 130 && queue.size == 2
puts "balance=#{balance};commands=#{queue.size}"
