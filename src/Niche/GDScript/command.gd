extends SceneTree

class Account:
	extends RefCounted
	var balance := 100

	func deposit(amount: int) -> void:
		balance += amount

	func withdraw(amount: int) -> void:
		balance -= amount

class Command:
	extends RefCounted
	var action: Callable

	func _init(command_action: Callable) -> void:
		action = command_action

	func execute() -> void:
		action.call()

func _init() -> void:
	var account := Account.new()
	var queue: Array[Command] = [
		Command.new(func(): account.deposit(50)),
		Command.new(func(): account.withdraw(20)),
	]
	for command in queue:
		command.execute()
	assert(account.balance == 130)
	print("balance=%d;commands=%d" % [account.balance, queue.size()])
	quit()
