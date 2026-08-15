package dev.genkidama.fieldflow

class WorkOrderBoard(initialOrders: Iterable<WorkOrder> = emptyList()) {
    private val orders = initialOrders.associateByTo(mutableMapOf()) { it.id }

    fun add(order: WorkOrder) {
        require(order.id !in orders) { "work order '${order.id}' already exists" }
        orders[order.id] = order
    }

    fun complete(id: String): WorkOrder {
        val current = orders[id] ?: throw NoSuchElementException("work order '$id' was not found")
        val completed = current.complete()
        orders[id] = completed
        return completed
    }

    fun pendingByPriority(): List<WorkOrder> = orders.values
        .asSequence()
        .filter { it.status == WorkOrderStatus.OPEN }
        .sortedByDescending { it.priority.ordinal }
        .toList()

    fun all(): List<WorkOrder> = orders.values.toList()
}
