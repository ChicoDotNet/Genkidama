package dev.genkidama.fieldflow

interface WorkOrderRepository {
    fun save(order: WorkOrder): WorkOrder
    fun findById(id: String): WorkOrder?
    fun findAll(): List<WorkOrder>
}

class InMemoryWorkOrderRepository(initialOrders: Iterable<WorkOrder> = emptyList()) : WorkOrderRepository {
    private val orders = initialOrders.associateByTo(mutableMapOf()) { it.id }

    override fun save(order: WorkOrder): WorkOrder {
        orders[order.id] = order
        return order
    }

    override fun findById(id: String): WorkOrder? = orders[id]

    override fun findAll(): List<WorkOrder> = orders.values.toList()
}
