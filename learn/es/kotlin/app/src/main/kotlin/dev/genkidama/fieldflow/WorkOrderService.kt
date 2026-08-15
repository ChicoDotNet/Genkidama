package dev.genkidama.fieldflow

class WorkOrderService(private val repository: WorkOrderRepository) {
    fun create(id: String, title: String, priority: Priority): WorkOrderResult<WorkOrder> {
        if (id.isBlank()) return WorkOrderResult.Invalid("id must not be blank")
        if (title.isBlank()) return WorkOrderResult.Invalid("title must not be blank")
        if (repository.findById(id) != null) return WorkOrderResult.Invalid("work order '$id' already exists")

        val order = WorkOrder(id = id, title = title, priority = priority)
        return WorkOrderResult.Success(repository.save(order))
    }

    fun complete(id: String): WorkOrderResult<WorkOrder> {
        val current = repository.findById(id) ?: return WorkOrderResult.NotFound(id)
        return WorkOrderResult.Success(repository.save(current.complete()))
    }

    fun pending(): List<WorkOrder> = repository.findAll()
        .filter { it.status == WorkOrderStatus.OPEN }
        .sortedByDescending { it.priority.ordinal }
}
