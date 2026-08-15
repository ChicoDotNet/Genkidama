package dev.genkidama.fieldflow

enum class Priority { LOW, MEDIUM, HIGH }
enum class WorkOrderStatus { OPEN, DONE }

data class WorkOrder(
    val id: String,
    val title: String,
    val priority: Priority,
    val status: WorkOrderStatus = WorkOrderStatus.OPEN,
) {
    init {
        require(id.isNotBlank()) { "id must not be blank" }
        require(title.isNotBlank()) { "title must not be blank" }
    }

    fun complete(): WorkOrder = copy(status = WorkOrderStatus.DONE)
}
