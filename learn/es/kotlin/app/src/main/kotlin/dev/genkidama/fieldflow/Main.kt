package dev.genkidama.fieldflow

fun main() {
    val board = WorkOrderBoard()
    board.add(WorkOrder("OT-001", "Revisar bomba de agua", Priority.HIGH))
    board.add(WorkOrder("OT-002", "Cambiar luminaria", Priority.MEDIUM))

    println("FieldFlow — órdenes pendientes")
    board.pendingByPriority().forEach { order ->
        println("${order.id} | ${order.priority} | ${order.title}")
    }
}
