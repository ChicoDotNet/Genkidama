package dev.genkidama.fieldflow

import kotlinx.serialization.Serializable

@Serializable
data class WorkOrderRecord(
    val id: String,
    val title: String,
    val priority: String,
    val status: String,
)

fun WorkOrder.toRecord(): WorkOrderRecord = WorkOrderRecord(
    id = id,
    title = title,
    priority = priority.name,
    status = status.name,
)

fun WorkOrderRecord.toDomain(): WorkOrder = WorkOrder(
    id = id,
    title = title,
    priority = Priority.valueOf(priority),
    status = WorkOrderStatus.valueOf(status),
)
