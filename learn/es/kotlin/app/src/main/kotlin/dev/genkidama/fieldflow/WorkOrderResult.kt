package dev.genkidama.fieldflow

sealed interface WorkOrderResult<out T> {
    data class Success<T>(val value: T) : WorkOrderResult<T>
    data class Invalid(val message: String) : WorkOrderResult<Nothing>
    data class NotFound(val id: String) : WorkOrderResult<Nothing>
}
