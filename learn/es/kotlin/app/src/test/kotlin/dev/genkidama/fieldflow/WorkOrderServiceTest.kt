package dev.genkidama.fieldflow

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs

class WorkOrderServiceTest {
    @Test
    fun createReturnsSuccessAndPersistsOrder() {
        val repository = InMemoryWorkOrderRepository()
        val service = WorkOrderService(repository)

        val result = service.create("OT-10", "Revisar bomba", Priority.HIGH)

        val success = assertIs<WorkOrderResult.Success<WorkOrder>>(result)
        assertEquals("OT-10", success.value.id)
        assertEquals(1, repository.findAll().size)
    }

    @Test
    fun createRejectsDuplicateIdWithoutThrowing() {
        val repository = InMemoryWorkOrderRepository(listOf(WorkOrder("OT-10", "Original", Priority.LOW)))
        val service = WorkOrderService(repository)

        val result = service.create("OT-10", "Duplicada", Priority.HIGH)

        assertIs<WorkOrderResult.Invalid>(result)
        assertEquals(1, repository.findAll().size)
    }

    @Test
    fun completeReturnsNotFoundForUnknownId() {
        val service = WorkOrderService(InMemoryWorkOrderRepository())

        val result = service.complete("OT-404")

        val notFound = assertIs<WorkOrderResult.NotFound>(result)
        assertEquals("OT-404", notFound.id)
    }

    @Test
    fun pendingIsSortedAndExcludesCompletedOrders() {
        val repository = InMemoryWorkOrderRepository(listOf(
            WorkOrder("OT-1", "Baja", Priority.LOW),
            WorkOrder("OT-2", "Alta", Priority.HIGH),
            WorkOrder("OT-3", "Hecha", Priority.HIGH, WorkOrderStatus.DONE),
        ))
        val service = WorkOrderService(repository)

        assertEquals(listOf("OT-2", "OT-1"), service.pending().map { it.id })
    }
}
