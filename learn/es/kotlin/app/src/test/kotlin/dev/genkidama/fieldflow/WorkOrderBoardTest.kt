package dev.genkidama.fieldflow

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class WorkOrderBoardTest {
    @Test
    fun `pending orders are sorted by priority`() {
        val board = WorkOrderBoard(
            listOf(
                WorkOrder("OT-1", "Baja", Priority.LOW),
                WorkOrder("OT-2", "Alta", Priority.HIGH),
                WorkOrder("OT-3", "Media", Priority.MEDIUM),
            ),
        )

        assertEquals(listOf("OT-2", "OT-3", "OT-1"), board.pendingByPriority().map { it.id })
    }

    @Test
    fun `completed order leaves pending queue`() {
        val board = WorkOrderBoard(listOf(WorkOrder("OT-1", "Revisar equipo", Priority.HIGH)))

        board.complete("OT-1")

        assertEquals(emptyList(), board.pendingByPriority())
        assertEquals(WorkOrderStatus.DONE, board.all().single().status)
    }

    @Test
    fun `duplicate id is rejected`() {
        val board = WorkOrderBoard(listOf(WorkOrder("OT-1", "Primera", Priority.LOW)))

        assertFailsWith<IllegalArgumentException> {
            board.add(WorkOrder("OT-1", "Duplicada", Priority.HIGH))
        }
    }

    @Test
    fun `missing order cannot be completed`() {
        val board = WorkOrderBoard()

        assertFailsWith<NoSuchElementException> { board.complete("OT-404") }
    }
}
