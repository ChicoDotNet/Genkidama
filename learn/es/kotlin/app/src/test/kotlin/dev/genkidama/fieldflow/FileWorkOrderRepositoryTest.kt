package dev.genkidama.fieldflow

import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class FileWorkOrderRepositoryTest {
    @Test
    fun `saved orders survive repository recreation`() {
        val directory = Files.createTempDirectory("fieldflow-test-")
        val file = directory.resolve("orders.json")
        val original = WorkOrder("WO-100", "Inspect pump", Priority.HIGH)

        FileWorkOrderRepository(file).save(original)

        val reopened = FileWorkOrderRepository(file)
        assertEquals(original, reopened.findById("WO-100"))
    }

    @Test
    fun `saving the same id replaces the persisted snapshot`() {
        val directory = Files.createTempDirectory("fieldflow-test-")
        val file = directory.resolve("orders.json")
        val repository = FileWorkOrderRepository(file)
        val original = WorkOrder("WO-101", "Replace filter", Priority.MEDIUM)

        repository.save(original)
        repository.save(original.complete())

        assertEquals(listOf(original.complete()), FileWorkOrderRepository(file).findAll())
    }

    @Test
    fun `corrupted data fails with storage context`() {
        val directory = Files.createTempDirectory("fieldflow-test-")
        val file = directory.resolve("orders.json")
        Files.writeString(file, "not-json")

        val error = assertFailsWith<IllegalStateException> {
            FileWorkOrderRepository(file).findAll()
        }

        assertEquals(true, error.message?.contains(file.toString()) == true)
    }
}
