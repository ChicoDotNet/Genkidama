package dev.genkidama.fieldflow

import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import kotlinx.serialization.SerializationException
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json

class FileWorkOrderRepository(
    private val file: Path,
    private val json: Json = Json {
        prettyPrint = true
        ignoreUnknownKeys = true
    },
) : WorkOrderRepository {
    override fun save(order: WorkOrder): WorkOrder {
        val orders = loadAll().associateByTo(linkedMapOf()) { it.id }
        orders[order.id] = order
        persist(orders.values.toList())
        return order
    }

    override fun findById(id: String): WorkOrder? = loadAll().firstOrNull { it.id == id }

    override fun findAll(): List<WorkOrder> = loadAll()

    private fun loadAll(): List<WorkOrder> {
        if (Files.notExists(file)) {
            return emptyList()
        }

        val raw = Files.readString(file)
        if (raw.isBlank()) {
            return emptyList()
        }

        return try {
            json.decodeFromString<List<WorkOrderRecord>>(raw).map { it.toDomain() }
        } catch (error: SerializationException) {
            throw IllegalStateException("Could not read FieldFlow data from $file", error)
        } catch (error: IllegalArgumentException) {
            throw IllegalStateException("FieldFlow data contains an unsupported value in $file", error)
        }
    }

    private fun persist(orders: List<WorkOrder>) {
        file.parent?.let(Files::createDirectories)
        val targetDirectory = file.toAbsolutePath().parent
        val temporary = Files.createTempFile(targetDirectory, "fieldflow-", ".tmp")

        try {
            Files.writeString(temporary, json.encodeToString(orders.map { it.toRecord() }))
            try {
                Files.move(
                    temporary,
                    file,
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING)
            }
        } finally {
            Files.deleteIfExists(temporary)
        }
    }
}
