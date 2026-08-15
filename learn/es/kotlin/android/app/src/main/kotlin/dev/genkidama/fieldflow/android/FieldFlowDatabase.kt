package dev.genkidama.fieldflow.android

import androidx.room.Dao
import androidx.room.Database
import androidx.room.Entity
import androidx.room.Insert
import androidx.room.OnConflictStrategy
import androidx.room.PrimaryKey
import androidx.room.Query
import androidx.room.RoomDatabase
import kotlinx.coroutines.flow.Flow

@Entity(tableName = "work_orders")
data class WorkOrderEntity(
    @PrimaryKey val id: String,
    val title: String,
    val priority: String,
    val completed: Boolean,
)

@Dao
interface WorkOrderDao {
    @Query("SELECT * FROM work_orders ORDER BY completed ASC, title ASC")
    fun observeAll(): Flow<List<WorkOrderEntity>>

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun upsert(order: WorkOrderEntity)

    @Query("UPDATE work_orders SET completed = 1 WHERE id = :id")
    suspend fun markCompleted(id: String): Int
}

@Database(entities = [WorkOrderEntity::class], version = 1, exportSchema = false)
abstract class FieldFlowDatabase : RoomDatabase() {
    abstract fun workOrders(): WorkOrderDao
}
