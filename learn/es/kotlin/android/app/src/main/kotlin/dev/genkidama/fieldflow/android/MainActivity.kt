package dev.genkidama.fieldflow.android

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.room.Room
import kotlinx.coroutines.launch

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val database = Room.databaseBuilder(
            applicationContext,
            FieldFlowDatabase::class.java,
            "fieldflow.db",
        ).build()

        setContent {
            MaterialTheme {
                Surface(modifier = Modifier.fillMaxSize()) {
                    FieldFlowScreen(database.workOrders())
                }
            }
        }
    }
}

@Composable
fun FieldFlowScreen(dao: WorkOrderDao) {
    val orders by dao.observeAll().collectAsState(initial = emptyList())
    val scope = rememberCoroutineScope()

    Column(
        modifier = Modifier.padding(24.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        Text("FieldFlow", style = MaterialTheme.typography.headlineMedium)
        Text("Room conserva las órdenes; Compose refleja el estado observable.")

        Button(
            onClick = {
                scope.launch {
                    dao.upsert(
                        WorkOrderEntity(
                            id = "WO-001",
                            title = "Revisar bomba de agua",
                            priority = "HIGH",
                            completed = false,
                        ),
                    )
                }
            },
        ) {
            Text("Crear orden de ejemplo")
        }

        orders.forEach { order ->
            Row(horizontalArrangement = Arrangement.spacedBy(12.dp)) {
                Text("${order.id}: ${order.title} [${order.priority}]")
                if (!order.completed) {
                    Button(onClick = { scope.launch { dao.markCompleted(order.id) } }) {
                        Text("Completar")
                    }
                } else {
                    Text("Completada")
                }
            }
        }
    }
}
