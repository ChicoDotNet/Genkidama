import scala.collection.mutable

trait DocumentStore {
  def get(id: Int): String
}

final class RemoteDocumentStore extends DocumentStore {
  var fetchCount: Int = 0
  def get(id: Int): String = {
    fetchCount += 1
    "doc(" + id + ")"
  }
}

final class DocumentStoreProxy extends DocumentStore {
  private var backend: Option[RemoteDocumentStore] = None
  private val cache = mutable.Map.empty[Int, String]

  def get(id: Int): String = cache.getOrElseUpdate(id, {
    val real = backend.getOrElse {
      val created = new RemoteDocumentStore
      backend = Some(created)
      created
    }
    real.get(id)
  })

  def backendCount: Int = if (backend.isDefined) 1 else 0
  def fetchCount: Int = backend.map(_.fetchCount).getOrElse(0)
}

object ProxyExample extends App {
  val store = new DocumentStoreProxy
  val first = store.get(42)
  val second = store.get(42)
  println("backend=" + store.backendCount + ";fetches=" + store.fetchCount + ";first=" + first + ";second=" + second)
}
