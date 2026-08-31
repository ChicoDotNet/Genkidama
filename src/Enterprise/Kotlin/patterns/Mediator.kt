object MediatorExample {
    private class CheckoutMediator {
        private val colleagues = mutableMapOf<String, (String, String) -> Unit>()

        fun register(name: String, receiver: (String, String) -> Unit) {
            colleagues[name] = receiver
        }

        fun send(sender: String, recipient: String, message: String): Boolean {
            val receiver = colleagues[recipient] ?: return false
            receiver(sender, message)
            return true
        }
    }

    fun run(): Boolean {
        val deliveries = mutableListOf<String>()
        val mediator = CheckoutMediator()

        mediator.register("payment") { sender, message ->
            deliveries += "payment<-$sender:$message"
        }
        mediator.register("inventory") { sender, message ->
            deliveries += "inventory<-$sender:$message"
        }

        val reserveDelivered = mediator.send("payment", "inventory", "reserve")
        val chargedDelivered = mediator.send("inventory", "payment", "reserved")
        val unknownRejected = !mediator.send("payment", "shipping", "dispatch")

        return reserveDelivered &&
            chargedDelivered &&
            unknownRejected &&
            deliveries == listOf("inventory<-payment:reserve", "payment<-inventory:reserved")
    }
}
