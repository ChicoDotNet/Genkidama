def erp_transport(value: Int) -> Int:
    return value + 100


def crm_transport(value: Int) -> Int:
    return value + 200


def send_business_message(value: Int, transport: def(Int) thin -> Int) -> Int:
    return transport(value)


def run() -> Bool:
    return send_business_message(5, erp_transport) == 105 and send_business_message(5, crm_transport) == 205
