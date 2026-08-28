class Handler {
  Handler(this.name, this.limit, [this.next]);

  final String name;
  final int limit;
  final Handler? next;

  String handle(int amount, List<String> visited) {
    visited.add(name);
    if (amount <= limit) {
      return 'handled=$name;result=refund($amount)';
    }
    final successor = next;
    if (successor == null) {
      return 'handled=none;result=rejected';
    }
    return successor.handle(amount, visited);
  }
}

void main() {
  final escalation = Handler('escalation', 1 << 30);
  final billing = Handler('billing', 500, escalation);
  final faq = Handler('faq', 50, billing);
  final visited = <String>[];
  final result = faq.handle(250, visited);
  print('visited=${visited.join('>')};$result');
}
