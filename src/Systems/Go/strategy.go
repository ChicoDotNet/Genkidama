package main

// strategyPrice applies an interchangeable pricing policy without knowing its algorithm.
// The distinct helper name keeps this canonical source linkable beside the historical
// aggregate sweep while that sweep is being deduplicated after the Observer reconciliation.
func strategyPrice(base int, strategy func(int) int) int {
	return strategy(base)
}

func verifyStrategy() {
	regular := func(value int) int { return value }
	vip := func(value int) int { return value * 80 / 100 }
	campaign := func(value int) int {
		if value >= 100 {
			return value - 25
		}
		return value
	}

	if !(strategyPrice(100, regular) == 100 &&
		strategyPrice(100, vip) == 80 &&
		strategyPrice(100, campaign) == 75 &&
		strategyPrice(80, campaign) == 80) {
		panic("Go Strategy contract failed")
	}
}

func init() {
	verifyStrategy()
}
