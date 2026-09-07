package main

// price applies an interchangeable pricing policy without knowing its algorithm.
func price(base int, strategy func(int) int) int {
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

	if !(price(100, regular) == 100 &&
		price(100, vip) == 80 &&
		price(100, campaign) == 75 &&
		price(80, campaign) == 80) {
		panic("Go Strategy contract failed")
	}
}
