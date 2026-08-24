package main

import "fmt"

type AuthService struct{}

func (AuthService) Authenticate(user string) string { return fmt.Sprintf("auth(%s)", user) }

type InventoryService struct{}

func (InventoryService) Reserve(sku string) string { return fmt.Sprintf("reserve(%s)", sku) }

type BillingService struct{}

func (BillingService) Charge(cents int) string { return fmt.Sprintf("charge(%d)", cents) }

type CheckoutFacade struct {
	auth      AuthService
	inventory InventoryService
	billing   BillingService
}

func (f CheckoutFacade) Checkout(user, sku string, cents int) string {
	return f.auth.Authenticate(user) + ">" + f.inventory.Reserve(sku) + ">" + f.billing.Charge(cents)
}

func main() {
	facade := CheckoutFacade{AuthService{}, InventoryService{}, BillingService{}}
	fmt.Printf("checkout=%s\n", facade.Checkout("alice", "SKU-42", 499))
}
