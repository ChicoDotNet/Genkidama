package main

// ObserverCallback is the notification contract used by the subject.
type ObserverCallback func(int) string

type observerSubscription struct {
	key    string
	notify ObserverCallback
}

// ObserverSubject owns subscription lifecycle without knowing concrete observers.
type ObserverSubject struct {
	observers []observerSubscription
}

func (s *ObserverSubject) Subscribe(key string, notify ObserverCallback) bool {
	for _, registered := range s.observers {
		if registered.key == key {
			return false
		}
	}
	s.observers = append(s.observers, observerSubscription{key: key, notify: notify})
	return true
}

func (s *ObserverSubject) Unsubscribe(key string) bool {
	for index, registered := range s.observers {
		if registered.key == key {
			s.observers = append(s.observers[:index], s.observers[index+1:]...)
			return true
		}
	}
	return false
}

func (s ObserverSubject) Publish(id int) []string {
	out := make([]string, 0, len(s.observers))
	for _, registered := range s.observers {
		out = append(out, registered.notify(id))
	}
	return out
}

func observerExamplePasses() bool {
	subject := ObserverSubject{}
	audit := func(id int) string { return "audit:" + itoa(id) }
	dashboard := func(id int) string { return "dashboard:" + itoa(id) }

	if !subject.Subscribe("audit", audit) || !subject.Subscribe("dashboard", dashboard) {
		return false
	}
	if subject.Subscribe("dashboard", dashboard) {
		return false
	}
	first := subject.Publish(42)
	if len(first) != 2 || first[0] != "audit:42" || first[1] != "dashboard:42" {
		return false
	}
	if !subject.Unsubscribe("dashboard") || subject.Unsubscribe("missing") {
		return false
	}
	second := subject.Publish(43)
	return len(second) == 1 && second[0] == "audit:43"
}

func itoa(value int) string {
	if value == 0 {
		return "0"
	}
	digits := [20]byte{}
	index := len(digits)
	for value > 0 {
		index--
		digits[index] = byte('0' + value%10)
		value /= 10
	}
	return string(digits[index:])
}
