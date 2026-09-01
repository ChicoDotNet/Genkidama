package main

import "fmt"

type documentMemento struct {
	title string
	tags  []string
}

type mementoDocument struct {
	title string
	tags  []string
}

func (d mementoDocument) save() documentMemento {
	tags := append([]string(nil), d.tags...)
	return documentMemento{title: d.title, tags: tags}
}

func (d *mementoDocument) restore(snapshot documentMemento) {
	d.title = snapshot.title
	d.tags = append([]string(nil), snapshot.tags...)
}

func verifyMementoCanonical() {
	document := mementoDocument{title: "draft", tags: []string{"pattern"}}
	snapshot := document.save()

	document.title = "published"
	document.tags = append(document.tags, "edited")

	if snapshot.title != "draft" || fmt.Sprint(snapshot.tags) != "[pattern]" {
		panic("snapshot changed with live state")
	}
	if document.title != "published" || fmt.Sprint(document.tags) != "[pattern edited]" {
		panic("live mutation was not observable")
	}

	document.restore(snapshot)
	if document.title != "draft" || fmt.Sprint(document.tags) != "[pattern]" {
		panic("restore did not recover the snapshot")
	}

	document.tags[0] = "restored"
	if snapshot.tags[0] != "pattern" {
		panic("restore aliased the caretaker snapshot")
	}
}

func main() {
	verifyMementoCanonical()
	fmt.Println("Go Memento: passed")
}
