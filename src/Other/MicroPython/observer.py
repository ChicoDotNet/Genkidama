class RecordingObserver:
    def __init__(self, name):
        self.name = name
        self.events = []

    def update(self, state):
        self.events.append(state)


class Subject:
    def __init__(self):
        self._observers = []

    def subscribe(self, observer):
        if observer in self._observers:
            return False
        self._observers.append(observer)
        return True

    def unsubscribe(self, observer):
        if observer not in self._observers:
            return False
        self._observers.remove(observer)
        return True

    def publish(self, state):
        for observer in tuple(self._observers):
            observer.update(state)


subject = Subject()
audit = RecordingObserver("audit")
dashboard = RecordingObserver("dashboard")

assert subject.subscribe(audit)
assert subject.subscribe(dashboard)
assert not subject.subscribe(audit)

subject.publish("draft")

assert subject.unsubscribe(dashboard)
assert not subject.unsubscribe(dashboard)

subject.publish("published")

assert audit.events == ["draft", "published"]
assert dashboard.events == ["draft"]

print("observer=audit:draft,published;dashboard:draft;duplicate=rejected;second-unsubscribe=rejected")
