const std = @import("std");

const Observer = *const fn (i32) i32;

const Subject = struct {
    observers: [4]Observer = undefined,
    count: usize = 0,

    fn subscribe(self: *Subject, observer: Observer) bool {
        for (self.observers[0..self.count]) |registered| {
            if (registered == observer) return false;
        }
        if (self.count == self.observers.len) return false;
        self.observers[self.count] = observer;
        self.count += 1;
        return true;
    }

    fn unsubscribe(self: *Subject, observer: Observer) bool {
        for (self.observers[0..self.count], 0..) |registered, index| {
            if (registered != observer) continue;
            var current = index;
            while (current + 1 < self.count) : (current += 1) {
                self.observers[current] = self.observers[current + 1];
            }
            self.count -= 1;
            return true;
        }
        return false;
    }

    fn notify(self: *const Subject, event_id: i32, results: []i32) usize {
        const delivered = @min(self.count, results.len);
        for (self.observers[0..delivered], 0..) |observer, index| {
            results[index] = observer(event_id);
        }
        return delivered;
    }
};

fn auditObserver(event_id: i32) i32 {
    return event_id + 1;
}

fn dashboardObserver(event_id: i32) i32 {
    return event_id + 2;
}

pub fn examplePasses() bool {
    var subject = Subject{};
    if (!subject.subscribe(auditObserver)) return false;
    if (!subject.subscribe(dashboardObserver)) return false;
    if (subject.subscribe(auditObserver)) return false;

    var results: [4]i32 = undefined;
    if (subject.notify(42, &results) != 2) return false;
    if (results[0] != 43 or results[1] != 44) return false;

    if (!subject.unsubscribe(auditObserver)) return false;
    if (subject.unsubscribe(auditObserver)) return false;
    if (subject.notify(42, &results) != 1) return false;
    return results[0] == 44;
}

pub fn main() void {
    std.debug.assert(examplePasses());
    std.debug.print("Zig Observer: passed\n", .{});
}
