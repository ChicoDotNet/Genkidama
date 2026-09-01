# Canonical Crystal Memento example for Genkidama.
# The originator owns capture/restoration; the caretaker only keeps the snapshot.

record MementoSnapshot, state : String

class MementoEditor
  property state : String

  def initialize(@state : String); end

  def save : MementoSnapshot
    MementoSnapshot.new(@state)
  end

  def restore(snapshot : MementoSnapshot)
    @state = snapshot.state
  end
end

def verify_memento_canonical
  editor = MementoEditor.new("draft")
  snapshot = editor.save

  editor.state = "published"
  raise "Memento mutation failed" unless editor.state == "published"
  raise "Memento snapshot changed" unless snapshot.state == "draft"

  editor.restore(snapshot)
  raise "Memento restore failed" unless editor.state == "draft"

  true
end

if PROGRAM_NAME == __FILE__
  verify_memento_canonical
  puts "Crystal Memento: passed"
end
