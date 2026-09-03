module ObserverExample
  alias Observer = Proc(Int32, String)

  class Subject
    def initialize
      @observers = {} of String => Observer
    end

    def subscribe(key : String, observer : Observer) : Bool
      return false if @observers.has_key?(key)

      @observers[key] = observer
      true
    end

    def unsubscribe(key : String) : Bool
      !@observers.delete(key).nil?
    end

    def publish(id : Int32) : Array(String)
      @observers.values.map { |observer| observer.call(id) }
    end
  end

  def self.example_passes? : Bool
    subject = Subject.new
    audit = ->(id : Int32) { "audit:#{id}" }
    dashboard = ->(id : Int32) { "dashboard:#{id}" }

    return false unless subject.subscribe("audit", audit)
    return false unless subject.subscribe("dashboard", dashboard)
    return false if subject.subscribe("audit", audit)
    return false unless subject.publish(42) == ["audit:42", "dashboard:42"]
    return false unless subject.unsubscribe("dashboard")
    return false if subject.unsubscribe("dashboard")

    subject.publish(43) == ["audit:43"]
  end
end
