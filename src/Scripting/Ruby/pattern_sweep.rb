# frozen_string_literal: true

# Ruby Design Pattern sweep for the 39 post-Chain-of-Responsibility catalog cells.
# Each check is intentionally small, executable, idiomatic, and isolated from
# Genkidama production/course code.

require 'monitor'

module PatternSweep
  module_function

  def assert(condition, message = 'assertion failed')
    raise message unless condition
  end

  def command
    balance = 100
    commands = [-> { balance += 50 }, -> { balance -= 20 }]
    commands.each(&:call)
    assert(balance == 130)
  end

  def interpreter
    env = { x: 4 }
    expr = [:add, [:var, :x], [:lit, 3]]
    evaluate = nil
    evaluate = lambda do |node|
      kind, *args = node
      case kind
      when :lit then args[0]
      when :var then env.fetch(args[0])
      when :add then evaluate.call(args[0]) + evaluate.call(args[1])
      else raise "unknown expression: #{kind}"
      end
    end
    assert(evaluate.call(expr) == 7)
  end

  def iterator
    countdown = Enumerator.new do |yielder|
      3.downto(1) { |n| yielder << n }
    end
    assert(countdown.to_a == [3, 2, 1])
  end

  def mediator
    events = []
    mediator = ->(sender, message) { events << [sender, message] }
    mediator.call(:checkout, :paid)
    assert(events == [[:checkout, :paid]])
  end

  def memento
    state = { text: 'draft' }
    snapshot = state.dup.freeze
    state[:text] = 'edited'
    state.replace(snapshot)
    assert(state[:text] == 'draft')
  end

  def observer
    seen = []
    subscribers = [->(event) { seen << event }]
    subscribers.each { |subscriber| subscriber.call(:changed) }
    assert(seen == [:changed])
  end

  def state
    door = Object.new
    door.instance_variable_set(:@state, :closed)
    door.define_singleton_method(:toggle) do
      @state = @state == :closed ? :open : :closed
    end
    door.toggle
    assert(door.instance_variable_get(:@state) == :open)
  end

  def strategy
    choose = ->(values, strategy) { strategy.call(values) }
    assert(choose.call([3, 1, 2], ->(items) { items.min }) == 1)
    assert(choose.call([3, 1, 2], ->(items) { items.max }) == 3)
  end

  def template_method
    report = Class.new do
      def render = "<#{body}>"
      def body = raise NotImplementedError
    end
    sales = Class.new(report) { def body = 'sales' }
    assert(sales.new.render == '<sales>')
  end

  def visitor
    number = Struct.new(:value)
    visitor = ->(node) { node.value * 2 }
    assert(visitor.call(number.new(5)) == 10)
  end

  def mvc
    model = { count: 0 }
    controller = -> { model[:count] += 1 }
    view = -> { "count=#{model[:count]}" }
    controller.call
    assert(view.call == 'count=1')
  end

  def mvvm
    model = { first: 'Ada', last: 'Lovelace' }
    view_model = -> { { display_name: "#{model[:first]} #{model[:last]}" } }
    assert(view_model.call[:display_name] == 'Ada Lovelace')
  end

  def microkernel
    plugins = { upper: ->(text) { text.upcase } }
    assert(plugins.fetch(:upper).call('plugin') == 'PLUGIN')
  end

  def microservices
    inventory = ->(sku) { { sku:, available: true } }
    order = ->(sku) { inventory.call(sku)[:available] }
    assert(order.call('A-1'))
  end

  def enterprise_adapter
    legacy = ->(cents) { cents }
    adapter = ->(amount) { legacy.call((amount * 100).round) }
    assert(adapter.call(12.34) == 1234)
  end

  def enterprise_bridge
    channel = Struct.new(:sender) do
      def notify(text) = sender.call(text)
    end
    assert(channel.new(->(text) { "sms:#{text}" }).notify('ok') == 'sms:ok')
  end

  def enterprise_facade
    stock = -> { true }
    charge = -> { :paid }
    checkout = -> { stock.call ? charge.call : :sold_out }
    assert(checkout.call == :paid)
  end

  def broker
    handlers = { price: ->(_sku) { 9 } }
    request = ->(topic, payload) { handlers.fetch(topic).call(payload) }
    assert(request.call(:price, 'A') == 9)
  end

  def message_bus
    bus = Hash.new { |hash, key| hash[key] = [] }
    seen = []
    bus[:paid] << ->(value) { seen << value }
    bus[:paid].each { |handler| handler.call(42) }
    assert(seen == [42])
  end

  def service_locator
    services = { clock: -> { '12:00' } }
    assert(services.fetch(:clock).call == '12:00')
  end

  def active_object
    mailbox = Queue.new
    state = []
    mailbox << -> { state << :done }
    mailbox.pop.call
    assert(state == [:done])
  end

  def monitor_object
    counter = Class.new do
      include MonitorMixin
      attr_reader :value

      def initialize
        super
        @value = 0
      end

      def increment
        synchronize { @value += 1 }
      end
    end.new
    threads = Array.new(4) { Thread.new { counter.increment } }
    threads.each(&:join)
    assert(counter.value == 4)
  end

  def half_sync_half_async
    incoming = Queue.new
    %w[a b].each { |value| incoming << value }
    completed = []
    completed << incoming.pop.upcase until incoming.empty?
    assert(completed == %w[A B])
  end

  def leader_followers
    events = %w[one two]
    workers = %i[leader follower].each
    handled = events.map { |event| [workers.next, event] }
    assert(handled == [[:leader, 'one'], [:follower, 'two']])
  end

  def client_server
    server = ->(request) { { echo: request } }
    client = ->(value) { server.call(value)[:echo] }
    assert(client.call('ping') == 'ping')
  end

  def peer_to_peer
    peers = { a: [], b: [] }
    send_message = ->(source, target, message) { peers.fetch(target) << [source, message] }
    send_message.call(:a, :b, 'hello')
    assert(peers[:b] == [[:a, 'hello']])
  end

  def publish_subscribe
    topics = Hash.new { |hash, key| hash[key] = [] }
    received = []
    topics[:news] << ->(value) { received << value }
    topics[:news].each { |subscriber| subscriber.call('v1') }
    assert(received == ['v1'])
  end

  def distributed_proxy
    remote = ->(id) { { id:, name: 'Ada' } }
    proxy = Struct.new(:id, :remote) do
      def name = remote.call(id)[:name]
    end
    assert(proxy.new(7, remote).name == 'Ada')
  end

  def presentation_abstraction_control
    state = { value: 1 }
    control = ->(delta) { state[:value] += delta }
    presentation = -> { state[:value].to_s }
    control.call(2)
    assert(presentation.call == '3')
  end

  def model_view_presenter
    model = { name: 'Ada' }
    view = {}
    presenter = -> { view[:text] = model[:name].upcase }
    presenter.call
    assert(view[:text] == 'ADA')
  end

  def document_view
    document = { title: 'One' }
    view_a = -> { document[:title] }
    view_b = -> { document[:title].upcase }
    assert([view_a.call, view_b.call] == ['One', 'ONE'])
  end

  def active_record
    table = {}
    user_class = Class.new do
      define_method(:initialize) { |id, name, storage| @id, @name, @storage = id, name, storage }
      define_method(:save) { @storage[@id] = { name: @name } }
    end
    user_class.new(1, 'Ada', table).save
    assert(table[1][:name] == 'Ada')
  end

  def data_mapper
    table = { 1 => { name: 'Ada' } }
    user = Struct.new(:name)
    mapper = ->(row) { user.new(row[:name]) }
    assert(mapper.call(table[1]).name == 'Ada')
  end

  def unit_of_work
    pending = [{ id: 1 }]
    database = []
    database.concat(pending)
    pending.clear
    assert(database == [{ id: 1 }] && pending.empty?)
  end

  def repository
    data = { 1 => { name: 'Ada' } }
    users = Object.new
    users.define_singleton_method(:get) { |id| data.fetch(id) }
    assert(users.get(1)[:name] == 'Ada')
  end

  def dependency_injection
    greeter = Struct.new(:clock) do
      def greet = "hello@#{clock.call}"
    end
    assert(greeter.new(-> { 'noon' }).greet == 'hello@noon')
  end

  def lazy_initialization
    calls = 0
    lazy = Object.new
    lazy.define_singleton_method(:value) do
      unless instance_variable_defined?(:@value)
        calls += 1
        @value = Object.new
      end
      @value
    end
    first = lazy.value
    second = lazy.value
    assert(first.equal?(second) && calls == 1)
  end

  def object_pool
    pool = [{ id: 1 }]
    item = pool.shift
    pool << item
    assert(pool.first.equal?(item))
  end

  def null_object
    null_logger = Object.new
    null_logger.define_singleton_method(:log) { |_message| nil }
    service = Struct.new(:logger) do
      def run
        logger.log('run')
        :ok
      end
    end
    assert(service.new(null_logger).run == :ok)
  end

  CHECKS = [
    :command, :interpreter, :iterator, :mediator, :memento, :observer, :state,
    :strategy, :template_method, :visitor, :mvc, :mvvm, :microkernel,
    :microservices, :enterprise_adapter, :enterprise_bridge, :enterprise_facade,
    :broker, :message_bus, :service_locator, :active_object, :monitor_object,
    :half_sync_half_async, :leader_followers, :client_server, :peer_to_peer,
    :publish_subscribe, :distributed_proxy, :presentation_abstraction_control,
    :model_view_presenter, :document_view, :active_record, :data_mapper,
    :unit_of_work, :repository, :dependency_injection, :lazy_initialization,
    :object_pool, :null_object
  ].freeze

  def run
    assert(CHECKS.length == 39)
    CHECKS.each { |check| public_send(check) }
    puts 'ruby-pattern-sweep: 39/39 passed'
  end
end

PatternSweep.run if $PROGRAM_NAME == __FILE__
