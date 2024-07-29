# Define the base path
$basePath = "c:\Code\PatternsWiki\wiki"

# Define the list of patterns
$patterns = @(
    "AbstractFactory",
    "Builder",
    "FactoryMethod",
    "Prototype",
    "Singleton",
    "Adapter",
    "Bridge",
    "Composite",
    "Decorator",
    "Facade",
    "Flyweight",
    "Proxy",
    "ChainOfResponsibility",
    "Command",
    "Interpreter",
    "Iterator",
    "Mediator",
    "Memento",
    "Observer",
    "State",
    "Strategy",
    "TemplateMethod",
    "Visitor",
    "MVC",
    "MVVM",
    "Microkernel",
    "Microservices",
    "AdapterEnterpriseIntegration",
    "BridgeEnterpriseIntegration",
    "FacadeEnterpriseIntegration",
    "Broker",
    "MessageBus",
    "ServiceLocator",
    "ActiveObject",
    "MonitorObject",
    "HalfSyncHalfAsync",
    "LeaderFollowers",
    "ClientServer",
    "PeerToPeer",
    "PublishSubscribe",
    "ProxyDistribuido",
    "PresentationAbstractionControl",
    "ModelViewPresenter",
    "DocumentView",
    "ActiveRecord",
    "DataMapper",
    "UnitOfWork",
    "DependencyInjection",
    "LazyInitialization",
    "ObjectPool",
    "NullObject",
    "Repository"
)

# Create the markdown files for each pattern
foreach ($pattern in $patterns) {
    $filePath = "$basePath\$pattern.md"
    New-Item -Path $filePath -ItemType File -Force
}

Write-Output "Markdown files created successfully at $basePath."
