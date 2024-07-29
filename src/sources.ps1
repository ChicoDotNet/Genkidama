# Define the base path
$basePath = "c:\Code\PatternsWiki\src"

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

# Create the pattern folders
foreach ($pattern in $patterns) {
    $patternPath = "$basePath\$pattern"
    New-Item -Path $patternPath -ItemType Directory -Force
}

Write-Output "Pattern folders created successfully at $basePath."
