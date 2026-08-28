// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

import "./patterns/Command.sol";
import "./patterns/Interpreter.sol";
import "./patterns/Iterator.sol";
import "./patterns/Mediator.sol";
import "./patterns/Memento.sol";
import "./patterns/Observer.sol";
import "./patterns/State.sol";
import "./patterns/Strategy.sol";
import "./patterns/TemplateMethod.sol";
import "./patterns/Visitor.sol";
import "./patterns/MVC.sol";
import "./patterns/MVVM.sol";
import "./patterns/Microkernel.sol";
import "./patterns/Microservices.sol";
import "./patterns/EnterpriseAdapter.sol";
import "./patterns/EnterpriseBridge.sol";
import "./patterns/EnterpriseFacade.sol";
import "./patterns/Broker.sol";
import "./patterns/MessageBus.sol";
import "./patterns/ServiceLocator.sol";
import "./patterns/ActiveObject.sol";
import "./patterns/MonitorObject.sol";
import "./patterns/HalfSyncHalfAsync.sol";
import "./patterns/LeaderFollowers.sol";
import "./patterns/ClientServer.sol";
import "./patterns/PeerToPeer.sol";
import "./patterns/PublishSubscribe.sol";
import "./patterns/DistributedProxy.sol";
import "./patterns/PresentationAbstractionControl.sol";
import "./patterns/ModelViewPresenter.sol";
import "./patterns/DocumentView.sol";
import "./patterns/ActiveRecord.sol";
import "./patterns/DataMapper.sol";
import "./patterns/UnitOfWork.sol";
import "./patterns/Repository.sol";
import "./patterns/DependencyInjection.sol";
import "./patterns/LazyInitialization.sol";
import "./patterns/ObjectPool.sol";
import "./patterns/NullObject.sol";

contract PatternSweep {
    function runAll() external pure returns(uint passed) {
        bool[39] memory checks;
        checks[0] = CommandPattern.run();
        checks[1] = InterpreterPattern.run();
        checks[2] = IteratorPattern.run();
        checks[3] = MediatorPattern.run();
        checks[4] = MementoPattern.run();
        checks[5] = ObserverPattern.run();
        checks[6] = StatePattern.run();
        checks[7] = StrategyPattern.run();
        checks[8] = TemplateMethodPattern.run();
        checks[9] = VisitorPattern.run();
        checks[10] = MvcPattern.run();
        checks[11] = MvvmPattern.run();
        checks[12] = MicrokernelPattern.run();
        checks[13] = MicroservicesPattern.run();
        checks[14] = EnterpriseAdapterPattern.run();
        checks[15] = EnterpriseBridgePattern.run();
        checks[16] = EnterpriseFacadePattern.run();
        checks[17] = BrokerPattern.run();
        checks[18] = MessageBusPattern.run();
        checks[19] = ServiceLocatorPattern.run();
        checks[20] = ActiveObjectPattern.run();
        checks[21] = MonitorObjectPattern.run();
        checks[22] = HalfSyncHalfAsyncPattern.run();
        checks[23] = LeaderFollowersPattern.run();
        checks[24] = ClientServerPattern.run();
        checks[25] = PeerToPeerPattern.run();
        checks[26] = PublishSubscribePattern.run();
        checks[27] = DistributedProxyPattern.run();
        checks[28] = PresentationAbstractionControlPattern.run();
        checks[29] = ModelViewPresenterPattern.run();
        checks[30] = DocumentViewPattern.run();
        checks[31] = ActiveRecordPattern.run();
        checks[32] = DataMapperPattern.run();
        checks[33] = UnitOfWorkPattern.run();
        checks[34] = RepositoryPattern.run();
        checks[35] = DependencyInjectionPattern.run();
        checks[36] = LazyInitializationPattern.run();
        checks[37] = ObjectPoolPattern.run();
        checks[38] = NullObjectPattern.run();
        for (uint256 i; i < checks.length; i++) { require(checks[i], "pattern failed"); passed++; }
        require(passed == 39, "expected 39 cases");
    }
}
