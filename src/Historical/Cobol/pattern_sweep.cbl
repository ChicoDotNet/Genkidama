>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. PATTERN-SWEEP.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 PASS-COUNT PIC 99 VALUE 0.
01 BALANCE PIC S9(9) COMP-5 VALUE 0.
01 VALUE-N PIC S9(9) COMP-5 VALUE 0.
01 STOCK PIC S9(9) COMP-5 VALUE 0.
01 BUILDS PIC S9(9) COMP-5 VALUE 0.
01 AUX-N PIC S9(9) COMP-5 VALUE 0.
01 INDEX-N PIC 9 COMP-5 VALUE 0.
01 TEXT-A PIC X(96) VALUE SPACES.
01 TEXT-B PIC X(96) VALUE SPACES.
01 TEXT-C PIC X(96) VALUE SPACES.
01 TEXT-D PIC X(96) VALUE SPACES.
01 VALUE-TABLE.
   05 VALUE-ITEM PIC S9(9) COMP-5 OCCURS 3 TIMES.
01 SEEN-TABLE.
   05 SEEN-ITEM PIC S9(9) COMP-5 OCCURS 3 TIMES.
PROCEDURE DIVISION.
MAIN.
    PERFORM COMMAND-PATTERN
    PERFORM INTERPRETER-PATTERN
    PERFORM ITERATOR-PATTERN
    PERFORM MEDIATOR-PATTERN
    PERFORM MEMENTO-PATTERN
    PERFORM OBSERVER-PATTERN
    PERFORM STATE-PATTERN
    PERFORM STRATEGY-PATTERN
    PERFORM TEMPLATE-METHOD-PATTERN
    PERFORM VISITOR-PATTERN
    PERFORM MVC-PATTERN
    PERFORM MVVM-PATTERN
    PERFORM MICROKERNEL-PATTERN
    PERFORM MICROSERVICES-PATTERN
    PERFORM ENTERPRISE-ADAPTER-PATTERN
    PERFORM ENTERPRISE-BRIDGE-PATTERN
    PERFORM ENTERPRISE-FACADE-PATTERN
    PERFORM BROKER-PATTERN
    PERFORM MESSAGE-BUS-PATTERN
    PERFORM SERVICE-LOCATOR-PATTERN
    PERFORM ACTIVE-OBJECT-PATTERN
    PERFORM MONITOR-OBJECT-PATTERN
    PERFORM HALF-SYNC-HALF-ASYNC-PATTERN
    PERFORM LEADER-FOLLOWERS-PATTERN
    PERFORM CLIENT-SERVER-PATTERN
    PERFORM PEER-TO-PEER-PATTERN
    PERFORM PUBLISH-SUBSCRIBE-PATTERN
    PERFORM DISTRIBUTED-PROXY-PATTERN
    PERFORM PAC-PATTERN
    PERFORM MVP-PATTERN
    PERFORM DOCUMENT-VIEW-PATTERN
    PERFORM ACTIVE-RECORD-PATTERN
    PERFORM DATA-MAPPER-PATTERN
    PERFORM UNIT-OF-WORK-PATTERN
    PERFORM REPOSITORY-PATTERN
    PERFORM DEPENDENCY-INJECTION-PATTERN
    PERFORM LAZY-INITIALIZATION-PATTERN
    PERFORM OBJECT-POOL-PATTERN
    PERFORM NULL-OBJECT-PATTERN
    IF PASS-COUNT NOT = 39
        DISPLAY "expected 39 cases" UPON STDERR
        STOP RUN RETURNING 1
    END-IF
    DISPLAY "COBOL pattern sweep: 39/39 examples passed"
    STOP RUN.
PASS.
    ADD 1 TO PASS-COUNT.
FAIL.
    DISPLAY "pattern assertion failed" UPON STDERR
    STOP RUN RETURNING 1.
COPY "src/Historical/Cobol/patterns/command_pattern.cpy".
COPY "src/Historical/Cobol/patterns/interpreter_pattern.cpy".
COPY "src/Historical/Cobol/patterns/iterator_pattern.cpy".
COPY "src/Historical/Cobol/patterns/mediator_pattern.cpy".
COPY "src/Historical/Cobol/patterns/memento_pattern.cpy".
COPY "src/Historical/Cobol/patterns/observer_pattern.cpy".
COPY "src/Historical/Cobol/patterns/state_pattern.cpy".
COPY "src/Historical/Cobol/patterns/strategy_pattern.cpy".
COPY "src/Historical/Cobol/patterns/template_method_pattern.cpy".
COPY "src/Historical/Cobol/patterns/visitor_pattern.cpy".
COPY "src/Historical/Cobol/patterns/mvc_pattern.cpy".
COPY "src/Historical/Cobol/patterns/mvvm_pattern.cpy".
COPY "src/Historical/Cobol/patterns/microkernel_pattern.cpy".
COPY "src/Historical/Cobol/patterns/microservices_pattern.cpy".
COPY "src/Historical/Cobol/patterns/enterprise_adapter_pattern.cpy".
COPY "src/Historical/Cobol/patterns/enterprise_bridge_pattern.cpy".
COPY "src/Historical/Cobol/patterns/enterprise_facade_pattern.cpy".
COPY "src/Historical/Cobol/patterns/broker_pattern.cpy".
COPY "src/Historical/Cobol/patterns/message_bus_pattern.cpy".
COPY "src/Historical/Cobol/patterns/service_locator_pattern.cpy".
COPY "src/Historical/Cobol/patterns/active_object_pattern.cpy".
COPY "src/Historical/Cobol/patterns/monitor_object_pattern.cpy".
COPY "src/Historical/Cobol/patterns/half_sync_half_async_pattern.cpy".
COPY "src/Historical/Cobol/patterns/leader_followers_pattern.cpy".
COPY "src/Historical/Cobol/patterns/client_server_pattern.cpy".
COPY "src/Historical/Cobol/patterns/peer_to_peer_pattern.cpy".
COPY "src/Historical/Cobol/patterns/publish_subscribe_pattern.cpy".
COPY "src/Historical/Cobol/patterns/distributed_proxy_pattern.cpy".
COPY "src/Historical/Cobol/patterns/pac_pattern.cpy".
COPY "src/Historical/Cobol/patterns/mvp_pattern.cpy".
COPY "src/Historical/Cobol/patterns/document_view_pattern.cpy".
COPY "src/Historical/Cobol/patterns/active_record_pattern.cpy".
COPY "src/Historical/Cobol/patterns/data_mapper_pattern.cpy".
COPY "src/Historical/Cobol/patterns/unit_of_work_pattern.cpy".
COPY "src/Historical/Cobol/patterns/repository_pattern.cpy".
COPY "src/Historical/Cobol/patterns/dependency_injection_pattern.cpy".
COPY "src/Historical/Cobol/patterns/lazy_initialization_pattern.cpy".
COPY "src/Historical/Cobol/patterns/object_pool_pattern.cpy".
COPY "src/Historical/Cobol/patterns/null_object_pattern.cpy".
