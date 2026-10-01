# OpenCL Accelerated/Heterogeneous pattern pilot

> **Dimension:** Accelerated/Heterogeneous  
> **Target:** OpenCL  
> **Language denominator impact:** none  
> **Specification:** OpenCL 3.1.2  
> **CI execution profile:** OpenCL ICD + PoCL CPU on Ubuntu 24.04  
> **Pilot:** 12 contracted / 12 implemented

## Why OpenCL is not language target #54

OpenCL combines a host API with OpenCL C kernels and exists to coordinate heterogeneous execution across devices. Genkidama therefore models it as an orthogonal execution dimension rather than duplicating the general-purpose language matrix.

The pilot asks a narrower question: **where does a pattern teach something materially useful about the host/device, kernel, queue, program or resource boundary?**

## RED/GREEN evidence

The RED head installed a real CPU OpenCL runtime and all 12 contracts failed only because each host/header/kernel implementation triple was intentionally absent.

The first GREEN head then executed **12/12 OpenCL cells successfully** through the real OpenCL API and PoCL device path. The job failed only after that target gate because the historical pre-CoR compatibility runner attempted to impose a legacy Abstract Factory source-word marker on the new dimension. The reconciliation head removes that accidental coupling while preserving census and test-existence checks.

## Pilot ledger

| Family | Pattern | Host source | Device kernel | Contract |
|---|---|---|---|---|
| Creational | Abstract Factory | [host](../../src/Accelerated/OpenCL/patterns/abstract_factory.c) | [kernel](../../src/Accelerated/OpenCL/kernels/abstract_factory.cl) | [test](../../src/Accelerated/OpenCL/tests/test_abstract_factory.c) |
| Structural | Adapter | [host](../../src/Accelerated/OpenCL/patterns/adapter.c) | [kernel](../../src/Accelerated/OpenCL/kernels/adapter.cl) | [test](../../src/Accelerated/OpenCL/tests/test_adapter.c) |
| Structural | Bridge | [host](../../src/Accelerated/OpenCL/patterns/bridge.c) | [kernel](../../src/Accelerated/OpenCL/kernels/bridge.cl) | [test](../../src/Accelerated/OpenCL/tests/test_bridge.c) |
| Creational | Builder | [host](../../src/Accelerated/OpenCL/patterns/builder.c) | [kernel](../../src/Accelerated/OpenCL/kernels/builder.cl) | [test](../../src/Accelerated/OpenCL/tests/test_builder.c) |
| Behavioral | Command | [host](../../src/Accelerated/OpenCL/patterns/command.c) | [kernel](../../src/Accelerated/OpenCL/kernels/command.cl) | [test](../../src/Accelerated/OpenCL/tests/test_command.c) |
| Structural | Facade | [host](../../src/Accelerated/OpenCL/patterns/facade.c) | [kernel](../../src/Accelerated/OpenCL/kernels/facade.cl) | [test](../../src/Accelerated/OpenCL/tests/test_facade.c) |
| Structural | Flyweight | [host](../../src/Accelerated/OpenCL/patterns/flyweight.c) | [kernel](../../src/Accelerated/OpenCL/kernels/flyweight.cl) | [test](../../src/Accelerated/OpenCL/tests/test_flyweight.c) |
| Structural | Proxy | [host](../../src/Accelerated/OpenCL/patterns/proxy.c) | [kernel](../../src/Accelerated/OpenCL/kernels/proxy.cl) | [test](../../src/Accelerated/OpenCL/tests/test_proxy.c) |
| Behavioral | Strategy | [host](../../src/Accelerated/OpenCL/patterns/strategy.c) | [kernel](../../src/Accelerated/OpenCL/kernels/strategy.cl) | [test](../../src/Accelerated/OpenCL/tests/test_strategy.c) |
| Concurrency | Active Object | [host](../../src/Accelerated/OpenCL/patterns/active_object.c) | [kernel](../../src/Accelerated/OpenCL/kernels/active_object.cl) | [test](../../src/Accelerated/OpenCL/tests/test_active_object.c) |
| Additional | Lazy Initialization | [host](../../src/Accelerated/OpenCL/patterns/lazy_initialization.c) | [kernel](../../src/Accelerated/OpenCL/kernels/lazy_initialization.cl) | [test](../../src/Accelerated/OpenCL/tests/test_lazy_initialization.c) |
| Additional | Object Pool | [host](../../src/Accelerated/OpenCL/patterns/object_pool.c) | [kernel](../../src/Accelerated/OpenCL/kernels/object_pool.cl) | [test](../../src/Accelerated/OpenCL/tests/test_object_pool.c) |

## Portability boundary

The shared host harness intentionally uses a conservative OpenCL host-API subset while the kernels remain portable OpenCL C. CI is CPU-backed so GitHub-hosted runners can exercise program creation, kernel compilation, enqueue and result readback without pretending GPU hardware exists.

Future CUDA or SYCL pilots, if approved, should join this dimension and be judged on genuinely accelerator-specific value rather than copying all 52 language examples mechanically.
