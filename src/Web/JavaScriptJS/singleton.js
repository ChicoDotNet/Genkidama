class ProcessRegistry {
  constructor() {
    if (ProcessRegistry.instance) return ProcessRegistry.instance;
    this.count = 0;
    ProcessRegistry.instance = this;
  }

  increment() {
    this.count += 1;
  }
}

const first = new ProcessRegistry();
const second = new ProcessRegistry();
first.increment();
console.log(`same=${first === second}`);
console.log(`count=${second.count}`);
