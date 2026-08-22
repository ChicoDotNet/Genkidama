class ProcessRegistry {
  private static readonly shared = new ProcessRegistry();
  private countValue = 0;

  private constructor() {}

  static instance(): ProcessRegistry {
    return ProcessRegistry.shared;
  }

  increment(): void {
    this.countValue += 1;
  }

  count(): number {
    return this.countValue;
  }
}

const first = ProcessRegistry.instance();
const second = ProcessRegistry.instance();
first.increment();
console.log(`same=${first === second}`);
console.log(`count=${second.count()}`);
