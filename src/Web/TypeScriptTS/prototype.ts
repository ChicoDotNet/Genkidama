interface Prototype<T> {
  clone(): T;
}

class ServiceProfile implements Prototype<ServiceProfile> {
  constructor(
    public name: string,
    public readonly features: string[],
  ) {}

  clone(): ServiceProfile {
    return new ServiceProfile(this.name, [...this.features]);
  }

  describe(): string {
    return `${this.name}: ${this.features.join(',')}`;
  }
}

const original = new ServiceProfile('orders', ['metrics']);
const canary = original.clone();

canary.name = 'orders-canary';
canary.features.push('tracing');

console.log(`original=${original.describe()}`);
console.log(`clone=${canary.describe()}`);
