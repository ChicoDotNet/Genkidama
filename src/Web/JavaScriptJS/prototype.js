'use strict';

const serviceProfilePrototype = {
  clone() {
    return Object.assign(Object.create(Object.getPrototypeOf(this)), this, {
      features: [...this.features],
    });
  },

  describe() {
    return `${this.name}: ${this.features.join(',')}`;
  },
};

function createServiceProfile(name, features) {
  return Object.assign(Object.create(serviceProfilePrototype), {
    name,
    features: [...features],
  });
}

const original = createServiceProfile('orders', ['metrics']);
const canary = original.clone();

canary.name = 'orders-canary';
canary.features.push('tracing');

console.log(`original=${original.describe()}`);
console.log(`clone=${canary.describe()}`);
