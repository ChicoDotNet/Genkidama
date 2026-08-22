class PlainMessage {
  render() {
    return "alert";
  }
}

class ComponentDecorator {
  constructor(inner) {
    this.inner = inner;
  }
}

class AuditDecorator extends ComponentDecorator {
  render() {
    return `audit(${this.inner.render()})`;
  }
}

class EncryptDecorator extends ComponentDecorator {
  render() {
    return `enc(${this.inner.render()})`;
  }
}

const component = new PlainMessage();
console.log(`base=${component.render()}`);
console.log(`audit=${new AuditDecorator(component).render()}`);
console.log(`encrypted=${new EncryptDecorator(component).render()}`);
console.log(`stacked=${new AuditDecorator(new EncryptDecorator(component)).render()}`);
