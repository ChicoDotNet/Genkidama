interface Component {
  render(): string;
}

class PlainMessage implements Component {
  render(): string {
    return "alert";
  }
}

abstract class ComponentDecorator implements Component {
  protected constructor(protected readonly inner: Component) {}
  abstract render(): string;
}

class AuditDecorator extends ComponentDecorator {
  render(): string {
    return `audit(${this.inner.render()})`;
  }
}

class EncryptDecorator extends ComponentDecorator {
  render(): string {
    return `enc(${this.inner.render()})`;
  }
}

const component: Component = new PlainMessage();
console.log(`base=${component.render()}`);
console.log(`audit=${new AuditDecorator(component).render()}`);
console.log(`encrypted=${new EncryptDecorator(component).render()}`);
console.log(`stacked=${new AuditDecorator(new EncryptDecorator(component)).render()}`);
