class Pipe {
  constructor() {
    this.listeners = [];
  }
  onChange(value) {}
  connect(to) {
    to.listeners.push((value) => this.onChange(value));
    return this;
  }
}

class Input extends Pipe {
  constructor() {
    super();
    const elem = document.createElement("input");
    elem.type = "text";
    document.body.appendChild(elem);
    elem.addEventListener("input", (e) => {
      this.listeners.forEach((listener) => listener(e.target.value));
    });
  }
}

class Output extends Pipe {
  constructor() {
    super();
    const elem = document.createElement("div");
    document.body.appendChild(elem);
    this.elem = elem;
  }
  onChange(value) {
    this.elem.textContent = value;
  }
}
class FromFunc extends Pipe {
  constructor(fn) {
    super();
    this.fn = fn;
  }
  onChange(value) {
    const calculated = this.fn(value);
    this.listeners.forEach((listener) => listener(calculated));
  }
}
const input = new Input();
const output = new Output();
