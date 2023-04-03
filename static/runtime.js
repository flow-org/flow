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
  constructor(id) {
    super();
    this.id = id;
    const elemId = `input-${id}`;
    const existed = document.getElementById(elemId);
    if (existed) {
      existed.addEventListener("input", (e) => {
        this.listeners.forEach((listener) => listener(e.target.value));
      });
    } else {
      const wrapper = document.createElement("p");
      wrapper.textContent = id === 'default' ? '' : `${id}: `;
      const elem = document.createElement("input");
      elem.type = "text";
      elem.id = elemId;
      wrapper.appendChild(elem);
      document.body.appendChild(wrapper);
      elem.addEventListener("input", (e) => {
        this.listeners.forEach((listener) => listener(e.target.value));
      });
    }
  }
}

class Output extends Pipe {
  constructor(id) {
    super();
    this.id = id;
    const elemId = `output-${id}`;
    const existed = document.getElementById(elemId);
    if (existed) {
      this.elem = existed;
    } else {
      const elem = document.createElement("div");
      elem.id = elemId;
      document.body.appendChild(elem);
      this.elem = elem;
    }
  }
  onChange(value) {
    this.elem.textContent = this.id === 'default' ? value : `${this.id}: ${value}`;
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
const input = (id = "default") => new Input(id);
const output = (id = "default") => new Output(id);
