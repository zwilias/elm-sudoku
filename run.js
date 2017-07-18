const Elm = require('./elm.js');
Error.stackTraceLimit = Infinity;
const app = Elm.App.worker();

app.ports.emit.subscribe((msg) => {
  console.log(msg);
});
