importScripts(/*'../logos-cla-ui-jsdeps.js', */'../logos-cla-ui-fastopt.js');

//generic loader. first event loads an entry point
onmessage = function (oEvent) {
  eval(oEvent.data)
};