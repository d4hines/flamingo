const argv = require("yargs").argv;
const addon = require('../native');

const executablePath = argv.$0;
const almFile = argv._[0];

console.log(almFile);

console.log(addon.hello());
