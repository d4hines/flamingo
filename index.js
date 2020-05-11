const path = require("path");
const { readFileSync, renameSync } = require("fs");

const ncp = require("ncp");
const argv = require("yargs").argv;
const { execSync } = require("child_process");

const flamingoRoot = path.normalize(path.join(path.dirname(argv.$0)));
console.log("flamingoRoot", flamingoRoot);
const almFile = argv._[0];
const almModule = almFile.replace(".alm", "");
console.log(`Compiling ALM module ${almFile}`);

const compilerPath = path.join(flamingoRoot, "compiler", "target", "debug", "compiler.exe");
// Compile the ALM module
execSync(`${compilerPath} ${almFile}`, { stdio: 'inherit' });

// Copy the template to the CWD.
const templatePath = path.join(flamingoRoot, "template")
// Spit out the compiled DDLog file.

// Copy the Rust template and compile the DDLog into Rust.
let copied;
ncp(templatePath, `${almModule}_alm`, () => {
    // The callback fires three times... o_O
    if (copied) return;
    copied = true;

    // Compile the DDLog.
    // Until DDLog works on windows, we have to shell out to WSL.
    const posixFlamingoRoot = path.posix.normalize(path.posix.join(path.posix.dirname(argv.$0), "../.."));
    const ddlogBin = path.posix.join(posixFlamingoRoot, "ddlog", "bin", "ddlog").replace("/", "//");
    const ddlogLib = path.posix.join(posixFlamingoRoot, "ddlog", "lib");
    const ddlogFile = path.posix.join(`${almModule}_alm`, "native", "logic.dl");
    
    renameSync(`${almModule}.dl`, ddlogFile);

    execSync(`wsl ${ddlogBin} -i ${ddlogFile} -L ${ddlogLib}`, { stdio: 'inherit' });
    console.log(`ALM module ${almModule} built successfully!`);
    const message = `
            .-.
            ((\`-)
             \\\\
              \\\\
       .="""=._))
      /  .,   .'
     /__(,_.-'
    \`    /|
        /_|__
          | \`))
          |
         -"==
`;
    if (!argv.nobanner) console.log(message);
});
