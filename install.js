const tar = require("tar");
const request = require("request");

const url = "https://github.com/vmware/differential-datalog/releases/download/v0.17.2/ddlog-v0.17.2-20200430164611-linux.tar.gz";

request({ method: "GET", url, gzip: true })
    .pipe(tar.x());
