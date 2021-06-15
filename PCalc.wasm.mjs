import fs from "fs";
export default fs.promises.readFile("PCalc.wasm").then(bufferSource => WebAssembly.compile(bufferSource));
