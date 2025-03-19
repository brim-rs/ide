import { defineConfig } from "tsup";
import fs from "fs";
import path from "path";

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");

export default defineConfig({
    entry: ["client/extension.ts"],
    format: ["cjs"],
    minify: production,
    sourcemap: !production,
    outDir: "dist",
    dts: false,
    clean: true,
    watch,
    platform: "node",
    external: ["vscode", "vscode-languageclient"],
});
