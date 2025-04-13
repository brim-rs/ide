#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell"]
set shell := ["bash", "-cu"]

_default:
    @just --list -u

build:
    cd vscode; pnpm.cmd build; cd ../server; cargo build