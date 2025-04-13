#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell"]
set shell := ["bash", "-cu"]

_default:
    @just --list -u

build:
    cd vscode; pnpm.cmd build; cd ../server; cargo build

ready:
    just fmt
    just check
    just lint
    git status

fix:
    cargo clippy --fix --allow-staged --no-deps
    just fmt
    git status

check:
    cargo check --workspace --all-features --all-targets --locked

lint:
    cargo clippy --workspace --all-targets --all-features -- --deny warnings

fmt:
    cargo shear --fix
    cargo fmt --all
    dprint fmt
