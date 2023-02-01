#!/usr/bin/bash

# Build the demo
cp index.html pkg;
wasm-pack build --target web;
cp index.html pkg;