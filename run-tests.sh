#!/bin/bash

echo "Installing dependencies..."
npm i

echo "Building the project..."
npm run build

echo "Running tests..."
npm run test
