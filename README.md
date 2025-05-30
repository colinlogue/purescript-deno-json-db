# PureScript Deno JSON DB

A simple JSON database for Deno, implemented in PureScript.

## Installation

```bash
npm i
```

## Building

```bash
npm run build
```

## Running the example

```bash
npm run main
```

## Running Tests

```bash
# Run all tests
npm run test

# Or use the convenience script
./run-tests.sh
```

## Project Structure

- `src/` - Source code
  - `JsonDb/` - High-level database API
  - `JsonDatabase/` - Low-level database implementation
- `test/` - Test code
  - `Test/JsonDb/Database.purs` - Tests for the high-level database API
  - `Test/JsonDb/Server.purs` - Tests for the server component
  - `Test/JsonDatabase.purs` - Tests for the low-level database implementation

## License

MIT
