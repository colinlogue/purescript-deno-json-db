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
npm test
```

## Project Structure

- `src/` - Source code
  - `JsonDb/` - Database API and implementation
    - `Database.purs` - Core database functionality (file operations, JSON handling)
    - `Server.purs` - HTTP server for database access
- `test/` - Test code
  - `Test/JsonDb/Database.purs` - Tests for the database API and operations
  - `Test/JsonDb/Server.purs` - Tests for the server component

## License

MIT
