# PureScript Deno JSON DB Project Instructions

## Project Setup and Commands

- Install dependencies with `npm i`
- Build the project with `npm run build`
- Run the project with `npm run main`
- Run tests with `npm run test`
- Always run the correct version of spago with `npx spago`

## Project Structure

- `src/` - Source code
  - `JsonDb/` - Database API and implementation
    - `Database.purs` - Core database functionality (file operations, JSON handling)
    - `Server.purs` - HTTP server for database access
- `test/` - Test code
  - `Test/JsonDb/Database.purs` - Tests for the database API and operations
  - `Test/JsonDb/Server.purs` - Tests for the server component

## Development Guidelines

### PureScript and TypeScript

- Source code for all PureScript modules that are installed can be found under the appropriate package in the `spago/p` directory
- Foreign modules should be written in TypeScript. Don't make .js files, they will be compiled from the .ts files
- Deno bindings for PureScript are available in the various deno-* packages (deno-errors, deno-file-system, deno-http-server, etc.)

### Code Conventions

- Use the `Effect.Aff` monad for asynchronous operations
- Use `Data.Argonaut` for JSON encoding and decoding
- Foreign JavaScript implementations should be written in TypeScript files with the same name as the PureScript module
- Database operations include proper error handling with typed errors (`RetrievalError`)
- Follow the existing code style for consistency:
  - Two-space indentation
  - Meaningful type signatures
  - Descriptive variable names
  - Documentation comments for public functions

### Testing

- Tests use the `spec-deno` library for running tests in the Deno environment
- Each module should have corresponding test files
- Run tests with `npm run test`
- Test files should be structured with describe/it blocks for readability

### Deno Compatibility

- Always use the Deno-specific versions of functions where available
- Remember that Deno is more restrictive with permissions, so specify required permissions in scripts
- For Node.js built-in modules, use the `node:` prefix (e.g. `import * from 'node:fs'`)

## Project-Specific Details

- The JSON database stores data in plain JSON files in the filesystem
- All database functionality is centralized in the `JsonDb.Database` module
- The database API provides operations for get, set, and update of JSON documents
- The server component (`JsonDb.Server`) provides a REST API for database operations
- The project uses PureScript's `JsonDecodeError` for handling decoding errors
- Records are organized hierarchically with location paths (using `index` arrays and `key` strings)

## Tips and Troubleshooting

- If encountering Deno import errors for Node.js modules, make sure to add the `node:` prefix in the deno.json imports
- If modules aren't found, check if they are listed in the spago.yaml dependencies
- For permissions errors in Deno, check the flags passed in the npm scripts

## Useful Resources

- [PureScript Documentation](https://www.purescript.org/documentation)
- [Deno Documentation](https://deno.land/manual)
- [PureScript Deno Core](https://github.com/colinlogue/purescript-deno-core)
