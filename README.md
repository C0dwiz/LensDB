# LensDB

A high-performance, resident NoSQL key-value database system written in Haskell.

## Overview

LensDB is a resident NoSQL database system that operates on key-value data structures through bytes. It can be used for:

- Databases
- Caches  
- Message brokers
- Other data storage needs

## Features

- **In-memory storage** with optional persistence
- **Byte-based key-value operations** for maximum flexibility
- **Network protocol** for remote access
- **Configurable** through YAML files
- **Thread-safe** operations using STM
- **Logging** for monitoring and debugging
- **Docker support** for containerized deployment
- **Systemd service** support for production environments

## Quick Start

### Building from Source

```bash
git clone https://github.com/c0dwiz/lensdb.git
cd lensdb
cabal build
```

### Running the Server

```bash
cabal run lensdb-server
```

### Docker Deployment

```bash
docker build -t lensdb .
docker run -p 8080:8080 -v /path/to/data:/data lensdb
```

## Configuration

LensDB uses a YAML configuration file. By default, it looks for `lensdb.yaml` in the current directory.

```yaml
server:
  host: "127.0.0.1"
  port: 8080
  max_connections: 1000

storage:
  max_memory: "1GB"
  persistence_enabled: true
  data_dir: "./data"

logging:
  level: "info"
  file: "./logs/lensdb.log"
```

## Protocol

The network protocol uses a simple binary format:

```
+--------+--------+--------+--------+--------+
| Magic  | Type   | KeyLen | ValLen |  Data  |
+--------+--------+--------+--------+--------+
```

- **Magic**: 4 bytes (0x4C454E53 - "LENS")
- **Type**: 1 byte (0x01=GET, 0x02=SET, 0x03=DEL, 0x04=PING)
- **KeyLen**: 4 bytes (key length)
- **ValLen**: 4 bytes (value length, 0 for GET/DEL)
- **Data**: Key + Value (for SET)

## API Examples

### SET Operation

```haskell
-- Set key "user:123" to value JSON data
key = "user:123"
value = "{\"name\":\"John\",\"age\":30}"
```

### GET Operation  

```haskell
-- Get value for key "user:123"
key = "user:123"
```

### DEL Operation

```haskell
-- Delete key "user:123"
key = "user:123"
```

## Performance

LensDB is designed for high-performance scenarios:

- **Memory-efficient**: Uses strict ByteStrings for storage
- **Concurrent**: STM-based transactions for thread safety
- **Network-optimized**: Binary protocol for minimal overhead

## Development

### Running Tests

```bash
cabal test
```

### Running Benchmarks

```bash
cabal bench
```

### Code Style

The project follows Haskell best practices:

- Comprehensive documentation with Haddock
- Type signatures for all functions
- Error handling with Either/Maybe
- Resource safety with bracket patterns

## License

BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Network Layer │────│   Protocol      │────│   Storage Core  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
                       ┌─────────────────┐
                       │   Persistence   │
                       └─────────────────┘
```

- **Network Layer**: Handles TCP connections and client management
- **Protocol**: Implements the binary communication protocol  
- **Storage Core**: In-memory key-value store with STM
- **Persistence**: Optional disk-based storage for durability
