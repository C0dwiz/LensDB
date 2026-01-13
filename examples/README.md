# LensDB Python Client Implementation

This directory contains a complete example implementation of a Python client for LensDB. This example demonstrates how to:

- Implement the binary LensDB protocol
- Handle network communication
- Manage connections and errors
- Provide a clean Python API

## Files

- `python_client_example.py` - Complete client implementation with examples
- `README.md` - This file with usage instructions

## Protocol Specification

The LensDB protocol uses binary messages with the following format:

### Message Format (Client → Server)

```
+----------+--------+------------+------------+------------+--------+-------+--------+
| Magic    | Version| Msg Type   | Key Length | Value Len  | Flags  | Key   | Value  |
| (4 bytes)| (1 byte)| (1 byte)   | (2 bytes)  | (4 bytes)  | (4 bytes)|Var   |Var     |
+----------+--------+------------+------------+------------+--------+-------+--------+
```

- **Magic**: `0x4C444244` ("LDBD")
- **Version**: `0x01`
- **Msg Type**: Message type (GET, SET, DELETE, etc.)
- **Key Length**: Length of key in bytes
- **Value Length**: Length of value in bytes
- **Flags**: Additional flags (used for TTL in SETEX)
- **Key**: Key data
- **Value**: Value data

### Response Format (Server → Client)

```
+----------+--------+----------+------------+--------+----------+
| Magic    | Version| Status   | Msg Length | Message| Data     |
| (4 bytes)| (1 byte)| (1 byte) | (2 bytes)  |Var     |Var       |
+----------+--------+----------+------------+--------+----------+
```

- **Magic**: `0x4C444244` ("LDBD")
- **Version**: `0x01`
- **Status**: Response status (SUCCESS, ERROR, KEY_NOT_FOUND, etc.)
- **Msg Length**: Length of error message
- **Message**: Error message (if any)
- **Data**: Response data

## Message Types

```python
class MessageType(IntEnum):
    MSG_GET = 0x01      # Get value by key
    MSG_SET = 0x02      # Set key-value pair
    MSG_DELETE = 0x03   # Delete key
    MSG_EXISTS = 0x04   # Check if key exists
    MSG_KEYS = 0x05     # Get all keys
    MSG_SIZE = 0x06     # Get database size
    MSG_CLEAR = 0x07    # Clear all data
    MSG_SETEX = 0x08    # Set with expiration
    MSG_PING = 0x09     # Ping server
```

## Response Status Codes

```python
class ResponseStatus(IntEnum):
    STATUS_SUCCESS = 0x00        # Operation successful
    STATUS_ERROR = 0x01          # General error
    STATUS_KEY_NOT_FOUND = 0x02  # Key not found
    STATUS_INVALID_REQUEST = 0x03 # Invalid request
    STATUS_SERVER_ERROR = 0x04   # Server error
```

## Usage Example

### Basic Usage

```python
from python_client_example import LensDBClient

# Connect and use
with LensDBClient(host='127.0.0.1', port=8080) as client:
    # Set a value
    client.set('user:1', b'{"name": "Alice", "age": 30}')
    
    # Get a value
    value = client.get('user:1')
    print(value)  # b'{"name": "Alice", "age": 30}'
    
    # Delete a key
    client.delete('user:1')
```

### Advanced Usage

```python
import json
from python_client_example import LensDBClient, LensDBError

class UserCache:
    def __init__(self, host='127.0.0.1', port=8080):
        self.client = LensDBClient(host=host, port=port)
        self.client.connect()
    
    def cache_user(self, user_id, user_data, ttl=3600):
        """Cache user data with TTL"""
        key = f'user:{user_id}'
        value = json.dumps(user_data).encode('utf-8')
        self.client.setex(key, value, ttl)
    
    def get_user(self, user_id):
        """Get cached user data"""
        key = f'user:{user_id}'
        value = self.client.get(key)
        if value:
            return json.loads(value.decode('utf-8'))
        return None
    
    def close(self):
        """Close connection"""
        self.client.disconnect()

# Usage
cache = UserCache()
try:
    cache.cache_user(123, {'name': 'Alice', 'email': 'alice@example.com'})
    user = cache.get_user(123)
    print(user)  # {'name': 'Alice', 'email': 'alice@example.com'}
finally:
    cache.close()
```

## Building a Production Library

This example can be used as a foundation for a production-ready library. Here are the recommended improvements:

### 1. Connection Pooling

```python
class ConnectionPool:
    def __init__(self, host='127.0.0.1', port=8080, max_connections=10):
        self.host = host
        self.port = port
        self.max_connections = max_connections
        self._pool = queue.Queue(maxsize=max_connections)
        self._create_connections()
    
    def get_connection(self):
        return self._pool.get()
    
    def return_connection(self, conn):
        self._pool.put(conn)
    
    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
```

### 2. Async Support

```python
import asyncio
import aiofiles

class AsyncLensDBClient:
    async def connect(self):
        self._reader, self._writer = await asyncio.open_connection(
            self.host, self.port
        )
    
    async def get(self, key):
        message = ProtocolMessage(msg_type=MessageType.MSG_GET, key=key.encode())
        await self._send_message(message)
        response = await self._receive_response()
        return response.data if response.status == ResponseStatus.STATUS_SUCCESS else None
```

### 3. Serialization Support

```python
import pickle
import msgpack

class LensDBClient:
    def set_json(self, key, obj, ttl=None):
        """Store JSON-serializable object"""
        import json
        value = json.dumps(obj).encode('utf-8')
        if ttl:
            self.setex(key, value, ttl)
        else:
            self.set(key, value)
    
    def get_json(self, key):
        """Get and deserialize JSON object"""
        value = self.get(key)
        if value:
            return json.loads(value.decode('utf-8'))
        return None
    
    def set_pickle(self, key, obj, ttl=None):
        """Store Python object with pickle"""
        value = pickle.dumps(obj)
        if ttl:
            self.setex(key, value, ttl)
        else:
            self.set(key, value)
    
    def get_pickle(self, key):
        """Get and deserialize pickle object"""
        value = self.get(key)
        if value:
            return pickle.loads(value)
        return None
```

### 4. Retry and Backoff

```python
import random
import time

class LensDBClient:
    def _send_message_with_backoff(self, message):
        for attempt in range(self.max_retries + 1):
            try:
                return self._send_message(message)
            except (ConnectionError, TimeoutError) as e:
                if attempt == self.max_retries:
                    raise
                
                # Exponential backoff with jitter
                delay = (2 ** attempt) + random.uniform(0, 1)
                time.sleep(delay)
```

### 5. Metrics and Monitoring

```python
import time
from collections import defaultdict

class LensDBClient:
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._metrics = defaultdict(int)
        self._latencies = []
    
    def get(self, key):
        start_time = time.time()
        try:
            result = super().get(key)
            self._metrics['get_success'] += 1
            return result
        except Exception as e:
            self._metrics['get_error'] += 1
            raise
        finally:
            latency = time.time() - start_time
            self._latencies.append(latency)
    
    def get_metrics(self):
        return {
            'operations': dict(self._metrics),
            'avg_latency': sum(self._latencies) / len(self._latencies) if self._latencies else 0,
            'total_operations': sum(self._metrics.values())
        }
```

## Testing

Run the example to test connectivity:

```bash
python examples/python_client_example.py
```

Make sure the LensDB server is running:

```bash
cabal run lensdb-server
```

## Error Handling

The client provides specific exceptions for different error types:

- `LensDBError` - Base exception
- `ConnectionError` - Network connection issues
- `ProtocolError` - Protocol parsing errors
- `ServerError` - Server returned error
- `TimeoutError` - Operation timed out

## License

This example code is provided under the same BSD-3-Clause license as LensDB. Feel free to use it as a foundation for your own LensDB client library.
