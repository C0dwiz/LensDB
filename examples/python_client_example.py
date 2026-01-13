#!/usr/bin/env python3
"""
LensDB Python Client Example
============================

This is a complete example of how to implement a Python client for LensDB.
It demonstrates the network protocol, message serialization, and provides
a clean API that can be used as a foundation for a production library.

The LensDB protocol uses binary messages with the following format:
- Magic Number (4 bytes): 0x4C444244 ("LDBD")
- Version (1 byte): 0x01
- Message Type (1 byte): Operation type
- Key Length (2 bytes): Length of key
- Value Length (4 bytes): Length of value
- Flags (4 bytes): Additional flags
- Key: Variable length
- Value: Variable length
"""

import socket
import struct
from typing import Optional, List
from dataclasses import dataclass
from enum import IntEnum
import time


class MessageType(IntEnum):
    """LensDB message types"""

    MSG_GET = 0x01
    MSG_SET = 0x02
    MSG_DELETE = 0x03
    MSG_EXISTS = 0x04
    MSG_KEYS = 0x05
    MSG_SIZE = 0x06
    MSG_CLEAR = 0x07
    MSG_SETEX = 0x08
    MSG_PING = 0x09


class ResponseStatus(IntEnum):
    """LensDB response status codes"""

    STATUS_SUCCESS = 0x00
    STATUS_ERROR = 0x01
    STATUS_KEY_NOT_FOUND = 0x02
    STATUS_INVALID_REQUEST = 0x03
    STATUS_SERVER_ERROR = 0x04


@dataclass
class ProtocolMessage:
    """LensDB protocol message"""

    magic: int = 0x4C444244  # "LDBD"
    version: int = 0x01
    msg_type: int = 0
    key: bytes = b""
    value: bytes = b""
    flags: int = 0

    def serialize(self) -> bytes:
        """Serialize message to bytes"""
        return (
            struct.pack(
                ">IBBHIBI",
                self.magic,
                self.version,
                self.msg_type,
                len(self.key),
                len(self.value),
                self.flags,
            )
            + self.key
            + self.value
        )

    @classmethod
    def deserialize(cls, data: bytes) -> "ProtocolMessage":
        """Deserialize message from bytes"""
        if len(data) < 16:
            raise ValueError("Message too short")

        magic, version, msg_type, key_len, value_len, flags = struct.unpack(
            ">IBBHIBI", data[:16]
        )

        if magic != 0x4C444244:
            raise ValueError(f"Invalid magic number: {magic}")

        if len(data) < 16 + key_len + value_len:
            raise ValueError("Message incomplete")

        key = data[16 : 16 + key_len]
        value = data[16 + key_len : 16 + key_len + value_len]

        return cls(magic, version, msg_type, key, value, flags)


@dataclass
class ProtocolResponse:
    """LensDB protocol response"""

    magic: int = 0x4C444244
    version: int = 0x01
    status: int = 0
    message: bytes = b""
    data: bytes = b""

    def serialize(self) -> bytes:
        """Serialize response to bytes"""
        return (
            struct.pack(
                ">IBHI", self.magic, self.version, self.status, len(self.message)
            )
            + self.message
            + self.data
        )

    @classmethod
    def deserialize(cls, data: bytes) -> "ProtocolResponse":
        """Deserialize response from bytes"""
        if len(data) < 12:
            raise ValueError("Response too short")

        magic, version, status, msg_len = struct.unpack(">IBHI", data[:12])

        if magic != 0x4C444244:
            raise ValueError(f"Invalid magic number: {magic}")

        if len(data) < 12 + msg_len:
            raise ValueError("Response incomplete")

        message = data[12 : 12 + msg_len]
        data_payload = data[12 + msg_len :]

        return cls(magic, version, status, message, data_payload)


class LensDBError(Exception):
    """Base LensDB exception"""

    pass


class ConnectionError(LensDBError):
    """Connection related error"""

    pass


class ProtocolError(LensDBError):
    """Protocol related error"""

    pass


class ServerError(LensDBError):
    """Server returned error"""

    pass


class TimeoutError(LensDBError):
    """Operation timed out"""

    pass


class LensDBClient:
    """
    LensDB Python Client

    This is a complete implementation of a LensDB client that demonstrates:
    - Network communication with the server
    - Binary protocol implementation
    - Error handling and reconnection logic
    - Clean Python API design

    This can be used as a reference for implementing your own LensDB client
    or as a foundation for a production-ready library.
    """

    def __init__(
        self,
        host: str = "127.0.0.1",
        port: int = 8080,
        timeout: int = 30,
        max_retries: int = 3,
        buffer_size: int = 4096,
    ):
        """
        Initialize LensDB client

        Args:
            host: Server hostname
            port: Server port
            timeout: Connection timeout in seconds
            max_retries: Maximum retry attempts for failed operations
            buffer_size: Network buffer size
        """
        self.host = host
        self.port = port
        self.timeout = timeout
        self.max_retries = max_retries
        self.buffer_size = buffer_size
        self._socket: Optional[socket.socket] = None
        self._connected = False

    def connect(self) -> None:
        """Connect to LensDB server"""
        if self._connected:
            return

        try:
            self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self._socket.settimeout(self.timeout)
            self._socket.connect((self.host, self.port))
            self._connected = True
        except socket.error as e:
            raise ConnectionError(f"Failed to connect to {self.host}:{self.port}: {e}")

    def disconnect(self) -> None:
        """Disconnect from server"""
        if self._socket:
            try:
                self._socket.close()
            except:  # noqa: E722
                pass
            finally:
                self._socket = None
                self._connected = False

    def _send_message(self, message: ProtocolMessage) -> ProtocolResponse:
        """Send message and receive response"""
        if not self._connected or not self._socket:
            raise ConnectionError("Not connected to server")

        for attempt in range(self.max_retries + 1):
            try:
                # Send message
                data = message.serialize()
                self._socket.sendall(data)

                # Receive response
                response_data = self._socket.recv(self.buffer_size)
                if not response_data:
                    raise ConnectionError("Connection closed by server")

                # Try to receive more data if buffer was full
                while len(response_data) < 12:  # Minimum response size
                    more_data = self._socket.recv(self.buffer_size)
                    if not more_data:
                        break
                    response_data += more_data

                response = ProtocolResponse.deserialize(response_data)
                return response

            except socket.timeout:
                if attempt == self.max_retries:
                    raise TimeoutError("Operation timed out")
                time.sleep(1)  # Wait before retry
            except socket.error as e:
                if attempt == self.max_retries:
                    raise ConnectionError(f"Network error: {e}")
                time.sleep(1)  # Wait before retry

    def get(self, key: str) -> Optional[bytes]:
        """
        Get value by key

        Args:
            key: The key to retrieve

        Returns:
            The value if found, None otherwise

        Raises:
            LensDBError: If operation fails
        """
        if not isinstance(key, str):
            raise TypeError("Key must be a string")

        message = ProtocolMessage(msg_type=MessageType.MSG_GET, key=key.encode("utf-8"))

        response = self._send_message(message)

        if response.status == ResponseStatus.STATUS_SUCCESS:
            return response.data if response.data else None
        elif response.status == ResponseStatus.STATUS_KEY_NOT_FOUND:
            return None
        else:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def set(self, key: str, value: bytes) -> None:
        """
        Set key-value pair

        Args:
            key: The key to set
            value: The value to store

        Raises:
            LensDBError: If operation fails
        """
        if not isinstance(key, str):
            raise TypeError("Key must be a string")
        if not isinstance(value, bytes):
            raise TypeError("Value must be bytes")

        message = ProtocolMessage(
            msg_type=MessageType.MSG_SET, key=key.encode("utf-8"), value=value
        )

        response = self._send_message(message)

        if response.status != ResponseStatus.STATUS_SUCCESS:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def setex(self, key: str, value: bytes, ttl: int) -> None:
        """
        Set key-value pair with expiration

        Args:
            key: The key to set
            value: The value to store
            ttl: Time to live in seconds

        Raises:
            LensDBError: If operation fails
        """
        if not isinstance(key, str):
            raise TypeError("Key must be a string")
        if not isinstance(value, bytes):
            raise TypeError("Value must be bytes")
        if not isinstance(ttl, int) or ttl < 0:
            raise TypeError("TTL must be a non-negative integer")

        # Encode TTL in flags (first 4 bytes)
        flags = ttl

        message = ProtocolMessage(
            msg_type=MessageType.MSG_SETEX,
            key=key.encode("utf-8"),
            value=value,
            flags=flags,
        )

        response = self._send_message(message)

        if response.status != ResponseStatus.STATUS_SUCCESS:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def delete(self, key: str) -> bool:
        """
        Delete key

        Args:
            key: The key to delete

        Returns:
            True if key was deleted, False if key didn't exist

        Raises:
            LensDBError: If operation fails
        """
        if not isinstance(key, str):
            raise TypeError("Key must be a string")

        message = ProtocolMessage(
            msg_type=MessageType.MSG_DELETE, key=key.encode("utf-8")
        )

        response = self._send_message(message)

        if response.status == ResponseStatus.STATUS_SUCCESS:
            return True
        elif response.status == ResponseStatus.STATUS_KEY_NOT_FOUND:
            return False
        else:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def exists(self, key: str) -> bool:
        """
        Check if key exists

        Args:
            key: The key to check

        Returns:
            True if key exists, False otherwise

        Raises:
            LensDBError: If operation fails
        """
        if not isinstance(key, str):
            raise TypeError("Key must be a string")

        message = ProtocolMessage(
            msg_type=MessageType.MSG_EXISTS, key=key.encode("utf-8")
        )

        response = self._send_message(message)

        if response.status == ResponseStatus.STATUS_SUCCESS:
            return response.data == b"true"
        else:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def keys(self) -> List[str]:
        """
        Get all keys

        Returns:
            List of all keys in the database

        Raises:
            LensDBError: If operation fails
        """
        message = ProtocolMessage(msg_type=MessageType.MSG_KEYS)
        response = self._send_message(message)

        if response.status == ResponseStatus.STATUS_SUCCESS:
            # Keys are returned as newline-separated bytes
            if response.data:
                return [k.decode("utf-8") for k in response.data.split(b"\n") if k]
            return []
        else:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def size(self) -> int:
        """
        Get database size (number of keys)

        Returns:
            Number of keys in the database

        Raises:
            LensDBError: If operation fails
        """
        message = ProtocolMessage(msg_type=MessageType.MSG_SIZE)
        response = self._send_message(message)

        if response.status == ResponseStatus.STATUS_SUCCESS:
            try:
                return int(response.data.decode("utf-8"))
            except ValueError:
                raise ProtocolError("Invalid size response from server")
        else:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def clear(self) -> None:
        """
        Clear all data in the database

        Raises:
            LensDBError: If operation fails
        """
        message = ProtocolMessage(msg_type=MessageType.MSG_CLEAR)
        response = self._send_message(message)

        if response.status != ResponseStatus.STATUS_SUCCESS:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def ping(self) -> bytes:
        """
        Ping server

        Returns:
            Server response (typically b'PONG')

        Raises:
            LensDBError: If operation fails
        """
        message = ProtocolMessage(msg_type=MessageType.MSG_PING)
        response = self._send_message(message)

        if response.status == ResponseStatus.STATUS_SUCCESS:
            return response.data
        else:
            raise ServerError(
                f"Server error: {response.message.decode('utf-8', errors='ignore')}"
            )

    def __enter__(self):
        """Context manager entry"""
        self.connect()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.disconnect()


# Example usage and testing
if __name__ == "__main__":
    import json

    print("LensDB Python Client Example")
    print("=" * 40)

    try:
        # Connect to server
        with LensDBClient(host="127.0.0.1", port=8080) as client:
            print("✓ Connected to LensDB server")

            # Test basic operations
            print("\n--- Basic Operations ---")

            # SET operation
            client.set("test:key", b"Hello, LensDB!")
            print("✓ SET test:key = 'Hello, LensDB!'")

            # GET operation
            value = client.get("test:key")
            print(f"✓ GET test:key = {value}")

            # EXISTS operation
            exists = client.exists("test:key")
            print(f"✓ EXISTS test:key = {exists}")

            # SETEX operation
            client.setex("temp:key", b"Temporary data", 60)
            print("✓ SETEX temp:key = 'Temporary data' (TTL: 60s)")

            # KEYS operation
            keys = client.keys()
            print(f"✓ KEYS = {keys}")

            # SIZE operation
            size = client.size()
            print(f"✓ SIZE = {size}")

            # PING operation
            pong = client.ping()
            print(f"✓ PING = {pong}")

            # Test JSON data
            print("\n--- JSON Data Example ---")
            user_data = {
                "id": 123,
                "name": "Alice",
                "email": "alice@example.com",
                "active": True,
            }
            client.set("user:123", json.dumps(user_data).encode("utf-8"))

            retrieved = client.get("user:123")
            if retrieved:
                user = json.loads(retrieved.decode("utf-8"))
                print(f"✓ Stored and retrieved user: {user}")

            # Test batch operations simulation
            print("\n--- Batch Operations ---")
            batch_users = [
                ("user:1", b'{"name": "Alice"}'),
                ("user:2", b'{"name": "Bob"}'),
                ("user:3", b'{"name": "Charlie"}'),
            ]

            for key, value in batch_users:
                client.set(key, value)

            print("✓ Batch SET completed")

            # Retrieve batch
            retrieved_users = []
            for key, _ in batch_users:
                value = client.get(key)
                retrieved_users.append((key, value))

            print("✓ Batch GET completed")
            for key, value in retrieved_users:
                print(f"  {key} = {value}")

            # Cleanup
            print("\n--- Cleanup ---")
            client.delete("test:key")
            client.delete("temp:key")
            client.delete("user:123")
            for key, _ in batch_users:
                client.delete(key)

            print("✓ Cleanup completed")

    except LensDBError as e:
        print(f"✗ LensDB Error: {e}")
    except Exception as e:
        print(f"✗ Unexpected error: {e}")

    print("\nExample completed!")
