#!/bin/bash

# LensDB Docker Entrypoint Script
# This script handles initialization and startup of LensDB in Docker containers

set -e

# Function to log messages
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') [entrypoint] $*" >&2
}

# Function to wait for a service to be available
wait_for_service() {
    local host=$1
    local port=$2
    local timeout=${3:-30}
    
    log "Waiting for service at $host:$port..."
    
    for i in $(seq 1 $timeout); do
        if nc -z "$host" "$port" 2>/dev/null; then
            log "Service at $host:$port is available"
            return 0
        fi
        sleep 1
    done
    
    log "Timeout waiting for service at $host:$port"
    return 1
}

# Function to create directories if they don't exist
ensure_directories() {
    local dirs=("$LENSDB_DATA_DIR" "$(dirname "$LENSDB_LOG_FILE")" "$(dirname "$LENSDB_CONFIG_FILE")")
    
    for dir in "${dirs[@]}"; do
        if [ ! -d "$dir" ]; then
            log "Creating directory: $dir"
            mkdir -p "$dir"
        fi
    done
}

# Function to validate configuration
validate_config() {
    local config_file="$1"
    
    if [ ! -f "$config_file" ]; then
        log "Configuration file not found: $config_file"
        return 1
    fi
    
    log "Configuration file found: $config_file"
    
    # Basic YAML validation (requires yq or python-yaml)
    if command -v yq >/dev/null 2>&1; then
        if yq eval '.' "$config_file" >/dev/null 2>&1; then
            log "Configuration file is valid YAML"
        else
            log "Configuration file has invalid YAML syntax"
            return 1
        fi
    elif command -v python3 >/dev/null 2>&1 && python3 -c "import yaml" 2>/dev/null; then
        if python3 -c "import yaml; yaml.safe_load(open('$config_file'))" 2>/dev/null; then
            log "Configuration file is valid YAML"
        else
            log "Configuration file has invalid YAML syntax"
            return 1
        fi
    else
        log "Warning: No YAML validator found, skipping validation"
    fi
    
    return 0
}

# Function to check data directory
check_data_directory() {
    local data_dir="$1"
    
    if [ ! -d "$data_dir" ]; then
        log "Creating data directory: $data_dir"
        mkdir -p "$data_dir"
    fi
    
    # Check if data directory is writable
    if [ ! -w "$data_dir" ]; then
        log "Error: Data directory is not writable: $data_dir"
        exit 1
    fi
    
    log "Data directory is ready: $data_dir"
}

# Function to perform health check
health_check() {
    local host=${LENSDB_HOST:-"localhost"}
    local port=${LENSDB_PORT:-8080}
    local timeout=${HEALTH_CHECK_TIMEOUT:-10}
    
    log "Performing health check..."
    
    # Try to connect to the server
    if nc -z "$host" "$port" 2>/dev/null; then
        log "Health check passed - server is responding"
        return 0
    else
        log "Health check failed - server is not responding"
        return 1
    fi
}

# Function to handle graceful shutdown
graceful_shutdown() {
    log "Received shutdown signal, initiating graceful shutdown..."
    
    # Send SIGTERM to the main process
    if [ -n "$LENSDB_PID" ]; then
        log "Sending SIGTERM to LensDB process (PID: $LENSDB_PID)"
        kill -TERM "$LENSDB_PID"
        
        # Wait for graceful shutdown
        for i in {1..30}; do
            if ! kill -0 "$LENSDB_PID" 2>/dev/null; then
                log "LensDB process has terminated gracefully"
                exit 0
            fi
            sleep 1
        done
        
        log "Graceful shutdown timeout, forcing termination"
        kill -KILL "$LENSDB_PID"
    else
        log "No LensDB PID found, exiting"
    fi
    
    exit 0
}

# Function to start LensDB
start_lensdb() {
    local config_file="$1"
    
    log "Starting LensDB with configuration: $config_file"
    
    # Set default environment variables if not set
    export LENSDB_HOST=${LENSDB_HOST:-"0.0.0.0"}
    export LENSDB_PORT=${LENSDB_PORT:-8080}
    export LENSDB_DATA_DIR=${LENSDB_DATA_DIR:-"/data"}
    export LENSDB_LOG_FILE=${LENSDB_LOG_FILE:-"/logs/lensdb.log"}
    export LENSDB_CONFIG_FILE=${LENSDB_CONFIG_FILE:-"/config/lensdb.yaml"}
    
    # Start LensDB in the background
    /usr/local/bin/lensdb-server --config "$config_file" &
    LENSDB_PID=$!
    
    log "LensDB started with PID: $LENSDB_PID"
    
    # Wait for the process to start
    sleep 2
    
    # Perform initial health check
    if health_check; then
        log "LensDB is ready and accepting connections"
    else
        log "Warning: Initial health check failed"
    fi
    
    # Wait for the process to finish
    wait "$LENSDB_PID"
    local exit_code=$?
    
    log "LensDB process exited with code: $exit_code"
    exit $exit_code
}

# Main execution
main() {
    log "LensDB Docker entrypoint starting..."
    
    # Set up signal handlers
    trap graceful_shutdown SIGTERM SIGINT
    
    # Set default values
    export LENSDB_CONFIG_FILE=${LENSDB_CONFIG_FILE:-"/config/lensdb.yaml"}
    export LENSDB_DATA_DIR=${LENSDB_DATA_DIR:-"/data"}
    export LENSDB_LOG_FILE=${LENSDB_LOG_FILE:-"/logs/lensdb.log"}
    export LENSDB_HOST=${LENSDB_HOST:-"0.0.0.0"}
    export LENSDB_PORT=${LENSDB_PORT:-8080}
    
    log "Environment variables:"
    log "  LENSDB_CONFIG_FILE: $LENSDB_CONFIG_FILE"
    log "  LENSDB_DATA_DIR: $LENSDB_DATA_DIR"
    log "  LENSDB_LOG_FILE: $LENSDB_LOG_FILE"
    log "  LENSDB_HOST: $LENSDB_HOST"
    log "  LENSDB_PORT: $LENSDB_PORT"
    
    # Ensure required directories exist
    ensure_directories
    
    # Validate configuration
    if ! validate_config "$LENSDB_CONFIG_FILE"; then
        log "Configuration validation failed, exiting"
        exit 1
    fi
    
    # Check data directory
    check_data_directory "$LENSDB_DATA_DIR"
    
    # Start LensDB
    start_lensdb "$LENSDB_CONFIG_FILE"
}

# Run main function
main "$@"
