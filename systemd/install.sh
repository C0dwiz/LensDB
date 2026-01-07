#!/bin/bash

# LensDB Systemd Service Installation Script
# This script installs and configures LensDB as a systemd service

set -e

# Configuration
SERVICE_NAME="lensdb"
USER="lensdb"
GROUP="lensdb"
INSTALL_DIR="/usr/local/bin"
CONFIG_DIR="/etc/lensdb"
DATA_DIR="/var/lib/lensdb"
LOG_DIR="/var/log/lensdb"
RUN_DIR="/var/run/lensdb"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Function to check if script is run as root
check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root"
        exit 1
    fi
}

# Function to create user and group
create_user() {
    log_info "Creating user and group: $USER:$GROUP"
    
    if ! getent group "$GROUP" >/dev/null 2>&1; then
        groupadd --system "$GROUP"
        log_info "Created group: $GROUP"
    else
        log_warn "Group $GROUP already exists"
    fi
    
    if ! getent passwd "$USER" >/dev/null 2>&1; then
        useradd --system --gid "$GROUP" --home-dir "$DATA_DIR" \
                --shell /usr/sbin/nologin --comment "LensDB service user" "$USER"
        log_info "Created user: $USER"
    else
        log_warn "User $USER already exists"
    fi
}

# Function to create directories
create_directories() {
    log_info "Creating directories..."
    
    local dirs=("$CONFIG_DIR" "$DATA_DIR" "$LOG_DIR" "$RUN_DIR")
    
    for dir in "${dirs[@]}"; do
        if [[ ! -d "$dir" ]]; then
            mkdir -p "$dir"
            log_info "Created directory: $dir"
        else
            log_warn "Directory already exists: $dir"
        fi
        
        # Set ownership and permissions
        chown "$USER:$GROUP" "$dir"
        chmod 755 "$dir"
    done
    
    # Special permissions for log directory
    chmod 755 "$LOG_DIR"
}

# Function to install the binary
install_binary() {
    local binary_path="$1"
    
    if [[ -z "$binary_path" ]]; then
        log_error "Binary path not provided"
        exit 1
    fi
    
    if [[ ! -f "$binary_path" ]]; then
        log_error "Binary not found: $binary_path"
        exit 1
    fi
    
    log_info "Installing binary from: $binary_path"
    
    # Copy binary to install directory
    cp "$binary_path" "$INSTALL_DIR/lensdb-server"
    
    # Set permissions
    chmod 755 "$INSTALL_DIR/lensdb-server"
    chown root:root "$INSTALL_DIR/lensdb-server"
    
    log_info "Binary installed to: $INSTALL_DIR/lensdb-server"
}

# Function to install configuration files
install_config() {
    local config_source="$1"
    
    log_info "Installing configuration files..."
    
    if [[ -n "$config_source" && -f "$config_source" ]]; then
        cp "$config_source" "$CONFIG_DIR/lensdb.yaml"
        log_info "Installed configuration from: $config_source"
    else
        # Create default configuration
        cat > "$CONFIG_DIR/lensdb.yaml" << 'EOF'
# LensDB Configuration
server:
  host: "127.0.0.1"
  port: 8080
  max_connections: 1000
  connection_timeout: 300
  buffer_size: 4096
  workers: 4

storage:
  max_memory: "1GB"
  max_keys: 0
  cleanup_interval: 300
  compaction_threshold: 0.8
  eviction_policy: "LRU"

logging:
  level: "info"
  file: "/var/log/lensdb/lensdb.log"
  max_size: "100MB"
  backup_count: 5
  format: "text"

persistence:
  enabled: true
  data_dir: "/var/lib/lensdb/data"
  sync_interval: 60
  compression: false
  backup_enabled: true
  backup_interval: 3600
EOF
        log_info "Created default configuration file"
    fi
    
    # Set permissions
    chown "$USER:$GROUP" "$CONFIG_DIR/lensdb.yaml"
    chmod 640 "$CONFIG_DIR/lensdb.yaml"
}

# Function to install systemd service files
install_service() {
    log_info "Installing systemd service files..."
    
    # Copy service files
    cp "$(dirname "$0")/lensdb.service" "/etc/systemd/system/"
    cp "$(dirname "$0")/lensdb@.service" "/etc/systemd/system/"
    
    # Reload systemd
    systemctl daemon-reload
    
    log_info "Systemd service files installed"
}

# Function to enable and start service
enable_service() {
    log_info "Enabling and starting LensDB service..."
    
    # Enable the service
    systemctl enable lensdb.service
    
    # Start the service
    systemctl start lensdb.service
    
    # Check status
    if systemctl is-active --quiet lensdb.service; then
        log_info "LensDB service is running"
    else
        log_error "Failed to start LensDB service"
        systemctl status lensdb.service
        exit 1
    fi
}

# Function to show service status
show_status() {
    log_info "LensDB service status:"
    systemctl status lensdb.service --no-pager
    
    log_info "Recent logs:"
    journalctl -u lensdb.service --no-pager -n 20
}

# Function to uninstall
uninstall() {
    log_info "Uninstalling LensDB service..."
    
    # Stop and disable service
    systemctl stop lensdb.service 2>/dev/null || true
    systemctl disable lensdb.service 2>/dev/null || true
    
    # Remove service files
    rm -f "/etc/systemd/system/lensdb.service"
    rm -f "/etc/systemd/system/lensdb@.service"
    systemctl daemon-reload
    
    # Remove binary
    rm -f "$INSTALL_DIR/lensdb-server"
    
    # Remove user and group (optional)
    read -p "Remove LensDB user and group? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        userdel "$USER" 2>/dev/null || true
        groupdel "$GROUP" 2>/dev/null || true
        log_info "Removed user and group"
    fi
    
    # Remove directories (optional)
    read -p "Remove LensDB directories? (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -rf "$CONFIG_DIR" "$DATA_DIR" "$LOG_DIR" "$RUN_DIR"
        log_info "Removed directories"
    fi
    
    log_info "LensDB service uninstalled"
}

# Function to show usage
show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

LensDB Systemd Service Installation Script

OPTIONS:
    -b, --binary PATH        Path to lensdb-server binary
    -c, --config PATH        Path to configuration file
    -u, --uninstall          Uninstall LensDB service
    -s, --status             Show service status
    -h, --help               Show this help message

EXAMPLES:
    $0 --binary ./dist/build/lensdb-server/lensdb-server
    $0 --binary ./lensdb-server --config ./lensdb.yaml
    $0 --uninstall
    $0 --status

EOF
}

# Main function
main() {
    local binary_path=""
    local config_path=""
    local uninstall=false
    local show_status_only=false
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -b|--binary)
                binary_path="$2"
                shift 2
                ;;
            -c|--config)
                config_path="$2"
                shift 2
                ;;
            -u|--uninstall)
                uninstall=true
                shift
                ;;
            -s|--status)
                show_status_only=true
                shift
                ;;
            -h|--help)
                show_usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    # Check if uninstalling
    if [[ "$uninstall" == true ]]; then
        check_root
        uninstall
        exit 0
    fi
    
    # Show status only
    if [[ "$show_status_only" == true ]]; then
        show_status
        exit 0
    fi
    
    # Check if running as root
    check_root
    
    log_info "Starting LensDB service installation..."
    
    # Install steps
    create_user
    create_directories
    
    if [[ -n "$binary_path" ]]; then
        install_binary "$binary_path"
    else
        log_error "Binary path is required. Use --binary option."
        show_usage
        exit 1
    fi
    
    install_config "$config_path"
    install_service
    enable_service
    
    log_info "LensDB service installation completed successfully!"
    log_info "Service is running and will start automatically on boot."
    log_info ""
    log_info "Configuration file: $CONFIG_DIR/lensdb.yaml"
    log_info "Data directory: $DATA_DIR"
    log_info "Log directory: $LOG_DIR"
    log_info ""
    log_info "To manage the service:"
    log_info "  Start:   systemctl start lensdb"
    log_info "  Stop:    systemctl stop lensdb"
    log_info "  Restart: systemctl restart lensdb"
    log_info "  Status:  systemctl status lensdb"
    log_info "  Logs:    journalctl -u lensdb -f"
}

# Run main function
main "$@"
