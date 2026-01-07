# Multi-stage Dockerfile for LensDB
# Stage 1: Build stage
FROM haskell:9.6.3-slim-buster AS builder

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libgmp-dev \
    libncurses-dev \
    libssl-dev \
    zlib1g-dev \
    ca-certificates \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install Cabal and update package index
RUN ghc --version && \
    cabal --version && \
    cabal update

# Copy cabal files for dependency caching
COPY lensdb.cabal cabal.project* ./

# Install dependencies
RUN cabal build --only-dependencies --enable-tests --enable-benchmarks

# Copy source code
COPY src/ ./src/
COPY app/ ./app/
COPY test/ ./test/
COPY bench/ ./bench/

# Build the application
RUN cabal build --enable-tests --enable-benchmarks all

# Stage 2: Runtime stage
FROM debian:buster-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libncurses6 \
    libssl1.1 \
    zlib1g \
    ca-certificates \
    netcat-openbsd \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -r lensdb && \
    useradd -r -g lensdb -d /data -s /bin/bash lensdb

# Create directories
RUN mkdir -p /data /logs /config && \
    chown -R lensdb:lensdb /data /logs /config

# Set working directory
WORKDIR /app

# Copy the built executable from builder stage
COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-9.6.3/lensdb-0.1.0.0/x/lensdb-server/build/lensdb-server/lensdb-server /usr/local/bin/lensdb-server

# Copy configuration files
COPY docker/lensdb.yaml /config/lensdb.yaml
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh

# Make scripts executable
RUN chmod +x /usr/local/bin/entrypoint.sh /usr/local/bin/lensdb-server

# Switch to non-root user
USER lensdb

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Set environment variables
ENV LENSDB_CONFIG_FILE=/config/lensdb.yaml
ENV LENSDB_DATA_DIR=/data
ENV LENSDB_LOG_FILE=/logs/lensdb.log

# Volume mounts
VOLUME ["/data", "/logs", "/config"]

# Set entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# Default command
CMD ["lensdb-server"]
