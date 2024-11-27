#!/bin/bash

# Exit on error
set -e

# Help message
show_help() {
    echo "Usage: ./build.sh [clean] [debug]"
    echo "  clean    Clean build directory before building"
    echo "  debug    Build with debug symbols and flags"
    exit 0
}

# Process arguments
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    show_help
fi

# Default build type
BUILD_TYPE="Release"

# Process all arguments
for arg in "$@"; do
    case $arg in
        clean)
            echo "Cleaning build directory..."
            rm -rf build
            ;;
        debug)
            BUILD_TYPE="Debug"
            echo "Setting debug build type..."
            ;;
    esac
done

# Create and enter build directory
mkdir -p build
cd build

# Configure with CMake
echo "Configuring with CMake..."
cmake -DCMAKE_BUILD_TYPE=$BUILD_TYPE ..

# Build
echo "Building..."
make -j$(sysctl -n hw.ncpu)

# Run tests
echo "Running tests..."
ctest --test-dir tests --output-on-failure

# Return to original directory
cd ..

echo "Build complete! (Build type: $BUILD_TYPE)"
