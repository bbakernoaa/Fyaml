# Fyaml - A Modern Fortran YAML Parser

A lightweight YAML parser written in modern Fortran that supports nested structures, sequences, and various data types. Designed for scientific computing applications needing configuration file support.

## Features

- Parse YAML files into native Fortran data structures
- Support for strings, integers, floats, booleans, and null values
- Handle nested mappings and sequences
- Comment parsing
- Debug logging with configurable levels
- Dot notation access for nested values
- Memory safe with pointer management

## Requirements

- Fortran 2008 compliant compiler (gfortran 8.0+ or ifort 19.0+)
- CMake 3.12+

## Installation

```bash
git clone https://github.com/yourusername/fyaml.git
cd fyaml
mkdir build && cd build
cmake ..
make
make install
```

## Usage
1) Create a YAML configuration file:

```yaml
database:
  host: localhost
  port: 5432
  credentials:
    username: admin
    password: secret
```

2) Parse it in your Fortran code:
```fortran
program example
    use fyaml
    type(fyaml_doc) :: doc
    type(yaml_value) :: val

    call doc%load("config.yaml")
    val = doc%root%get("database.host")
    print *, "Host:", val%str_val
end program
```

## Project Structure
```text
.
├── CMakeLists.txt
├── src/
│   ├── yaml_types.f90    ! Core type definitions
│   ├── yaml_parser.f90   ! YAML parsing implementation
│   └── fyaml.f90         ! High-level interface
├── tests/
│   └── test_example.yaml
└── example/
    ├── example.yaml
    └── yaml_example.f90
```

## Testing
```bash
ctest --test-dir build/tests --output-on-failure
```

## Documentation
Documentation is generated using FORD. To build:

```bash
ford docs.md
```

## License
GNU General Public License v3.0

## Contributing
Contributions welcome! Please read CONTRIBUTING.md for guidelines.
