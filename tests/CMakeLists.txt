set(_srcs
    test_utils.F90
    test_fyaml.F90)

add_compile_options(${_fyaml_compiler_options})

enable_testing()

add_executable(test_fyaml ${_srcs})
target_link_libraries(test_fyaml PRIVATE fyaml)
set_target_properties(test_fyaml PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

add_test(
  NAME test_fyaml
  COMMAND test_fyaml
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
