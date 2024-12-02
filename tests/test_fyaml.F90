program test_fyaml
  use fyaml
  use yaml_parser
  use yaml_types
  use test_utils
  use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
  implicit none

  character(len=256) :: test_dir, filename, line
  logical :: file_exists
  integer :: io_stat, unit
  integer :: i

  call set_debug_level(4)
  ! Get test directory from environment
  call get_environment_variable("FYAML_TEST_DIR", test_dir)
  if (len_trim(test_dir) == 0) test_dir = "."

  filename = trim(test_dir)//"/test_example.yaml"

  ! Debug: Print full path and verify file
  write(*,*) 'Full path:', trim(filename)

  inquire(file=filename, exist=file_exists)
  if (.not. file_exists) then
    write(error_unit,*) 'Error: test file not found:', trim(filename)
    error stop
  end if

  ! Debug: Try reading file contents
  open(newunit=unit, file=filename, status='old', action='read', iostat=io_stat)
  if (io_stat /= 0) then
    write(error_unit,*) 'Error opening file:', io_stat
    error stop
  endif

  ! Debug: Print first few lines
  write(*,*) 'File contents:'
  do i = 1, 5
    read(unit, '(a)', iostat=io_stat) line
    if (io_stat /= 0) exit
    write(*,*) trim(line)
  end do
  close(unit)

  call test_basic_loading()
  call test_basic_types()
  call test_sequences()
  ! call test_nested_structures()
  call test_dict_operations()

  print *, 'All tests passed!'

contains
  subroutine test_basic_loading()
    type(fyaml_doc) :: doc
    logical :: success

    success = .false.
    write(*,*) 'Loading file:', trim(filename)
    call doc%load(filename, success)

    if (.not. success) then
      write(error_unit,*) 'Error: Failed to load YAML file'
      error stop
    end if

    print *, 'Basic loading test passed'
  end subroutine

end program test_fyaml
