program test_fyaml
    use fyaml
    implicit none

    type(fyaml_doc) :: doc
    type(yaml_value) :: val, person
    logical :: success
    character(len=:), allocatable, dimension(:) :: keys
    character(len=*), parameter :: source_dir = SOURCE_DIR  ! macro
    character(len=*), parameter :: filename = source_dir //"/"//"example.yaml"

    success = .false.
    print "('Loading file:', x, a)", filename
    call doc%load(filename, success)
    if (.not. success) then
      write(error_unit,*) 'Error: Failed to load YAML file'
      error stop
    end if

    ! Get all person keys
    person = doc%root%get("person")
    keys = person%dict_val%keys()
    print *, "Keys:", keys

    ! Get specific values
    val = person%get("name")
    print *, "Name:", val%str_val

    val = person%get("age")
    print *, "Age:", val%int_val

    val = person%get("skills")
    if (allocated(val%str_sequence)) then
        print *, "Skills:", val%str_sequence
    endif

end program test_fyaml
