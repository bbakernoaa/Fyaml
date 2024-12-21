program example
    use fyaml
    type(fyaml_doc) :: doc
    type(yaml_value) :: root_value
    character(len=:), allocatable, dimension(:) :: keys
    logical :: success

    call doc%load("example.yaml", success)
    if (.not. success) then
        write(error_unit,*) 'Error: Failed to load YAML file'
        error stop
    end if

    root_value = doc%root
    keys = root_value%dict_val%keys()
    print *, "Root keys:", keys
end program
