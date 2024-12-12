module test_fyaml
    use test_utils
    use fyaml
    use iso_fortran_env, only: error_unit
    implicit none

    contains

    subroutine run_all_tests(status)
        integer, intent(inout) :: status

        print *, "Running Basic Loading Test..."
        status = test_basic_loading()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Basic Loading Test Failed with status:", status
        else
            print *, "Basic Loading Test Passed."
        endif

        print *, "Running Basic Types Test..."
        status = test_basic_types()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Basic Types Test Failed with status:", status
        else
            print *, "Basic Types Test Passed."
        endif

        print *, "Running Sequences Test..."
        status = test_sequences()
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Sequences Test Failed with status:", status
        else
            print *, "Sequences Test Passed."
        endif

        ! Add additional tests here following the same pattern

        print *, "All tests completed."
    end subroutine run_all_tests

end module test_fyaml

program test_fyaml_main
    use test_fyaml
    implicit none

    integer :: status

    call run_all_tests(status)
    if (status /= 0) error stop
end program test_fyaml_main
