module test_utils
    use fyaml
    use iso_fortran_env, only: error_unit
    implicit none

    ! Error codes and debug levels
    integer, parameter :: ERR_SUCCESS = 0
    integer, parameter :: ERR_ALLOC = 1
    integer, parameter :: ERR_ASSERT = 2
    integer, parameter :: DEBUG = 1

    ! Test data parameters
    integer, parameter :: flow_seq_int(3) = [1, 2, 4]
    real, parameter :: flow_seq_real(3) = [1.1, 2.2, 3.3]
    logical, parameter :: flow_seq_log(3) = [.true., .false., .true.]
    character(len=5), parameter :: flow_seq_str(3) = ["one  ", "two  ", "three"]

    ! Test keys
    character(len=*), parameter :: KEY_COMPANY = "company"
    character(len=*), parameter :: KEY_FLOW_SEQ = "flow_sequence"
    character(len=*), parameter :: KEY_FLOW_REAL = "flow_sequence_real"
    character(len=*), parameter :: KEY_FLOW_LOG = "flow_sequence_logical"
    character(len=*), parameter :: KEY_FLOW_STR = "flow_sequence_string"

    interface assert_equal
        module procedure assert_equal_int
        module procedure assert_equal_string
        module procedure assert_equal_logical
        module procedure assert_equal_real
    end interface

contains
    subroutine assert_equal_int(expected, actual, message, status)
        integer, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        integer, intent(out) :: status
        status = ERR_SUCCESS
        ! write(*,*) 'Comparing: ', expected, actual
        if (expected /= actual) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if
    end subroutine

    ! Update real assertion to match interface
    subroutine assert_equal_real(expected, actual, message, status)
        real, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        integer, intent(out) :: status
        real, parameter :: tolerance = 1.0e-6

        status = ERR_SUCCESS
        if (abs(expected - actual) > tolerance) then
            write(error_unit,*) "FAILED:", message
            write(error_unit,*) "Expected:", expected, " Got:", actual
            status = ERR_ASSERT
        endif
    end subroutine assert_equal_real

    subroutine assert_equal_logical(expected, actual, message, status)
        logical, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        integer, intent(out) :: status
        if (.not. (expected .eqv. actual)) then
            write(error_unit,*) 'FAILED: ', message
            write(error_unit,*) 'Expected: ', expected, ' Got: ', actual
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if
    end subroutine

    subroutine assert_equal_string(expected, actual, message, status)
        character(len=*), intent(in) :: expected, actual, message
        character(len=:), allocatable :: exp_val, act_val
        integer :: alloc_status
        integer, intent(out) :: status

        call allocate_string_value(exp_val, expected, alloc_status)
        if (alloc_status /= 0) then
            write(error_unit,*) 'FAILED: Memory allocation error for expected value'
            status = ERR_ALLOC
            return
        end if

        call allocate_string_value(act_val, actual, alloc_status)
        if (alloc_status /= 0) then
            print *, 'FAILED: Memory allocation error for actual value'
            if (allocated(exp_val)) deallocate(exp_val)
            status = ERR_ALLOC
            return
        end if

        if (exp_val /= act_val) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', trim(exp_val), ' Got: ', trim(act_val)
            status = ERR_ASSERT
        else
            status = ERR_SUCCESS
        end if

        if (allocated(exp_val)) deallocate(exp_val)
        if (allocated(act_val)) deallocate(act_val)
    end subroutine

    ! Helper for safe string allocation
    subroutine safe_allocate_string(str, length, status)
        character(len=:), allocatable, intent(out) :: str
        integer, intent(in) :: length
        integer, intent(out) :: status

        if (allocated(str)) deallocate(str)
        allocate(character(len=length) :: str, stat=status)
        if (status /= 0) then
            write(error_unit,*) "Failed to allocate string of length", length
        end if
    end subroutine

    ! Allocation helper
    subroutine allocate_string_value(val, str, status)
        character(len=:), allocatable, intent(out) :: val
        character(len=*), intent(in) :: str
        integer, intent(out) :: status

        if (allocated(val)) deallocate(val)
        allocate(character(len=len_trim(str)) :: val, stat=status)
        if (status == 0) then
            val = trim(str)
        end if
    end subroutine

    ! Test routines returning status
    integer function test_basic_loading()
        type(fyaml_doc) :: doc
        character(len=100) :: filename = "test_example.yaml"
        logical :: success

        test_basic_loading = ERR_SUCCESS
        write(*,*) 'Loading file:', trim(filename)
        call doc%load(filename, success)
        if (.not. success) then
            write(error_unit,*) 'Error loading YAML document.'
            test_basic_loading = ERR_ALLOC
            return
        endif
        write(*,*) 'YAML document loaded successfully.'
    end function

    integer function test_basic_types()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        type(yaml_dict) :: company
        character(len=:), allocatable :: key
        integer :: status

        test_basic_types = ERR_SUCCESS
        call safe_allocate_string(key, 20, status)
        if (status /= 0) then
            write(error_unit,*) "Failed to allocate key string"
            test_basic_types = ERR_ALLOC
            return
        endif

        call doc%load("test_example.yaml")

        key = "company"
        val = doc%root%get(key)
        if (.not. associated(val%dict_val)) then
            write(error_unit,*) "Failed to get company dictionary"
            test_basic_types = ERR_ASSERT
            return
        endif
        company = val%dict_val
        ! Assuming keys() returns an integer for size
        call assert_equal(11, size(company%keys()), "Number of keys in company", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        ! Additional type assertions...
        ! Example:
        key = "name"
        val = company%get(key)
        call assert_equal("Example Corp", val%str_val, "String value test", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        key = "founded"
        val = company%get(key)
        call assert_equal(2001, val%int_val, "Integer value test", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        key = "employees"
        val = company%get(key)
        call assert_equal(150, val%int_val, "Integer value test", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        key = "pi"
        val = company%get(key)
        call assert_equal(3.14159, val%real_val, "Real value test", status=status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        key = "goodness"
        val = company%get(key)
        call assert_equal(.true., val%is_null, "Null value test", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        key = "okay"
        val = company%get(key)
        call assert_equal(.true., val%bool_val, "Boolean value test", status)
        if (status /= ERR_SUCCESS) then
            test_basic_types = status
            return
        endif

        if (allocated(key)) deallocate(key)

    end function test_basic_types

    ! Test all sequence types
    function test_sequences() result(status)
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        type(yaml_dict) :: company
        character(len=:), allocatable :: key
        integer :: i, status

        status = ERR_SUCCESS
        call doc%load("test_example.yaml")

        key = "company"
        val = doc%root%get(key)
        company = val%dict_val

        ! Test integer sequence
        val = company%get("flow_sequence")
        if (.not. allocated(val%int_sequence)) then
            write(error_unit,*) "Integer sequence not allocated"
            status = ERR_ASSERT
            return
        endif
        write(*,*) "Processing integer sequence"
        call assert_equal(size(flow_seq_int), size(val%int_sequence), "Integer sequence size", status)
        do i = 1, size(flow_seq_int)
            call assert_equal(flow_seq_int(i), val%int_sequence(i), "Integer element", status)
        end do

        ! Test real sequence
        write(*,*) 'Getting real sequence'
        val = company%get(KEY_FLOW_REAL)

        ! Add debug output
        write(*,*) "Real sequence value type:", val%value_type
        write(*,*) "Is real sequence allocated:", allocated(val%real_sequence)

        if (.not. allocated(val%real_sequence)) then
            write(error_unit,*) "Real sequence not allocated"
            status = ERR_ASSERT
            return
        endif

        write(*,*) "Processing real sequence"
        call assert_equal(size(flow_seq_real), size(val%real_sequence), "Real sequence size", status)
        if (status /= ERR_SUCCESS) then
            write(error_unit,*) "Real sequence size mismatch"
            return
        endif

        do i = 1, size(flow_seq_real)
            write(error_unit,*) "Comparing real elements:", flow_seq_real(i), val%real_sequence(i)
            call assert_equal(flow_seq_real(i), val%real_sequence(i), "Real element", status)
            if (status /= ERR_SUCCESS) then
                write(error_unit,*) "Failed on real element", i
                return
            end if
        end do

        ! Test logical sequence
        val = company%get("flow_sequence_logical")
        if (.not. allocated(val%bool_sequence)) then
            write(error_unit,*) "Boolean sequence not allocated"
            status = ERR_ASSERT
            return
        endif
        write(*,*) "Processing logical sequence"
        call assert_equal(size(flow_seq_log), size(val%bool_sequence), "Logical sequence size", status)
        do i = 1, size(flow_seq_log)
            call assert_equal(flow_seq_log(i), val%bool_sequence(i), "Logical element", status)
        end do
    end function test_sequences
    ! Additional test functions...

end module test_utils
