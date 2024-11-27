module test_utils
    use fyaml
    use iso_fortran_env, only: error_unit
    implicit none

    ! Error codes and debug levels
    integer, parameter :: ERR_SUCCESS = 0
    integer, parameter :: ERR_ALLOC = 1
    integer, parameter :: DEBUG = 1

    interface assert_equal
        module procedure assert_equal_int
        module procedure assert_equal_real
        module procedure assert_equal_logical
        module procedure assert_equal_string
    end interface

contains
    subroutine assert_equal_int(expected, actual, message)
        integer, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        if (expected /= actual) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            error stop
        end if
    end subroutine

    subroutine assert_equal_real(expected, actual, message, tolerance)
        real, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        real, intent(in), optional :: tolerance
        real :: tol
        tol = 1.0e-6
        if (present(tolerance)) tol = tolerance
        if (abs(expected - actual) > tol) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            error stop
        end if
    end subroutine

    subroutine assert_equal_logical(expected, actual, message)
        logical, intent(in) :: expected, actual
        character(len=*), intent(in) :: message
        if (expected .neqv. actual) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', expected, ' Got: ', actual
            error stop
        end if
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
        endif
    end subroutine

    ! Add allocation helper
    subroutine allocate_string_value(val, str, status)
        character(len=:), allocatable, intent(out) :: val
        character(len=*), intent(in) :: str
        integer, intent(out) :: status

        if (allocated(val)) deallocate(val)
        allocate(character(len=len_trim(str)) :: val, stat=status)
        if (status == 0) then
            val = trim(str)
        endif
    end subroutine

    ! Update string comparison with allocation
    subroutine assert_equal_string(expected, actual, message)
        character(len=*), intent(in) :: expected, actual, message
        character(len=:), allocatable :: exp_val, act_val
        integer :: status

        call allocate_string_value(exp_val, expected, status)
        if (status /= 0) then
            print *, 'FAILED: Memory allocation error for expected value'
            error stop
        endif

        call allocate_string_value(act_val, actual, status)
        if (status /= 0) then
            print *, 'FAILED: Memory allocation error for actual value'
            if (allocated(exp_val)) deallocate(exp_val)
            error stop
        endif

        if (exp_val /= act_val) then
            print *, 'FAILED: ', message
            print *, 'Expected: ', trim(exp_val), ' Got: ', trim(act_val)
            if (allocated(exp_val)) deallocate(exp_val)
            if (allocated(act_val)) deallocate(act_val)
            error stop
        endif

        if (allocated(exp_val)) deallocate(exp_val)
        if (allocated(act_val)) deallocate(act_val)
    end subroutine

    subroutine test_basic_types()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        type(yaml_dict) :: company
        character(len=:), allocatable :: key
        integer :: status

        ! Allocate key with sufficient length
        call safe_allocate_string(key, 20, status)
        if (status /= 0) then
            write(error_unit,*) "Failed to allocate key string"
            return
        endif

        call doc%load("test_example.yaml")

        key = "company"
        val = doc%root%get(key)
        if (.not. associated(val%dict_val)) then
            write(error_unit,*) "Failed to get company dictionary"
            error stop
        endif
        company = val%dict_val
        print *, company%keys()

        key = "name"
        val = company%get(key)
        print *, val%value_type
        if (.not. allocated(val%str_val)) then
            write(error_unit,*) "String value .name not allocated"
            error stop
        endif
        call assert_equal("Example Corp", val%str_val, "String value test")

        key = "founded"
        val = company%get(key)
        call assert_equal(2001, val%int_val, "Integer value test")

        key = "employees"
        val = company%get(key)
        call assert_equal(150, val%int_val, "Integer value test")

        key = "pi"
        val = company%get(key)
        call assert_equal(3.14159, val%real_val, "Real value test")

        key = "okay"
        val = company%get(key)
        call assert_equal(.true., val%bool_val, "Boolean value test")

        key = "goodness"
        val = company%get(key)
        call assert_equal(.true., val%is_null, "Null value test")

        if (allocated(key)) deallocate(key)
    end subroutine

    subroutine test_sequences()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        character(len=:), allocatable :: key
        integer :: status

        call safe_allocate_string(key, 20, status)
        if (status /= 0) return

        call doc%load("test_example.yaml")

        key = "person"
        val = doc%root%get(key)
        if (.not. associated(val%dict_val)) goto 100

        key = "skills"
        val = val%get(key)
        if (.not. allocated(val%sequence)) goto 100

        call assert_equal(3, size(val%sequence), "Sequence size test")
        call assert_equal("R", val%sequence(1), "Sequence item 1 test")
        call assert_equal("SQL", val%sequence(2), "Sequence item 2 test")
        call assert_equal("Python", val%sequence(3), "Sequence item 3 test")

        100 continue
        if (allocated(key)) deallocate(key)
    end subroutine

    subroutine test_nested_structures()
        type(fyaml_doc) :: doc
        type(yaml_value) :: val
        character(len=:), allocatable :: key
        integer :: status

        call safe_allocate_string(key, 50, status)  ! Longer for nested paths
        if (status /= 0) return

        call doc%load("test_example.yaml")

        ! Test direct nested access
        key = "person.name"
        val = doc%root%get(key)
        if (allocated(val%str_val)) then
            call assert_equal("Jane Doe", val%str_val, "Nested string test")
        endif

        key = "person.age"
        val = doc%root%get(key)
        call assert_equal(25, val%int_val, "Nested integer test")

        if (allocated(key)) deallocate(key)
    end subroutine

    subroutine test_dict_operations()
        type(fyaml_doc) :: doc
        type(yaml_dict), pointer :: dict
        type(yaml_value) :: val
        character(len=:), allocatable, dimension(:) :: keys

        call doc%load("test_example.yaml")

        ! Get person dictionary first
        val = doc%root%get("person")
        if (associated(val%dict_val)) then
            ! Now we can get keys from the dictionary
            dict => val%dict_val
            keys = dict%keys()

            ! Test dictionary operations
            call assert_equal(4, size(keys), "Number of keys in person")

            ! Additional assertions for specific keys can be added here
        end if
    end subroutine

end module test_utils
