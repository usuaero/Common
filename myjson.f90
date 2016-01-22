module myjson_m
    use json_m
    implicit none
    
    logical :: json_found
    
contains

!-----------------------------------------------------------------------------------------------------------
real function json_required_real(json,name)
    implicit none
    type(json_value),intent(in),pointer :: json
    character(len=*) :: name
    real :: value

    call json_get(json, name, value, json_found)
    if(json_failed()) then
        write(*,*) 'Error: Unable to read required value: ',name
        STOP
    end if

    json_required_real = value
end function json_required_real

!-----------------------------------------------------------------------------------------------------------
real function json_optional_real(json,name,default_value)
    implicit none
    type(json_value),intent(in),pointer :: json
    character(len=*) :: name
    real :: value, default_value

    call json_get(json, name, value, json_found)
    if((.not.json_found) .or. json_failed()) then
        write(*,*) trim(name),' set to ',default_value
        value = default_value
        call json_clear_exceptions()
    end if

    json_optional_real = value
end function json_optional_real

!-----------------------------------------------------------------------------------------------------------
integer function json_optional_integer(json,name,default_value)
    implicit none
    type(json_value),intent(in),pointer :: json
    character(len=*) :: name
    integer :: value, default_value

    call json_get(json, name, value, json_found)
    if((.not.json_found) .or. json_failed()) then
        write(*,*) trim(name),' set to ',default_value
        value = default_value
        call json_clear_exceptions()
    end if

    json_optional_integer = value
end function json_optional_integer

!-----------------------------------------------------------------------------------------------------------
real function json_file_required_real(json,name)
    implicit none
    type(json_file) :: json
    character(len=*) :: name
    real :: value

    call json%get(name, value)
    if(json_failed()) then
        write(*,*) 'Error: Unable to read required value: ',name
        STOP
    end if

    json_file_required_real = value
end function json_file_required_real

!-----------------------------------------------------------------------------------------------------------
real function json_file_optional_real(json,name,default_value)
    implicit none
    type(json_file) :: json
    character(len=*) :: name
    real :: value, default_value

    call json%get(name, value)
    if(json_failed()) then
        write(*,*) name,' set to ',default_value
        value = default_value
        call json_clear_exceptions()
    end if

    json_file_optional_real = value
end function json_file_optional_real

!-----------------------------------------------------------------------------------------------------------
integer function json_file_optional_integer(json,name,default_value)
    implicit none
    type(json_file) :: json
    character(len=*) :: name
    integer :: value, default_value

    call json%get(name, value)
    if(json_failed()) then
        write(*,*) trim(name),' set to ',default_value
        value = default_value
        call json_clear_exceptions()
    end if

    json_file_optional_integer = value
end function json_file_optional_integer

!-----------------------------------------------------------------------------------------------------------
subroutine json_check()
    if(json_failed()) then
        call print_json_error_message()
        STOP
    end if
end subroutine json_check
!-----------------------------------------------------------------------------------------------------------
subroutine print_json_error_message()
    implicit none
    character(len=:),allocatable :: error_msg
    logical :: status_ok

    !get error message:
    call json_check_for_errors(status_ok, error_msg)

    !print it if there is one:
    if (.not. status_ok) then
        write(*,'(A)') error_msg
        deallocate(error_msg)
        call json_clear_exceptions()
    end if

end subroutine print_json_error_message

end module myjson_m
