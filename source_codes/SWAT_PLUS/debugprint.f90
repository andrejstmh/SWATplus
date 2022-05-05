#ifdef DEBUG
    subroutine debugprint(nr, stype, value)
        !use parm
        use time_module
        integer, intent (in) :: nr
        character(len=*),intent(in) :: stype
        real, intent (in) :: value
        write(7743, '(I0,"'//achar(9)//'",I0,"'//achar(9)//'",I0,"'//achar(9)//'",A,"'//achar(9)//'",G0)') &
        &time%yrc, time%day, nr, stype, value
        write(*, '(I0,"'//achar(9)//'",I0,"'//achar(9)//'",I0,"'//achar(9)//'",A,"'//achar(9)//'",G0)') &
        &time%yrc, time%day, nr, stype, value
        return
    end
#else
    subroutine debugprint(nr, stype, value)
        integer, intent (in) :: nr
        character(len=*),intent(in) :: stype
        real, intent (in) :: value
        return
    end
#endif