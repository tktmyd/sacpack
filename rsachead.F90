!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! Read SAC header and display to standard output. Compatible to official saclst command
!!
!! @usage rsachead.x  sacfile  header_name  -v 
!<
!! --
program rsachead

  !! -- Dependency
  use m_std
  use m_sac
  use m_system
  use m_getopt

  !! -- Declarations
  implicit none

  
  character(256) :: fn_sac
  type(sac__hdr) :: sh
  character(16)  :: hdr
  integer        :: io
  integer        :: narg
  logical        :: is_all
  logical        :: is_var
  !! ----

  if( system__iargc() /= 2 .and. system__iargc() /= 3 ) then
     write(STDERR,'(A)') "rsh.exe <sacfile> [header_name] (-v)"
     write(STDERR,'(A)') " -v: shows variable only. default is (header_name = var)" 
     stop
  end if

  call system__getarg( 1, fn_sac )
  call system__getarg( 2, hdr    )
  call getopt( 'v', is_var )
  call sac__read( fn_sac, sh )

  if( trim(hdr) == 'ALL' .or. trim(hdr) == 'all' ) then
     is_all = .true.
  else
     is_all = .false.
  end if


  if( is_var ) then
     
     if( chk( hdr,'DELTA'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%delta
     if( chk( hdr,'DEPMIN' ) .or. is_all ) write(STDOUT,'(F12.4)') sh%depmin
     if( chk( hdr,'DEPMAX' ) .or. is_all ) write(STDOUT,'(F12.4)') sh%depmax
     if( chk( hdr,'B'      ) .or. is_all ) write(STDOUT,'(F12.4)') sh%b
     if( chk( hdr,'E'      ) .or. is_all ) write(STDOUT,'(F12.4)') sh%e
     if( chk( hdr,'O'      ) .or. is_all ) write(STDOUT,'(F12.4)') sh%o
     if( chk( hdr,'A'      ) .or. is_all ) write(STDOUT,'(F12.4)') sh%a
     if( chk( hdr,'STLA'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%stla
     if( chk( hdr,'STLO'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%stlo
     if( chk( hdr,'STEL'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%stel
     if( chk( hdr,'STDP'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%stdp
     if( chk( hdr,'EVLA'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%evla
     if( chk( hdr,'EVLO'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%evlo
     if( chk( hdr,'EVEL'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%evel
     if( chk( hdr,'EVDP'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%evdp
     if( chk( hdr,'MAG'    ) .or. is_all ) write(STDOUT,'(F12.4)') sh%mag
     if( chk( hdr,'DIST'   ) .or. is_all ) write(STDOUT,'(F12.4)') sh%dist
     if( chk( hdr,'AZ'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%az
     if( chk( hdr,'BAZ'    ) .or. is_all ) write(STDOUT,'(F12.4)') sh%baz
     if( chk( hdr,'GCARC'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%gcarc
     if( chk( hdr,'DEPMEN' ) .or. is_all ) write(STDOUT,'(F12.4)') sh%depmen
     if( chk( hdr,'CMPAZ'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%cmpaz
     if( chk( hdr,'CMPINC' ) .or. is_all ) write(STDOUT,'(F12.4)') sh%cmpinc
     if( chk( hdr,'NZYEAR' ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%nzyear
     if( chk( hdr,'NZJDAY' ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%nzjday
     if( chk( hdr,'NZHOUR' ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%nzhour
     if( chk( hdr,'NZMIN'  ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%nzmin
     if( chk( hdr,'NZSEC'  ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%nzsec
     if( chk( hdr,'NZMSEC' ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%nzmsec
     if( chk( hdr,'NPTS'   ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%npts
     if( chk( hdr,'IFTYPE' ) .or. is_all ) write(STDOUT,'(I7)'   ) sh%iftype
     if( chk( hdr,'KSTNM'  ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%kstnm)
     if( chk( hdr,'KEVNM'  ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%kevnm)
     if( chk( hdr,'KCMPNM' ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%kcmpnm)
     
     if( chk( hdr,'T0'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T0
     if( chk( hdr,'T1'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T1
     if( chk( hdr,'T2'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T2
     if( chk( hdr,'T3'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T3
     if( chk( hdr,'T4'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T4
     if( chk( hdr,'T5'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T5
     if( chk( hdr,'T6'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T6
     if( chk( hdr,'T7'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T7
     if( chk( hdr,'T8'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T8
     if( chk( hdr,'T9'     ) .or. is_all ) write(STDOUT,'(F12.4)') sh%T9
     
     
     if( chk( hdr,'KT0'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT0)
     if( chk( hdr,'KT1'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT1)
     if( chk( hdr,'KT2'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT2)
     if( chk( hdr,'KT3'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT3)
     if( chk( hdr,'KT4'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT4)
     if( chk( hdr,'KT5'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT5)
     if( chk( hdr,'KT6'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT6)
     if( chk( hdr,'KT7'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT7)
     if( chk( hdr,'KT8'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT8)
     if( chk( hdr,'KT9'    ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KT9)
     
     if( chk( hdr,'USER0'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER0
     if( chk( hdr,'USER1'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER1
     if( chk( hdr,'USER2'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER2
     if( chk( hdr,'USER3'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER3
     if( chk( hdr,'USER4'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER4
     if( chk( hdr,'USER5'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER5
     if( chk( hdr,'USER6'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER6
     if( chk( hdr,'USER7'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER7
     if( chk( hdr,'USER8'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER8
     if( chk( hdr,'USER9'  ) .or. is_all ) write(STDOUT,'(F12.4)') sh%USER9
     
     if( chk( hdr,'KUSER0' ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KUSER0)
     if( chk( hdr,'KUSER1' ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KUSER1)
     if( chk( hdr,'KUSER2' ) .or. is_all ) write(STDOUT,'(A)'    ) trim(sh%KUSER2)
  else
     
     
     if( chk( hdr,'DELTA'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " DELTA   =  ", sh%delta
     if( chk( hdr,'DEPMIN' ) .or. is_all ) write(STDOUT,'(A,F12.4)') " DEPMIN  =  ", sh%depmin
     if( chk( hdr,'DEPMAX' ) .or. is_all ) write(STDOUT,'(A,F12.4)') " DEPMAX  =  ", sh%depmax
     if( chk( hdr,'B'      ) .or. is_all ) write(STDOUT,'(A,F12.4)') " B       =  ", sh%b
     if( chk( hdr,'E'      ) .or. is_all ) write(STDOUT,'(A,F12.4)') " E       =  ", sh%e
     if( chk( hdr,'O'      ) .or. is_all ) write(STDOUT,'(A,F12.4)') " O       =  ",sh%o
     if( chk( hdr,'A'      ) .or. is_all ) write(STDOUT,'(A,F12.4)') " A       =  ",sh%a
     if( chk( hdr,'STLA'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " STLA    =  ",sh%stla
     if( chk( hdr,'STLO'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " STLO    =  ",sh%stlo
     if( chk( hdr,'STEL'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " STEL    =  ",sh%stel
     if( chk( hdr,'STDP'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " STDP    =  ",sh%stdp
     if( chk( hdr,'EVLA'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " EVLA    =  ",sh%evla
     if( chk( hdr,'EVLO'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " EVLO    =  ",sh%evlo
     if( chk( hdr,'EVEL'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " EVEL    =  ",sh%evel
     if( chk( hdr,'EVDP'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " EVDP    =  ",sh%evdp
     if( chk( hdr,'MAG'    ) .or. is_all ) write(STDOUT,'(A,F12.4)') " MAG     =  ",sh%mag
     if( chk( hdr,'DIST'   ) .or. is_all ) write(STDOUT,'(A,F12.4)') " DIST    =  ",sh%dist
     if( chk( hdr,'AZ'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " AZ      =  ",sh%az
     if( chk( hdr,'BAZ'    ) .or. is_all ) write(STDOUT,'(A,F12.4)') " BAZ     =  ",sh%baz
     if( chk( hdr,'GCARC'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " GCARC   =  ",sh%gcarc
     if( chk( hdr,'DEPMEN' ) .or. is_all ) write(STDOUT,'(A,F12.4)') " DEPMEN  =  ",sh%depmen
     if( chk( hdr,'CMPAZ'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " CMPAZ   =  ",sh%cmpaz
     if( chk( hdr,'CMPINC' ) .or. is_all ) write(STDOUT,'(A,F12.4)') " CMPINC  =  ",sh%cmpinc
     if( chk( hdr,'NZYEAR' ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NZYEAR  =  ",sh%nzyear
     if( chk( hdr,'NZJDAY' ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NZJDAY  =  ",sh%nzjday
     if( chk( hdr,'NZHOUR' ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NZHOUR  =  ",sh%nzhour
     if( chk( hdr,'NZMIN'  ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NZMIN   =  ",sh%nzmin
     if( chk( hdr,'NZSEC'  ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NZSEC   =  ",sh%nzsec
     if( chk( hdr,'NZMSEC' ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NZMSEC  =  ",sh%nzmsec
     if( chk( hdr,'NPTS'   ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " NPTS    =  ",sh%npts
     if( chk( hdr,'IFTYPE' ) .or. is_all ) write(STDOUT,'(A,I7)'   ) " IFTYPE  =  ",sh%iftype
     if( chk( hdr,'KSTNM'  ) .or. is_all ) write(STDOUT,'(A)'      ) " KSTNM   =  '"//trim(sh%kstnm)//"'"
     if( chk( hdr,'KEVNM'  ) .or. is_all ) write(STDOUT,'(A)'      ) " KEVNM   =  '"//trim(sh%kevnm)//"'"
     if( chk( hdr,'KCMPNM' ) .or. is_all ) write(STDOUT,'(A)'      ) " KCMPNM  =  '"//trim(sh%kcmpnm)//"'"
     
     if( chk( hdr,'T0'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T0      =  ", sh%T0
     if( chk( hdr,'T1'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T1      =  ", sh%T1
     if( chk( hdr,'T2'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T2      =  ", sh%T2
     if( chk( hdr,'T3'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T3      =  ", sh%T3
     if( chk( hdr,'T4'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T4      =  ", sh%T4
     if( chk( hdr,'T5'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T5      =  ", sh%T5
     if( chk( hdr,'T6'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T6      =  ", sh%T6
     if( chk( hdr,'T7'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T7      =  ", sh%T7
     if( chk( hdr,'T8'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T8      =  ", sh%T8
     if( chk( hdr,'T9'     ) .or. is_all ) write(STDOUT,'(A,F12.4)') " T9      =  ", sh%T9
     
     
     if( chk( hdr,'KT0'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT0     =  '"//trim(sh%KT0)//"'"
     if( chk( hdr,'KT1'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT1     =  '"//trim(sh%KT1)//"'"
     if( chk( hdr,'KT2'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT2     =  '"//trim(sh%KT2)//"'"
     if( chk( hdr,'KT3'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT3     =  '"//trim(sh%KT3)//"'"
     if( chk( hdr,'KT4'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT4     =  '"//trim(sh%KT4)//"'"
     if( chk( hdr,'KT5'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT5     =  '"//trim(sh%KT5)//"'"
     if( chk( hdr,'KT6'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT6     =  '"//trim(sh%KT6)//"'"
     if( chk( hdr,'KT7'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT7     =  '"//trim(sh%KT7)//"'"
     if( chk( hdr,'KT8'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT8     =  '"//trim(sh%KT8)//"'"
     if( chk( hdr,'KT9'    ) .or. is_all ) write(STDOUT,'(A)'      ) " KT9     =  '"//trim(sh%KT9)//"'"
     
     if( chk( hdr,'USER0'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER0   =  ", sh%USER0
     if( chk( hdr,'USER1'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER1   =  ", sh%USER1
     if( chk( hdr,'USER2'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER2   =  ", sh%USER2
     if( chk( hdr,'USER3'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER3   =  ", sh%USER3
     if( chk( hdr,'USER4'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER4   =  ", sh%USER4
     if( chk( hdr,'USER5'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER5   =  ", sh%USER5
     if( chk( hdr,'USER6'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER6   =  ", sh%USER6
     if( chk( hdr,'USER7'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER7   =  ", sh%USER7
     if( chk( hdr,'USER8'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER8   =  ", sh%USER8
     if( chk( hdr,'USER9'  ) .or. is_all ) write(STDOUT,'(A,F12.4)') " USER9   =  ", sh%USER9
     
     if( chk( hdr,'KUSER0' ) .or. is_all ) write(STDOUT,'(A)'      ) " KUSER0  =  '"//trim(sh%KUSER0)//"'"
     if( chk( hdr,'KUSER1' ) .or. is_all ) write(STDOUT,'(A)'      ) " KUSER1  =  '"//trim(sh%KUSER1)//"'"
     if( chk( hdr,'KUSER2' ) .or. is_all ) write(STDOUT,'(A)'      ) " KUSER2  =  '"//trim(sh%KUSER2)//"'"

  end if


  

contains

  function chk( c_in, c_ref )
    
    character(*), intent(in) :: c_in 
    character(*), intent(in) :: c_ref
    logical :: chk
    character(256) :: c_in2

    c_in2=trim(c_in)
    call lw2up(c_in2)

    if( trim(c_in) == trim(c_ref) ) then
       chk = .true.
    else if ( trim(c_in2) == trim(c_ref) ) then
       chk = .true.
    else
       chk = .false.
    end if
  end function chk
    
    
  subroutine up2lw(cc)

    ! Upper case to lower case converter
    
    character(*), intent(inout) :: cc
    integer :: n
    integer :: i
    n=len_trim(cc)
    
    do i=1, n
       if( 65 <= ichar(cc(i:i)) .and. ichar(cc(i:i)) <= 90 ) then
          cc(i:i) = char(ichar(cc(i:i))+32)
       end if
    end do

    write(STDERR,*) trim(cc)
  end subroutine up2lw

  subroutine lw2up(cc)
    ! lower case to upper case converter
    character(*), intent(inout) :: cc
    integer :: n
    integer :: i
    n=len_trim(cc)
    
    do i=1, n
       if( 97 <= ichar(cc(i:i)) .and. ichar(cc(i:i)) <=1220 ) then
          cc(i:i) = char(ichar(cc(i:i))-32)
       end if
    end do

  end subroutine lw2up
  
  

  
end program rsachead
!------------------------------------------------------------------------------!
