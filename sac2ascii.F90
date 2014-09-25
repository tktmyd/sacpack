!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! Convert sac data in ascii format. Result will be exported to standard output
!<
!! --
program sac2ascii

  !! -- Dependency
  use m_std
  use m_sac
  use m_system

  !! -- Declarations
  implicit none
  
  character(256)        :: fn
  type(sac__hdr)        :: sh
  real(DP), allocatable :: dat(:)
  real(DP), allocatable :: t(:)
  integer               :: i

  !! ----

  if( system__iargc() /= 1 ) then
     write(STDERR,'(A)') 'usage: sac2ascii.x sacfile'
     write(STDERR,'(A)')
     write(STDERR,'(A)') 'convert sac file in two-column ascii data of time and amplitude'
     write(STDERR,'(A)') 'result will be exported to standard output'
     write(STDERR,'(A)')
     stop
  end if

  call system__getarg( 1, fn )

  call sac__read( fn, sh, dat )
  allocate(t(1:sh%npts) )

  !! check if origin time header "o" is used
  if( abs( sh%o + 12345.0 ) > epsilon(1.0) ) then
     do i=1, sh%npts
        t(i) = (i-1)*sh%delta - sh%o + sh%b
     end do
  else
     do i=1, sh%npts
        t(i) = (i-1)*sh%delta + sh%b
     end do
  end if

  do i=1, sh%npts
     write(STDOUT,'(ES20.8,ES25.16 )') t(i), dat(i)
  end do

  deallocate( t, dat )

end program sac2ascii
!! ----------------------------------------------------------------------------------------------------------------------------- !!
