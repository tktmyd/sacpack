!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! create sac-formatted file from two-column ascii file
!!
!<
!! --
program ascii2sac

  !! -- Dependency
  use m_std
  use m_sac
  use m_system

  !! -- Declarations
  implicit none

  character(256)        :: fn_in, fn_sac
  type(sac__hdr)        :: sh
  real(DP), allocatable :: dat(:)
  integer               :: io
  real(DP)              :: t
  integer               :: ierr
  integer               :: i
  !! ----
  
  if( system__iargc() /= 2 ) then
     write(STDERR,*)
     write(STDERR,*) ' usage: ascii2sac.x infile outfile'
     write(STDERR,*) ' [infile]  : specify input two-column ascii file. First column is treated as time, second is amplitude'
     write(STDERR,*) ' [outfile] : specify output sac filename [out.sac]'
     write(STDERR,*)
     stop
  end if

  call system__getarg(1, fn_in )
  call system__getarg(2, fn_sac)
  
  call std__getio( io )
  open( io, file = trim( fn_in ), status='old', action='read', iostat = ierr )
  if( ierr /= 0 ) then
     write(STDERR,*) 'File '// trim(fn_in) // ' not open. '
     stop
  end if

  call sac__init( sh )
  call std__countline( io, sh%npts )

  allocate( dat(sh%npts) )

  read( io, *) sh%b,     dat(1)
  read( io, *) sh%delta, dat(2)
  sh%delta = sh%delta - sh%b
  do i=3, sh%npts
     read( io, * ) t, dat(i)
  end do
  sh%e = t
  sh%depmax = maxval( dat(:) )
  sh%depmin = minval( dat(:) )

  call sac__write( fn_sac, sh, dat, .true. )
  
end program ascii2sac
!! ----------------------------------------------------------------------------------------------------------------------------- !!
