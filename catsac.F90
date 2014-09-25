!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! concatnate sac files
!!
!! @par Usage
!! catsac.x [sacfile list] (-o outfile)
!!
!! Options
!!   -o: Specify output sac filename. Default is cat.sac
!!
!<
!! --
program catsac

  !! -- Dependency
  use m_std
  use m_sac
  use m_system
  use m_getopt
  use m_daytim

  !! -- Declarations
  implicit none

  integer, parameter    :: N_MAXFILE = 10000
  
  integer               :: nfile
  type(sac__hdr)        :: sh(N_MAXFILE)
  real(DP), allocatable :: dat(:)
  real(DP), allocatable :: catdat(:)
  integer               :: itim_min, itim_max
  logical               :: is_opt
  logical               :: is_exist
  character(256)        :: fn_out
  character(256)        :: fn_tmp
  character(256)        :: fn_sac(N_MAXFILE)
  integer :: ib, ie
  integer :: i
  integer :: npta
  
  call getopt( 'o', is_opt, fn_out, 'cat.sac' )

  nfile = 0
  do i=1, system__iargc()

     call getarg(i, fn_tmp)

     if( trim(fn_tmp) == '-o'         ) cycle
     if( trim(fn_tmp) == trim(fn_out) ) cycle

     inquire( file=fn_tmp, exist=is_exist ) !! check if the file exist

     if( .not. is_exist ) then
        write(STDERR,*) "Input "//trim(fn_tmp)//" does not exist."
        cycle
     end if

     nfile = nfile + 1
     fn_sac(nfile) = trim(fn_tmp)

     !! read (only) header
     call sac__read( fn_sac(nfile), sh(nfile) )

  end do

  npta = sum( sh(1:nfile)%npts )
  allocate( catdat(npta) )

  do i=1, nfile
     call sac__read( fn_sac(i), sh(i), dat )

     if( i==1 ) then
        ib = 1
     else
        ib = sum( sh(1:i-1)%npts ) + 1
     end if
     ie = ib + sh(i)%npts - 1
     catdat(ib:ie) = dat(1:sh(i)%npts)

     deallocate(dat)
  end do

  sh(1)%npts = npta
  sh(1)%e = sh(1)%b + (sh(1)%npts-1)*sh(1)%delta
  sh(1)%depmin = minval( catdat )
  sh(1)%depmax = maxval( catdat )

  call sac__write( fn_out, sh(1), catdat )
  deallocate( catdat )
end program catsac
!! ----------------------------------------------------------------------------------------------------------------------------- !!
