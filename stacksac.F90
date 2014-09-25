!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! stack sac files
!!
!! @par Usage
!! stacksac.x (-n) [sacfile list] (-o outfile)
!! Options
!!   -n: Normalize all sacfiles by their maximum amplitude
!!   -o: Specify output sac filename. Default is stack.sac
!!
!! @par Example
!! A simple usage
!!   stacksac.x *.sac
!! will read all sac files from the current directory and create stack.sac as a result.
!! 
!<
!! --
program stacksac

  !! -- Dependency
  use m_std
  use m_sac
  use m_system
  use m_getopt
  
  !! -- Declarations
  implicit none

  integer, parameter    :: N_MAXFILE = 10000

  integer               :: nfile
  type(sac__hdr)        :: sh_in, sh_out
  character(256)        :: fn_tmp
  character(256)        :: fn_sac(N_MAXFILE)
  character(256)        :: fn_out
  integer               :: i
  logical               :: is_normalize
  logical               :: is_opt
  logical               :: is_exist
  real(DP), allocatable :: dat(:)
  real(DP), allocatable :: stackdat(:)
  !! ----

  !!
  !! argument processing
  !!
  call getopt('n', is_normalize )
  call getopt('o', is_opt, fn_out, 'stack.sac' )
  
  nfile = 0
  do i=1, system__iargc()

     call getarg(i,fn_tmp) !! assumes argument character as input file

     if( trim(fn_tmp) == '-o'         ) cycle
     if( trim(fn_tmp) == '-n'         ) cycle
     if( trim(fn_tmp) == trim(fn_out) ) cycle
     
     inquire( file=fn_tmp, exist=is_exist ) !! check if the file exist

     if( is_exist ) then
        nfile = nfile + 1
        fn_sac(nfile) = trim(fn_tmp)
     end if

  end do


  !!
  !! Read the first file
  !!
  call sac__read( fn_sac(1), sh_out, dat )
  if( is_normalize ) dat(:) = dat(:) / maxval( abs(dat(:)) )

  allocate( stackdat(sh_out%npts) )
  stackdat(:) = dat(:)

  deallocate(dat)

  
  !!
  !! do stacking
  !!
  do i=2, nfile
     
     call sac__read( fn_sac(i), sh_in, dat )
     if( sh_in%npts /= sh_out%npts ) then
        write(STDERR,*) "ERROR [stacksac]: SAC file size mismatch for "//trim(fn_sac(i))
        stop
     end if
     if( abs( sh_in%delta - sh_out%delta ) > 0.0001 ) then
        write(STDERR,*) "ERROR [stacksac]: SAC file sampling interval mismatch for "//trim(fn_sac(i))
        stop
     end if

     if( is_normalize ) dat(:) = dat(:) / maxval( abs(dat(:)) )
     stackdat(:) = stackdat(:) + dat(:)
     deallocate(dat)
  end do

  if( is_normalize ) stackdat(:) = stackdat(:) / maxval( abs(stackdat(:)) )
  call sac__write( fn_out, sh_out, stackdat, overwrite=.true. )

  deallocate( stackdat )
  
end program stacksac
!! ----------------------------------------------------------------------------------------------------------------------------- !!
