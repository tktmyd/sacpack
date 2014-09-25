!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! Convert sac data in double-precision binary format
!!
!! output binary file can be used as input of psxy with -bi option
!<
!! --
program sac2bin

  !! -- Dependency
  use m_std
  use m_sac
  use m_system

  !! -- Declarations
  implicit none
  
  character(256)        :: fn_in, fn_out
  type(sac__hdr)        :: sh
  real(DP), allocatable :: dat(:)
  real(DP), allocatable :: odat(:,:)
  integer               :: i
  integer               :: io
  integer               :: ierr
  !! ----

  !! Command-line option processing
  if( system__iargc() /= 2 ) then
     write(STDERR,'(A)') 'usage: sac2bin.x sacfile outputfile'
     write(STDERR,'(A)')
     write(STDERR,'(A)') 'The output binary file can be used as input of psxy with -bi option'
     write(STDERR,'(A)')
     stop
  end if

  call sac__read( fn_in, sh, dat )
  allocate(odat(2,1:sh%npts) )

  !! check if origin time header "o" is used
  if( abs( sh%o + 12345.0 ) > epsilon(1.0) ) then
     do i=1, sh%npts
        odat(1,i) = (i-1)*sh%delta - sh%o + sh%b
     end do
  else
     do i=1, sh%npts
        odat(1,i) = (i-1)*sh%delta + sh%b
     end do
  end if
  odat(2,1:sh%npts) = dat(1:sh%npts)

  call std__getio( io )
  open( io, file=trim(fn_out), action='write', status='unknown', iostat=ierr, access='stream' )
  if( ierr /= 0 ) then
     write(STDERR,'(A)') 'Output file ' // trim( fn_out ) // ' cannot be created. Abort.'
     stop
  end if
  
  write(io) odat
  close(io)
  
  deallocate( dat, odat )

end program sac2bin
!! ----------------------------------------------------------------------------------------------------------------------------- !!
