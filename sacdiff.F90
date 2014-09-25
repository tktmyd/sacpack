!! ----------------------------------------------------------------------------------------------------------------------------- !!
!>
!! Take diff between two SAC files
!!
!! @par Usage
!!
!!    sacdiff.x sacfile1 sacfile2 diffsac
!!
!!    The output file diffsac contains waveform amplitude of (sacfile1 - sacfile2)
!!
!<
!! --
program sacdiff

  use m_std
  use m_system
  use m_sac
  implicit none

  character(256) :: fn_in1, fn_in2, fn_diff
  real(DP), allocatable :: wav_in1(:), wav_in2(:), wav_diff(:)
  type(sac__hdr) :: sh_in1, sh_in2, sh_diff
  
  if( system__iargc () /= 3 ) then
     write(STDERR,*) "usage: sacdiff.x sacfile1 sacfile2 diffsac"
     stop
  end if

  call system__getarg(1, fn_in1)
  call system__getarg(2, fn_in2)
  call system__getarg(3, fn_diff)

  call sac__read( fn_in1, sh_in1, wav_in1 )
  call sac__read( fn_in2, sh_in2, wav_in2 )

  !! Check consistency
  if( ( abs( sh_in1%delta - sh_in2%delta ) > epsilon(1.0)  ) .or.  ( sh_in1%npts /= sh_in2%npts ) ) then
     write(STDERR,*) "inconsistent file header. abort."
     stop
  end if
  
  
  !! Create SAC header

  ! first initialize it by the header of input1
  sh_diff = sh_in1
  ! then, modify it
  sh_diff%kstnm = trim( sh_in1%kstnm ) //"-"//trim( sh_in1%kstnm )

  allocate( wav_diff( sh_diff%npts ) )
  wav_diff(:) = wav_in1(:) - wav_in2(:)

  sh_diff%depmin = minval(wav_diff)
  sh_diff%depmax = maxval(wav_diff)
  call sac__write( fn_diff, sh_diff, wav_diff, .true. )
  
end program sacdiff
!! ----------------------------------------------------------------------------------------------------------------------------- !!
