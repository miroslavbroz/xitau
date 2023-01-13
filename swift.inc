!*************************************************************************
!                        SWIFT.INC
!*************************************************************************
! Include file for SWIFT
!
! Author:  Hal Levison  
! Date:    2/2/93
! Last revision: 3/7/93
!
! Modified: Miroslav Broz, miroslav.broz@email.cz
! Date: Oct 10th 2001


        implicit NONE    ! you got it baby

!...   Version of Swift
       real*8 VER_NUM
       parameter(VER_NUM=2.0d0)

!...   Maximum array size
       integer  NPLMAX, NTPMAX
       parameter  (NPLMAX = 15)   ! max number of planets, including the Sun 
       parameter  (NTPMAX = 2001) ! max number of test particles

!...   Size of the test particle integer status flag
	integer NSTATP            ! Number of status parameters
        parameter  (NSTATP = 3)  
	integer NSTAT            ! Number of status parameters
        parameter  (NSTAT = NSTATP + NPLMAX - 1)  ! include one for @ planet  

!...   Size of the test particle integer status flag
	integer NSTATR    
        parameter  (NSTATR = NSTAT)  ! io_init_tp assumes NSTAT==NSTATR

!...   convergence criteria for danby
	real*8 DANBYAC , DANBYB
	parameter (DANBYAC= 1.0d-14, DANBYB = 1.0d-13)

!...    loop limits in the Laguerre attempts
	integer NLAG1, NLAG2
	parameter(NLAG1 = 50, NLAG2 = 400)

!...    A small number
	real*8 TINY
        PARAMETER(TINY=4.D-15)

!...    trig stuff
	real*8 PI,TWOPI,PIBY2,DEGRAD
        parameter(PI = 4.d0*atan(1.d0))
	parameter (TWOPI = 2.0D0 * PI)
	parameter (PIBY2 = PI/2.0D0)
	parameter (DEGRAD = 180.0D0 / PI)

!  number of steps in step_kdk_pl2, size of PL coordinates buffer
      integer INTMAX
      parameter (INTMAX = 3)

!...  Coefficients of the 2nd order integrator of type SBAB
      real*8 c_koef(INTMAX-1),d_koef(INTMAX)
      data c_koef /0.5d0, 0.5d0/
      data d_koef /0.16666666666666666667d0,                            &
     &             0.66666666666666666667d0,                            &
     &             0.16666666666666666667d0/

!  functions
      integer ibset
      logical btest

!-------------------------------------------------------------------------

