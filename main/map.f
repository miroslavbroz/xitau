c map.f
c Calculate a chi^2 map (1-dimensional).
c Miroslav Broz (miroslav.broz@email.cz), Nov 16th 2022

      program map

      implicit none
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'

      integer whichparam
      real*8 val1, val2, step

      integer i
      real*8 x(NDIMMAX), chi2, val, eps
      character*80 str
      real*8 chi2_func
      parameter(eps = 1.0e-8)
c
c  read input parameters
c
      write(*,*) "# nparam : "
5     continue
        read(*,10,err=990,end=990) str
10      format(a)
      if (str(1:1).eq.'#') goto 5

      read(str,*,err=990,end=990) nparam
      if (nparam.gt.NDIMMAX) then
        write(*,*) 'Error: nparam > NDIMMAX = ', NDIMMAX
        stop
      endif
      write(*,*) "# nparam = ", nparam

      write(*,*) "# x_param() : "
      read(*,*,err=990,end=990) (x_param(i), i=1,nparam)
      do i = 1, nparam
        write(*,*) "# x_param(", i, ") = ", x_param(i)
      enddo

      call read_dependent()

      write(*,*) "# whichparam :"
      read(*,*,err=990,end=990) whichparam

      write(*,*) "# val1 val2 step :"
      read(*,*,err=990,end=990) val1, val2, step 

      ndim = nparam
      do i = 1, nparam
        x(i) = x_param(i)
        variable(i) = .TRUE.
      enddo
c
c  calculate chi^2 values
c
      val = val1
      do while (val.le.val2+eps)
        x(whichparam) = val
        chi2 = chi2_func(x)
        val = val+step
      enddo

      stop

c  error handlers
990   continue
      write(*,*) 'map1: Error reading standard input.'

      end


