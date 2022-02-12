c subplex.f
c Subplex for the xi Tau problem.
c Miroslav Broz (miroslav.broz@email.cz), Feb 12th 2022

      program simplex

      implicit none
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'

      integer i
      real*8 x(NDIMMAX), e(NDIMMAX), e_param(NDIMMAX)
      real*8 tol1, tol2, tolfact
      integer iter1, iter2, iterincr

c internal
      integer nsmin, nsmax
      parameter(nsmin = min(2,NDIMMAX))
      parameter(nsmax = min(5,NDIMMAX))
      integer iwork(NDIMMAX+int(NDIMMAX/nsmin))
      real*8 work(2*NDIMMAX+nsmax*(nsmax+4)+1)
      integer iflag, mode, mdcont, mduser, mdsing, maxiter, numiter
      real*8 tol, y
      character*80 str

c functions
      external func
c
c  print version
c
      call util_version
c
c  read input parameters (and write input to terminal)
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

      write(*,*) "# e_param() : "
      read(*,*,err=990,end=990) (e_param(i), i=1,nparam)
      do i = 1,nparam
        write(*,*) "# e_param(", i, ") = ", e_param(i)
      enddo

      write(*,*) "# variable() : "
      read(*,*,err=990,end=990) (variable(i), i=1,nparam)
      do i = 1,nparam
        write(*,*) "# variable(", i, ") = ", variable(i)
      enddo

      call read_dependent()

      write(*,*) "# tol1 : "
      read(*,*,err=990,end=990) tol1
      write(*,*) "# tol1 = ", tol1

      write(*,*) "# tol2 : "
      read(*,*,err=990,end=990) tol2
      write(*,*) "# tol2 = ", tol2

      write(*,*) "# tolfact : "
      read(*,*,err=990,end=990) tolfact
      write(*,*) "# tolfact = ", tolfact

      write(*,*) "# iter1 : "
      read(*,*,err=990,end=990) iter1
      write(*,*) "# iter1 = ", iter1

      write(*,*) "# iter2 : "
      read(*,*,err=990,end=990) iter2
      write(*,*) "# iter2 = ", iter2

      write(*,*) "# iterincr : "
      read(*,*,err=990,end=990) iterincr
      write(*,*) "# iterincr = ", iterincr

c  resolve variable/fixed parameters
      ndim = 0
      do i = 1,nparam
        if (variable(i)) then
          ndim = ndim + 1
          x(ndim) = x_param(i)
          e(ndim) = e_param(i)
        endif
      enddo

      if (debug) then
        write(*,*) '# ndim = ', ndim
      endif

c  pass the rest in common block /dependent/ 

      mdcont = 0  ! continuation
      mduser = 0  ! user-defined
      mdsing = 0  ! single-step
      tol = tol1
      maxiter = iter1

      do while ((maxiter.lt.iter2).and.(tol.gt.tol2).and.
     :  (iflag.ne.-2).and.(iflag.ne.1))

        mode = 4*mdsing + 2*mduser + mdcont

        call subplx(func,ndim,tol,maxiter,mode,e,x,y,numiter,
     :    work,iwork,iflag)

        mdcont = 1
        if (iflag.eq.-1) then
          maxiter = maxiter + iterincr
        else if (iflag.eq.0) then
          tol = tol*tolfact
        endif

c  write intermediate results
        write(*,*) "# iflag = ", iflag, " # -1 .. maxiter, 0 .. tol"
        write(*,*) "# numiter = ", numiter
        write(*,*) "# maxiter = ", maxiter
        write(*,*) "# tol = ", tol

      enddo

c  write the result of minimalisation
      write(*,*) "x = ", x
      write(*,*) "y = ", y

      stop

c  error handlers
990   continue
      write(*,*) 'subplex: Error reading standard input.'

      end

      double precision function func(n,x)
      implicit none
      integer n
      double precision x(n)
      double precision chi2_func
      func = chi2_func(x)
      return
      end


