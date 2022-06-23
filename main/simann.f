c simann.f
c Simmulated annealing for the xi Tau problem.
c Miroslav Broz (miroslav.broz@email.cz), May 24th 2017

      program simann

      implicit none
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'
      include '../simplex/cb_itmax.inc'

      integer i, j, iter, mp, np, id(NDIMMAX+1)
      real*8 x(NDIMMAX), e(NDIMMAX), p(NDIMMAX+1,NDIMMAX), y(NDIMMAX+1),
     :  ftol, xtry(NDIMMAX), e_param(NDIMMAX)
      integer itertot, iter_at_temp
      real*8 pb(NDIMMAX), yb, temptr, eps_temptr
      character*80 str

      integer length
      real*8 chi2_func
c
c print version
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

      write(*,*) "# ftol : "
      read(*,*,err=990,end=990) ftol
      write(*,*) "# ftol = ", ftol

      write(*,*) "# ITMAX : "
      read(*,*,err=990,end=990) ITMAX
      write(*,*) "# ITMAX = ", ITMAX

      write(*,*) "# iter_at_temp : "
      read(*,*,err=990,end=990) iter_at_temp
      write(*,*) "# iter_at_temp = ", iter_at_temp

      write(*,*) "# temptr : "
      read(*,*,err=990,end=990) temptr
      write(*,*) "# temptr = ", temptr

      write(*,*) "# eps_temptr : "
      read(*,*,err=990,end=990) eps_temptr
      write(*,*) "# eps_temptr = ", eps_temptr

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

c  initialise the simplex (p array)

      do i = 1,ndim+1
        do j = 1,ndim
          if (j.eq.i) then
            p(i,j) = x(j) + e(i)
          else
            p(i,j) = x(j)
          endif
        enddo
      enddo

      if (debug) then
        write(*,*) "# initial p() array:"
        do i = 1,ndim+1
          do j = 1,ndim
            write(*,20) p(i,j)
20          format(f16.8,1x,$)
          enddo
          write(*,*)
        enddo
        write(*,*)
      endif

c  y() array
      do i = 1,ndim+1
        do j = 1,ndim
          xtry(j) = p(i,j)
        enddo
        y(i) = chi2_func(xtry)
      enddo

      if (debug) then
        write(*,*) "# initial y() array:"
        do i = 1,ndim+1
          write(*,*) y(i)
        enddo
        write(*,*)
      endif

      mp = NDIMMAX+1
      np = NDIMMAX

      yb = 1.d38
      do i = 1,ndim
        pb(i) = 0.d0
      enddo

c  run it!
      itertot = 0
      iter = 0
      do while ((iter.le.0).and.(itertot.le.ITMAX))
        iter = iter_at_temp
        itertot = itertot + iter

        call amebsa(p,y,mp,np,ndim,pb,yb,ftol,chi2_func,iter,temptr)

        temptr = (1.d0-eps_temptr)*temptr  ! annealing schedule
        itertot = itertot - iter

        write(*,*) "# iter = ", iter
        write(*,*) "# itertot = ", itertot
        write(*,*) "# temptr = ", temptr
      enddo

c  sort the output according to chi^2
      call srtidx(ndim+1,y,id)

c  write the result of minimalisation

      if (debug) then
        write(*,*) "# p() array:"
        do i = 1,ndim+1
          do j = 1,ndim
            write(*,20) p(id(i),j)
          enddo
          write(*,*)
        enddo

        write(*,*) "# y() array:"
        do i = 1,ndim+1
          write(*,*) y(id(i))
        enddo
        write(*,*)
      endif

c      write(*,*) (p(id(1),j), j = 1,ndim), y(id(1))
      write(*,*) (pb(j), j = 1,ndim), yb

      stop

c  error handlers
990   continue
      write(*,*) 'simplex: Error reading standard input.'

      end


