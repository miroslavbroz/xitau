c chi2_func_RV.f
c Calculate chi^2 for radial-velocity data.
c Miroslav Broz (miroslav.broz@email.cz), Sep 18th 2015

      subroutine chi2_func_RV(NOUT, tout, vb, chi2, n)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

c input
      integer NOUT
      real*8 tout(OUTMAX)
      real*8 vb(OUTMAX,NBODMAX,3)

c output
      real*8 chi2
      integer n

c observational data
      integer m_RV(NBODMAX)
      real*8 t_RV(OBSMAX,NBODMAX), vzb_RV(OBSMAX,NBODMAX),
     :  sigmavz_RV(OBSMAX,NBODMAX)

c internal variables
      integer i,j,k,l,i1st,iu
      real*8 chi2_
      real*8 vzb_interp,dvz

c functions
      real*8 interp,kms_auday,auday_kms

      data i1st /0/
      data m_RV /NBODMAX*0/
      data iu /10/

      save i1st,
     :  m_RV,t_RV,vzb_RV,sigmavz_RV

c-----------------------------------------------------------------------
c
c read radial-velocity observations  (only 1st time!)
c

      if (i1st.eq.0) then

        do j = 1, nbod
          call read_RV(file_RV(j), m_RV(j), t_RV(1,j), vzb_RV(1,j),
     :      sigmavz_RV(1,j))

c convert RV data to AU/day
          do i = 1, m_RV(j)
            vzb_RV(i,j) = kms_auday(vzb_RV(i,j))
            sigmavz_RV(i,j) = kms_auday(sigmavz_RV(i,j))
          enddo

          if (debug) then
            write(*,*) "# m_RV(", j, ") = ", m_RV(j)
          endif
        enddo

        i1st = 1

      endif  ! i1st

c-----------------------------------------------------------------------
c
c chi^2 for radial-velocity data
c
      chi2 = 0.d0
      n = 0

      if (debug) then
        open(unit=iu,file="chi2_RV.dat",status="unknown")
        write(iu,*) "# t_RV & vzb_interp [km/s] & sigmavz_RV & ",
     :    "ibod & chi^2"
        write(iu,*) "# t_RV & vzb_RV            & sigmavz_RV & ",
     :    "ibod & chi^2"
      endif

      do k = 1, nbod
        j = 2
        do i = 1, m_RV(k)

          do while ((j.lt.NOUT).and.(tout(j).le.t_RV(i,k)))
            j = j+1
          enddo

c linear interpolation of integrated data to a given position in time

          vzb_interp = interp(tout(j-1), tout(j), vb(j-1,k,3),
     :      vb(j,k,3), t_RV(i,k))  ! k-th body, vb_z coordinate

          dvz = vzb_interp - vzb_RV(i,k)
          chi2_ = (dvz/sigmavz_RV(i,k))**2
          lns = lns + log(sigmavz_RV(i,k))
          chi2 = chi2 + chi2_
          n = n + 1

          if (debug) then
            write(iu,*) t_RV(i,k), auday_kms(vzb_interp),
     :        auday_kms(sigmavz_RV(i,k)), k, chi2_
            write(iu,*) t_RV(i,k), auday_kms(vzb_RV(i,k)),
     :        auday_kms(sigmavz_RV(i,k)), k, chi2_
            write(iu,*)
          endif

        enddo
      enddo  ! nbod

      if (debug) then
        close(iu)
      endif

      return
      end


