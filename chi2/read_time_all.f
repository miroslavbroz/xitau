c read_time_all.f
c Read time data from all relevant data files.
c Miroslav Broz (miroslav.broz@email.cz), Mar 19th 2016

      subroutine read_time_all(n, t)

      implicit none
      include 'chi2.inc'
      include 'dependent.inc'

      integer n
      real*8 t(TIMEMAX)

c internal
      integer i, j, k, i1st, iu, m_OBS
      real*8 t_OBS(OBSMAX), eps

      data i1st /0/
      save i1st

      if (i1st.eq.0) then
        n = 0
        do i = 1, nbod
          call read_time(file_SKY(i), n, t)
          call read_time(file_RV(i), n, t)
        enddo
        call read_time(file_VIS, n, t)
        call read_time(file_CLO, n, t)
        call read_time(file_SYN, n, t)
        call read_time(file_AO, n, t)

c for TTV's use three times

        m_OBS = 0
        call read_time(file_TTV, m_OBS, t_OBS)

        j = n
        do i = 1, m_OBS
          j = j + 1
          t(j) = t_OBS(i)
          j = j + 1
          t(j) = t_OBS(i) - approx_eclipse_duration/2.d0
          j = j + 1
          t(j) = t_OBS(i) + approx_eclipse_duration/2.d0
        enddo
        n = j

c dtto for eclipse durations

        m_OBS = 0
        call read_time(file_ECL, m_OBS, t_OBS)

        j = n
        do i = 1, m_OBS
          j = j + 1
          t(j) = t_OBS(i)
          j = j + 1
          t(j) = t_OBS(i) - approx_eclipse_duration/2.d0
          j = j + 1
          t(j) = t_OBS(i) + approx_eclipse_duration/2.d0
        enddo
        n = j

c for lightcurve, perform binning of the data (the same as in chi2_func_LC.f)

        do k = 1, nband

          m_OBS = 0
          call read_time(file_LC(k), m_OBS, t_OBS)
         
          j = n
          eps = 1.d-8
          if (m_OBS.ge.1) then
            j = j + 1
            t(j) = t_OBS(1)-eps
         
            do i = 2, m_OBS
              if (t_OBS(i)-t(j) > lightcurve_timestep) then
                j = j + 1
                t(j) = t_OBS(i)
              endif
            enddo
         
            j = j + 1
            t(j) = t_OBS(m_OBS)+eps
          endif
          n = j

        enddo  ! nband

        call quicksort(n, t)
 
        if (debug_swift) then
          open(unit=iu, file="times.dat", status="unknown")
          write(iu,*) "# t_of_interest [JD] & t-T0 [d] & i []"
          do i = 1, n
            write(iu,*) t(i), t(i)-T0, i
          enddo
          close(iu)
        endif

        i1st = 1
      endif

      return
      end


