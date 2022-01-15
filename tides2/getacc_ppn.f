c getacc_ppn.f
c Parametrized post-newtonian accelerations (to be added).
c Miroslav Broz (miroslav.broz@email.cz), May 26th 2016

c Reference: Fabrycky (2010), Eq. (2)
c
c f_GR = -G(m_star + m_p)/(r^2 c^2) * ( -2(2-eta)\dot r \dot\vec r + [ (1+3eta)\dot\vec r\cdot\dot\vec r - 3/2 eta(\dot r)^2 - 2(2+eta) G(m_star+m_p)/r ] \hat r )

      subroutine getacc_ppn(nbod,mass,xb,yb,zb,vxb,vyb,vzb,irij,irij2,
     :  axb,ayb,azb)

      include "../swift.inc"
      include "../misc/const.inc"
      include "tides2.inc"

c input
      integer nbod
      real*8 mass(nbod)
      real*8 xb(nbod),yb(nbod),zb(nbod)
      real*8 vxb(nbod),vyb(nbod),vzb(nbod)
      real*8 irij(NPLMAX,NPLMAX)
      real*8 irij2(NPLMAX,NPLMAX)

c output
      real*8 axb(nbod),ayb(nbod),azb(nbod)

c internal
      integer i, j
      real*8 xij, yij, zij, vij2, dotr2, dotr
      real*8 vxij, vyij, vzij
      real*8 eta, t1, t2, t3, t4, t5, c_
      real*8 acc, ax, ay, az, fac

      if (.not.use_ppn) return

      c_ = 1.d0/(c/AU*day)**2

      do i = 1, nbod  ! "planet"
        do j = i+1, nbod  ! "star"

          xij = xb(i) - xb(j)
          yij = yb(i) - yb(j)
          zij = zb(i) - zb(j)
          vxij = vxb(i) - vxb(j)
          vyij = vyb(i) - vyb(j)
          vzij = vzb(i) - vzb(j)

          vij2 = vxij*vxij + vyij*vyij + vzij*vzij
          dotr = (xij*vxij + yij*vyij + zij*vzij)*irij(i,j)
          dotr2 = dotr*dotr

          eta = mass(j)*mass(i)/(mass(j)+mass(i))**2
          t1 = (mass(j)+mass(i))*c_*irij2(i,j)
          t2 = -2.d0*(2.d0-eta)*dotr
          t3 = (1.d0+3.0d0*eta)*vij2
          t4 = -3.d0/2.d0*eta*dotr2
          t5 = -2.d0*(2.d0+eta)*(mass(j)+mass(i))*irij(i,j)

          ax = t1*(t2*vxij + (t3+t4+t5)*xij*irij(i,j))
          ay = t1*(t2*vyij + (t3+t4+t5)*yij*irij(i,j))
          az = t1*(t2*vzij + (t3+t4+t5)*zij*irij(i,j))

          axb(i) = axb(i) - ax
          ayb(i) = ayb(i) - ay
          azb(i) = azb(i) - az

        enddo
      enddo

      return
      end


