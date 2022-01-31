c getacc_ppn.f
c Parametrized post-newtonian accelerations (to be added).
c Miroslav Broz (miroslav.broz@email.cz), Jan 30th 2021

c Reference: Standish & Wiliams (2006): Explanatory Supplement
c to the Astronomical Alamanach. Chap. 8, Eq. (8-1).
c "Orbital ephemerides of the Sun, Moon and Planets."


      subroutine getacc_ppn(nbod_,mass,xb,yb,zb,vxb,vyb,vzb,irij,irij2,
     :  axb,ayb,azb)

      include "../swift.inc"
      include "../misc/const.inc"
      include "../simplex/simplex.inc"
      include "../simplex/dependent.inc"

c input
      integer nbod_
      real*8 mass(nbod_)
      real*8 xb(nbod_),yb(nbod_),zb(nbod_)
      real*8 vxb(nbod_),vyb(nbod_),vzb(nbod_)
      real*8 irij(NPLMAX,NPLMAX)
      real*8 irij2(NPLMAX,NPLMAX)

c output
      real*8 axb(nbod_),ayb(nbod_),azb(nbod_)

c internal
      integer i, j, k
      real*8 beta, gama, c_
      real*8 xij, yij, zij
      real*8 vxij, vyij, vzij
      real*8 vi2, vj2, dotri_dotrj, rij_dotri, rij_dotrj, rji_ddotrj
      real*8 k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, ksum
      real*8 ax, ay, az

      integer i1st
      real*8 last
      data i1st /0/
      save i1st,last

      if (.not.use_ppn) return

      beta = 1.d0
      gama = 1.d0
      c_ = 1.d0/(c/AU*day)**2

      do i = 1, nbod
        do j = 1, nbod
          if (i.ne.j) then

            xij = xb(i) - xb(j)
            yij = yb(i) - yb(j)
            zij = zb(i) - zb(j)
            vxij = vxb(i) - vxb(j)
            vyij = vyb(i) - vyb(j)
            vzij = vzb(i) - vzb(j)

            vi2 = vxb(i)**2 + vyb(i)**2 + vzb(i)**2
            vj2 = vxb(j)**2 + vyb(j)**2 + vzb(j)**2
            dotri_dotrj = vxb(i)*vxb(j) + vyb(i)*vyb(j) + vzb(i)*vzb(j)
            rij_dotri = xij*vxb(i) + yij*vyb(i) + zij*vzb(i)
            rij_dotrj = xij*vxb(j) + yij*vyb(j) + zij*vzb(j)
            rji_ddotrj = -(xij*axb(j) + yij*ayb(j) + zij*azb(j))

            k1 = mass(j)*irij(i,j)*irij2(i,j)*c_
            k2 = 0.d0
            k3 = 0.d0
            do k = 1, nbod
              if (k.ne.i) then
                k2 = k2 + mass(k)*irij(i,k)
              endif
              if (k.ne.j) then
                k3 = k3 + mass(k)*irij(j,k)
              endif
            enddo
            k2 = -2.d0*(beta+gama)*k2
            k3 = -(2.d0*beta-1.d0)*k3
            k4 = gama*vi2
            k5 = (1.d0+gama)*vj2
            k6 = -2.d0*(1.d0+gama)*dotri_dotrj
            k7 = -3.d0/2.d0*irij2(i,j)*rij_dotrj**2
            k8 = 1.d0/2.d0*rji_ddotrj

            k9 = (2.d0+2.d0*gama)*rij_dotri
            k10 = -(1.d0+2.d0*gama)*rij_dotrj

            k11 = (3.d0+4.d0*gama)/2.d0*c_*mass(j)*irij(i,j)

            ksum = k2+k3+k4+k5+k6+k7+k8

            ax = k1*ksum*(-xij) + k1*(k9+k10)*vxij + k11*axb(j)
            ay = k1*ksum*(-yij) + k1*(k9+k10)*vyij + k11*ayb(j)
            az = k1*ksum*(-zij) + k1*(k9+k10)*vzij + k11*azb(j)

            axb(i) = axb(i) + ax
            ayb(i) = ayb(i) + ay
            azb(i) = azb(i) + az

          endif
        enddo
      enddo

      return
      end


