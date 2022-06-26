c srtidx.f
c Minimal sort of array (with output od indices).
c Miroslav Broz (miroslav.broz@email.cz), Nov 15th 2009

      subroutine srtidx(n,x,id)
      implicit none
      integer n,id(n)
      real*8 x(n)
      integer j,k,min,tmp

      do j = 1,n
        id(j) = j
      enddo
      do j = 1,n
        min=j
        do k = j+1,n
          if (x(id(min)).gt.x(id(k))) then
            min=k
          endif
        enddo
        tmp=id(j)
        id(j)=id(min)
        id(min)=tmp
      enddo
      
      return
      end


