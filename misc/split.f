c----------------------------------------------------------------------
      integer function split(s,sep,strings,max)
      implicit none

      integer max
      character*(*) s,strings(max)
      character sep
c
c  split string on separator
c----------------------------------------------------------------------
      integer i,j,k,length

      i = 0
      if (s(1:1).ne.sep) then
        k = 0
      else
        k = 1
      endif
10    continue
15      continue
        if ((s(k+1:k+1).eq.sep).and.(k.le.length(s))) then
          k = k + 1
          goto 15
        endif
        j = k + 1
25      continue
        if ((s(j+1:j+1).ne.sep).and.(j.le.length(s))) then
          j = j + 1
          goto 25
        endif
      if (k.lt.length(s)) then
        i = i + 1
        strings(i) = s(k+1:j)
        k = j + 1
        goto 10
      else
        split = i
        return
      endif
      end

