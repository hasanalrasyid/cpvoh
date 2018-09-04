      REAL*8 FUNCTION ADD (A, B, C, D) 
      REAL*8 B,D
      INTEGER*4 A,C
      DIMENSION B(4), D(4)
      ADD = B(A) + D(C)
      RETURN
      END

      subroutine dbgstamp (hfout,fname,fline)
        integer*4 today(3), now(3)
        call idate(today)
        call itime(now)
        write (hfout, 1000) today(2),today(1),today(3), now
1000    format ('Date ', i2.2, '/', i2.2, '/', i4.4, '; time ',
     &          i2.2, ':', i2.2, ':', i2.2 )
      end
