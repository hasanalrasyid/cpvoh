      subroutine dbgstamp (hfout,fname,fline)
        character(len=*), intent(in) :: fname
        integer*4 hfout,fline, t(8)
        
        call date_and_time (values=t)
        write (hfout,1000) (t(i),i=1,3),(t(i),i=5,8),fname,fline
1000    FORMAT('Date ', i4.4, '/', i2.2, '/', i2.2, '; time ',&
    &   i2.2, ':', i2.2, ':', i2.2 , ':', i4.4 ,'===',A20,'@',i4 )
      end
