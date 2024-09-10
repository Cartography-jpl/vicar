C
C     PROGRAM minfilt
c        jan 93       jjl      
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      parameter (nsmax=10000)
      implicit integer(a-z)
      INTEGER*4 SL,SS,NL,NS,NLW,NSW
      integer*2 OUT(nsmax), IN(51*nsmax)
      INTEGER OUTUNIT,INUNIT,CNT
C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','HALF',' ')
      CALL XVSIZE(SL,SS,NL,NS,nlin,nsin)
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','HALF',' ')
      SL = SL - 1

C
C  GET NLW AND NSW VALUES
      CALL XVP('NLW',NLW,CNT)
      CALL XVP('NSW',NSW,CNT)
      NLW = NLW / 2 * 2 + 1
      NSW = NSW / 2 * 2 + 1

      if(nsmax.lt.ns+nsw-1)then
         call xvmessage('input line too long',' ')
         call abend
      endif
      if(51*nsmax.lt.(ns+nsw-1)*nlw)then
         call xvmessage('insufficient memory available',' ')
         call abend
      endif
C
C  DETERMINE STARTING LINE AND INCREMENT VALUES
      LINE = 1 - NLW / 2
      L = NSW / 2
      L1 = (NSW + 1) / 2
      L2 = L1 + 1
      L3 = L1 - 1
      L4 = L1 + NS - 2
      L5 = L1 + NS
      M = NL + NL
      CURR = (NLW + 1) / 2
      IPOS = 0
      INC = NS + NSW - 1
      IMAX = (NLW) * INC
      NLWM1 = NLW - 1
      IF (NLWM1 .NE. 0) THEN
C
C  READ INITIAL NLW LINES INTO CORE, REFLECTING AT BOUNDARIES
C
         DO 200 I=1,NLWM1
            ILINE=LINE
            LINE=LINE+1
  201       IF (ILINE .LT. 1) ILINE = 2 - ILINE
            IF (ILINE .GT. NL) ILINE = M - ILINE
            IF (ILINE .LT. 1) GO TO 201
            CALL XVREAD(INUNIT,IN(IPOS+L1),STATUS,'NSAMPS',NS,'SAMP',SS,
     &                'LINE',SL+ILINE,' ')
            IF (NSW .NE. 1) THEN
                do j=1,L
                   in(ipos+L3+1-j)=in(ipos+L2-1+j)
                   in(ipos+L5-1+j)=in(ipos+L4+1-j)
                enddo
            ENDIF
            IPOS=IPOS+INC
200      CONTINUE
      ENDIF


      DO 300 I = 1, NL
         ILINE = LINE
         LINE = LINE + 1
301      IF (ILINE .LT. 1) ILINE = 2 - ILINE
         IF(ILINE .GT. NL) ILINE = M - ILINE
         IF (ILINE .LT. 1) GOTO 301
         CALL XVREAD(INUNIT,IN(IPOS+L1),STATUS,'NSAMPS',NS,'SAMP',SS,
     &               'LINE',SL+ILINE,' ')
         IF(NSW .NE. 1) THEN
             do j=1,L
                in(ipos+L3+1-j)=in(ipos+L2-1+j)
                in(ipos+L5-1+j)=in(ipos+L4+1-j)
             enddo
         ENDIF

         CALL MIN2D(IN,OUT,nlw,nsw,ns)

         CALL XVWRIT(OUTUNIT,OUT,STATUS,' ')
         IPOS=IPOS+INC
         IF(IPOS .GE. IMAX) IPOS=0
         CURR=MOD(CURR,NLW)+1
300   CONTINUE
C
C *****         CLOSE DATA SETS
C
999   CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END

      subroutine min2d(in,out,nlw,nsw,ns)
      integer*2 in(1),out(1),mindn
      len=ns+nsw-1

c find min dn for left most block
      k=0
      mindn=in(1)
      mincol=1
      do j=1,nlw
        do i=1,nsw
          k=k+1
          if(in(k).lt.mindn)then
             mincol=i
             mindn=in(k)      
          endif
        enddo
        k=j*len
      enddo
      out(1)=mindn

c do the rest
      do i=2,ns
        if(mincol.ge.i)then                ! check only right column
           k=i+nsw-1
           do j=1,nlw
             if(in(k).lt.mindn)then
                mincol=i+nsw-1
                mindn=in(k)      
             endif
             k=k+len
           enddo
           out(i)=mindn
        else                               ! check all columns
           mindn=in(i)
           mincol=i
           k=i
           do j=1,nlw
             do m=i,i+nsw-1
               if(in(k).lt.mindn)then
                  mincol=m
                  mindn=in(k)      
               endif
               k=k+1
             enddo
             k=j*len+i
           enddo
           out(i)=mindn
        endif
      enddo
      return
      end
