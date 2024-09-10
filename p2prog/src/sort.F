      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C  IBIS ROUTINE SORT
C
C  PURPOSE:  SORT ALL COLUMNS OF AN IBIS INTERFACE FILE BASED UPON ONE
C  ONE OR MORE CONTROL KEY COLUMNS.  THE KEYS MAY BE NUMERIC OR
C  APHABETIC.  THE RESULTING LEXICOGRAPHIC ORDER MAY BE STORED IN 
C  AN INDEX COLUMN.
C
C  USER PARAMETERS:
C
C  SORTCOL,N1,...NK - THE INTEGERS N1 THROUGH NK SPECIFY THE COLUMNS 
C     	BEING SORTED. N1 IS THE PRIMARY SORT COLUMN, ETC.
C  DESCEND - THIS KEYWORD INDICATES THAT ALL COLUMNS WILL BE SORTED IN
C	DESCENDING ORDER.
C  INDEXCOL,N - THE INTEGER N DESIGNATES A COLUMN TO RECEIVE AN ORDINAL
C	CREATED BY THE SORTING PROCESS.  AN INDEX NUMBER IS ASSIGNED TO
C	EACH UNIQUE COMBINATION OF LETTERS OR NUMBERS IN THE SORTED LIST.
C  ALPHA -   INDICATES THAT THE SORT COLUMNS ARE ALPHABETIC.

      INCLUDE 'sort.fin'
      INTEGER RUNIT,STATUS,LST,I,PTR
C
C  INITIALIZE, GET PARAMETERS, OPEN FILES
C
      CALL IFMESSAGE('SORT version 6-MAR-95')
      CALL XVUNIT( RUNIT,'INP', 1, STATUS,' ')
      IF (STATUS.NE.1) THEN
	   CALL XVMESSAGE('INPUT FILE NOT FOUND by XVUNIT',' ')
	   CALL ABEND
      ENDIF
      CALL IBIS_FILE_OPEN(RUNIT,IBIS,'UPDATE',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL GET_PARMS
      
      CALL SET_UP_FILE
 
      CALL SORT_FILE     
	
C
C  MOVE ENTIRE FILE ACCORDING TO POINTERS CX, using the most
C   EFFICIENT METHODS. IN SOME CASES, BOTH METHODS WILL BE USED
C   (E.G. COLUMN-ORIENTED FILES HAVING A COLUMN WIDER THAN 8 BYTES).
C

      if (NCCOLS.GT.0) CALL CORMOVCOLUMNS
      
      if (NRCOLS.GT.0) CALL CORMOVRECORD

C
C  Set up INDEX Column, if requested
	IF (INDEX.GT.0) THEN
	    LST = -1
	    PTR = 0
	    DO I=1,CLEN
	       IF (CS(I).NE.LST) PTR = PTR+1.
	       COL1(I) = PTR
	       LST = CS(I)
	    ENDDO
 	    CALL IBIS_COLUMN_SET(IBIS,'U_FORMAT','FULL',INDEX,STATUS) 
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    CALL IBIS_COLUMN_WRITE(IBIS,COL1,INDEX,1,CLEN,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	ENDIF
C
	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,0)

	RETURN
	END



      SUBROUTINE GET_PARMS
      INCLUDE 'sort.fin'
      INTEGER DEF,COUNT,IBIS_FILE_GET,STATUS
      LOGICAL XVPTST
      CHARACTER*7  IBISFTYPE,IBISORG

      IBIS1 = .FALSE.
      IBIS2 = .FALSE.
      CALL XVPARM ('SORTCOL',SORTCL,SORTCNT,DEF,20)
      CALL XVP ('INDEXCOL',INDEX,COUNT)
      ALPH  = XVPTST ('ALPHA')
      DSCND = XVPTST ('DESCEND')     
      COUNT = IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      COUNT =  IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      COUNT =  IBIS_FILE_GET(IBIS,'VERSION',IBISFTYPE ,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      COUNT =  IBIS_FILE_GET(IBIS,'ORG',IBISORG ,1,1)
      IF (COUNT.LE.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      ROWORG = (IBISORG(1:3).EQ.'ORG')
      IF (IBISFTYPE (1:6).EQ.'IBIS-1') THEN
          IBIS1 = .TRUE.
          CF = 'A4' 
      ELSE
          IF (IBISFTYPE(1:6).EQ.'IBIS-2') THEN
             IBIS2 = .TRUE.
             CF = 'A8' 
          ELSE
             CALL MABEND('IBIS FILE TYPE UNKNOWN')
          ENDIF
      ENDIF 	
	
      RETURN
      END





      SUBROUTINE SET_UP_FILE
      INCLUDE 'sort.fin'
      INTEGER I,IY,STATUS,CSIZE
      CHARACTER*10 FMT

C
C  SET UP COLUMN FORMATS FOR IBIS-1
C
      if (IBIS1.AND.ALPH) then
         do i=1,SORTCNT
            iy=SORTCL(i)
 	    call IBIS_COLUMN_SET(IBIS,'FORMAT',CF,iy,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
         enddo
      endif
C
C  SET UP COLUMN TRANSLATION, and determine which method of
C     data shuffling (by row/by column) should be done for each.
C     Columns wider than 8 bytes must use record transfer, and
C     row-oriented files should use record transfer regardless.
C
      NCCOLS=0
      NRCOLS=0
      do i=1,ncol
         if (i.ne.INDEX) then
	    CALL IBIS_COLUMN_GET(IBIS,'FORMAT',FMT,I,STATUS)	    
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    CALL IBIS_COLUMN_GET(IBIS,'U_SIZE',CSIZE,I,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    IF (.NOT.ROWORG.AND.CSIZE.LE.8) THEN
	    	NCCOLS = NCCOLS+1
		CCOLS(NCCOLS) = I
	    ELSE
	    	NRCOLS = NRCOLS+1
		RCOLS(NRCOLS) = I
	    ENDIF
            if (FMT(1:1).EQ.'A'.OR.FMT(1:1).EQ.'a') then
               call IBIS_COLUMN_SET(IBIS,'U_FORMAT',CF,i,STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
            else      ! (.not.) ALPH
               call IBIS_COLUMN_SET(IBIS,'U_FORMAT','REAL',i,STATUS) 
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    endif
         endif
      enddo

C
C  SET UP COLUMN ARRAYS
c

      DO I=1,CLEN
	 CX(I) = I
	 CS(I) = 1
	 if (IBIS1.AND.ALPH) then
            COL1ALPH(I) ='    '
            COL1ATEMP(I)='    '
	 endif
         if (IBIS2.AND.ALPH) then
            COL1I2A(I) = '        '
            COL1I2AT(I)= '        '
         endif
      ENDDO
      
      RETURN
      END



 
      SUBROUTINE SORT_FILE
      INCLUDE 'sort.fin'
      INTEGER IX,STATUS,PU,PL,SLEN,PK,IT1,IIT,K,KK,OLDCS
      INTEGER  TOP,BOT,TEMP

C
C  PERFORM SORT.  CS WILL CONTAIN LEXICOGRAPHIC INDEX.  CX WILL CONTAIN
C  POINTERS.  THE FILE IS NOT MOVED HERE. First sort is only for ibis-1 
C  files.

      DO IX=1,SORTCNT
	 if (ALPH) then
            if (IBIS1) then
     	       CALL IBIS_COLUMN_READ(IBIS,COL1ALPH,SORTCL(IX),1,
     +                              CLEN,STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	       call CORMOVALPH (COL1ALPH,CX,CLEN)
	    else            ! IBIS2
     	       CALL IBIS_COLUMN_READ(IBIS,COL1I2A,SORTCL(IX),1,
     +                              CLEN,STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	       call CORMOVI2A (COL1I2A,CX,CLEN)
            endif            ! IBIS1 & IBIS2 & ALPH
         else                ! (not) ALPH
            CALL IBIS_COLUMN_READ(IBIS,COL1,SORTCL(IX),1,
     +                             CLEN,STATUS)
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	    CALL CORMOV (COL1,CX,CLEN)
    	 endif
	 PU = 1
	 PL = 1
	 DO WHILE (PL.LE.CLEN)
	    DO WHILE (PU.LT.CLEN.AND.CS(PU+1).EQ.CS(PL))
	      PU = PU+1
	    ENDDO
	    SLEN = PU-PL+1
            call MVE(7,SLEN,CX(PL),CXTEMP,1,1)
	    IF (ALPH) THEN
               PK=PL+SLEN-1
               IT1=1
               DO IIT=PL,PK
		  if (IBIS1) COL1ATEMP(IT1) = COL1ALPH(IIT)
		  if (IBIS2) COL1I2AT(IT1)  = COL1I2A(IIT)
		  IT1=IT1+1
	       ENDDO
	       if (IBIS1) CALL CSORTP(COL1ALPH(PL),1,SLEN,1,4,IP)
	       if (IBIS2) call CSORTP(COL1I2A(PL) ,1,SLEN,1,8,IP)
	    ELSE             ! (not) ALPH
	       call MVE(7,SLEN,COL1(PL),COL1TEMP,1,1)
	       CALL SSORTP (COL1(PL),1,SLEN,IP)
	    ENDIF
	    do K = 1,SLEN
	       KK = K + PL - 1
               if (ALPH) then
                  if (IBIS1) COL1ALPH(KK) = COL1ATEMP(IP(K))
		  if (IBIS2) COL1I2A(KK)  = COL1I2AT(IP(K))
               else           ! (not) ALPH
	         COL1(KK) = COL1TEMP(IP(K))
               endif
               CX(KK)   = CXTEMP(IP(K))
	    enddo	   
	    PL = PU + 1
	 ENDDO                ! while
 	 PU = 1
	 PL = 1
	 OLDCS = CS(1)
	 CS(1) = 1
	 DO WHILE (PU.LT.CLEN)
            if (ALPH) then
               if (IBIS1) then
	          IF (COL1ALPH(PU+1).NE.COL1ALPH(PL)
     +	                  .OR.CS(PU+1).NE.OLDCS) THEN
	             PL = PU+1
	             OLDCS = CS(PL)
	          ENDIF
	       else      ! IBIS2
	          IF (COL1I2A(PU+1).NE.COL1I2A(PL)
     +	                  .OR.CS(PU+1).NE.OLDCS) THEN
              	     PL = PU+1
                     OLDCS = CS(PL)
	          ENDIF
               endif   ! IBIS1, IBIS2 & ALPH
	   else        ! (not) ALPH
 	       IF (COL1(PU+1).NE.COL1(PL).OR.CS(PU+1).NE.OLDCS) THEN
	          PL = PU+1
	          OLDCS = CS(PL)
	       ENDIF
           endif
	   PU = PU+1
	   CS(PU) = PL
	 ENDDO
      ENDDO
C
C  IF SORT NEEDS TO BE DESCENDING, FLIP THE TABLE NOW
C
      IF (DSCND) THEN
	 TOP = 1
	 BOT = CLEN
	 DO WHILE (TOP.LT.BOT)
	    TEMP = CS(BOT)
	    CS(BOT) = CS(TOP)
	    CS(TOP) = TEMP
	    TEMP = CX(BOT)
	    CX(BOT) = CX(TOP)
	    CX(TOP) = TEMP
	    TOP=TOP+1
	    BOT=BOT-1
	 ENDDO
      ENDIF
C
      
      RETURN
      END
     

	SUBROUTINE CORMOVRECORD
C
C  PERFORMS AN IN PLACE MOVE OF IBIS-RECORDS ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	INCLUDE 'sort.fin'
	REAL*8 RAWBUF(4000),TEMPBUF(4000)
        INTEGER*4 RECORD,STATUS,IX,I,IQ,K
	
C
        call ibis_record_open(IBIS,RECORD,' ',
     +                        RCOLS,NRCOLS,'NONE',STATUS)
	if (STATUS.ne.1) call ibis_signal(IBIS,STATUS,1)
	call ibis_record_set(RECORD,'NR',1,STATUS)   ! Don't buffer record
	if (STATUS.ne.1) call ibis_signal(IBIS,STATUS,1)

	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  call ibis_record_read(RECORD,TEMPBUF,IX,STATUS) !TEMP = COL(IX)
	  if (status.ne.1) call ibis_signal(ibis,status,1)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    call ibis_record_read(RECORD,RAWBUF,K,STATUS) 
	    if (status.ne.1) call ibis_signal(ibis,status,1)
	    call ibis_record_write(RECORD,RAWBUF,I,STATUS) ! COL(I) = COL(K)
	    if (status.ne.1) call ibis_signal(ibis,status,1)	   
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  call ibis_record_write(RECORD,TEMPBUF,IQ,STATUS) !COL(IQ) = TEMP
	  if (status.ne.1) call ibis_signal(ibis,status,1)
	ENDDO
        call ibis_record_close(record,status)
	if (status.ne.1) call ibis_signal(ibis,status,1)
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END



	SUBROUTINE CORMOVCOLUMNS
C
C  PERFORMS AN IN PLACE MOVE OF IBIS-COLUMNS (OF WIDTH <= 8)
C  ACCORDING TO POINTERS CX. FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	INCLUDE 'sort.fin'
	CHARACTER*8 FMT
        INTEGER*4 I,IX,STATUS
	LOGICAL ALPHCOL

        DO I=1,NCCOLS
             IX = CCOLS(I)  
C --         READ AND SHUFFLE	

	     CALL IBIS_COLUMN_GET(IBIS,'FORMAT',FMT,IX,STATUS)
	     ALPHCOL = (FMT(1:1).EQ.'A'.OR.FMT(1:1).EQ.'a')
	     
	     if (ALPHCOL) then
	       if (IBIS1) then
 	          CALL IBIS_COLUMN_READ(IBIS,COL1ALPH,IX,1,CLEN,STATUS) 
	          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	          call CORMOVALPH (COL1ALPH,CX,CLEN)
 	       else         ! IBIS2
  	          CALL IBIS_COLUMN_READ(IBIS,COL1I2A,IX,1,CLEN,STATUS) 
 	          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
 	          call CORMOVI2A (COL1ALPH,CX,CLEN)
	       endif        ! IBIS1, IBIS2 & ALPH
             else            ! (not) ALPH
	          CALL IBIS_COLUMN_READ(IBIS,COL1,IX,1,CLEN,STATUS)
	          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	          CALL CORMOV (COL1,CX,CLEN)
             endif

C --        WRITE IT OUT

	    if (ALPHCOL) then
	      if (IBIS1) then
                 CALL IBIS_COLUMN_WRITE(IBIS,COL1ALPH,IX,1,CLEN,STATUS)
	      else    ! IBIS2
                 CALL IBIS_COLUMN_WRITE(IBIS,COL1I2A ,IX,1,CLEN,STATUS)
              endif   ! IBIS1, IBIS2 & ALPH
	    else       ! (not) ALPHCOL
	        CALL IBIS_COLUMN_WRITE(IBIS,COL1,IX,1,CLEN,STATUS)
 	    endif
	    IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  
        ENDDO

	RETURN
	END


	SUBROUTINE CORMOV (COL,CX,CLEN)
C
C  PERFORMS AN IN PLACE MOVE OF VECTOR COL ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	IMPLICIT INTEGER(A-Z)
	DIMENSION COL(1),CX(1)
C
	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  TEMP = COL(IX)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    COL(I) = COL(K)
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  COL(IQ) = TEMP
	ENDDO
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END
C
	SUBROUTINE CORMOVALPH (COLALPH,CX,CLEN)
C
C  PERFORMS AN IN PLACE MOVE OF VECTOR COLALPH ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	IMPLICIT INTEGER(A-Z)
        CHARACTER*4  COLALPH
        CHARACTER*5  TEMPA
	DIMENSION COLALPH(1),CX(1)
C
	TEMPA='     '   ! 5 SPACES
	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  TEMPA= COLALPH(IX)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    COLALPH(I)= COLALPH(K)
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  COLALPH(IQ)= TEMPA
	ENDDO
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END
C
	SUBROUTINE CORMOVI2A (COLIALPH,CX,CLEN)
C
C  PERFORMS AN IN PLACE MOVE OF VECTOR COLALPH ACCORDING TO POINTERS CX.
C  FORMS THE DISJOINT CYCLES OF THE PERMUTATION.
C
	IMPLICIT INTEGER(A-Z)
        CHARACTER*8  COLIALPH
        CHARACTER*9  TEMPIA
	DIMENSION COLIALPH(1),CX(1)
C
	TEMPIA='         '   ! 9 SPACES
	DO IX=1,CLEN
	  I = IX
	  IQ = IX
	  TEMPIA= COLIALPH(IX)
	  K = CX(IX)
	  DO WHILE (K.GE.0)
	    CX(I) = -K
	    COLIALPH(I)= COLIALPH(K)
	    IQ = I
	    I = K
	    K = CX(I)
	  ENDDO
	  COLIALPH(IQ)= TEMPIA
	ENDDO
	DO I=1,CLEN
	  CX(I) = -CX(I)
	ENDDO
	RETURN
	END

