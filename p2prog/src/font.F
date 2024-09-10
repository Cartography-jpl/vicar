C  REVISION HISTORY
C     2-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     7-96  HBM  FIX HANDLING OF BLANKS WITHIN TEXT
C     9-09  SRL  removed intial copy of the file to output, 
C           then close and then ropen to update mode.
C           Compressed files don't work in update mode. 
C           Now it will Read, modify, write
C           Add a call to xveaction so IO errors are reported
C           It is still unreliable when using the size option
      PROGRAM font
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      PARAMETER (NP=30)      
      PARAMETER (MAX_BYTES=524 288)	! 0.5 MBytes
      BYTE IMAGE(MAX_BYTES)
      CHARACTER*100 TEXT(NP)
      BYTE TEXT1(100)
      INTEGER FONT(NP),TALL(NP),DN(NP),SIZE(4),START(2*NP),THICK(NP),
     &        STATUS,TallCnt,WideCnt,LocCnt,RotCnt,DNCnt,FontCnt,
     &        LOC(NP),INU,OUTU,NC(NP),DEF,COUNT,SL,SS,
     &        ThickCnt,SCOUNT,TCOUNT,FAT,NL,NS,
     &        NLCHUNK,SLCHUNK,SLOUT
      REAL WIDE(NP), ROTATE(NP)
c....flag to determine if a text string has been written, in part or
c    whole, off the image.
      LOGICAL FLAG
      INTEGER F2HBUF(12)
      INTEGER*2 FONT1, DN1, TALL1, NL1, NS1, ISS1, ISL1, NC1
      LOGICAL TXTTEXT, TXTFONT, TXTSIZE, TXTCOLOR, STATUS1
C 
      CALL IFMESSAGE('FONT version March-09')
	  call xveaction ('S', 'ERROR ***') 
C     ading the 'A' flag will cause an abort on error
C      WRITE (*,'(A)') 'FONT ##################'
      call XVTRANS_SET(F2HBUF,'FULL','HALF',STATUS)
      CALL XVUNIT(INU,'INP',1,STATUS,' ')
      CALL XVUNIT(OUTU,'OUT',1,STATUS,' ')
	  CALL XVUNIT(OUTU2,'cc',1,STATUS,' ')
C	  WRITE (*,'(A,I3,A,I3)') 'XVUNIT OUTU2=',OUTU2,' status ',STATUS
C  
c...get the number of lines and samples of the input image from its internal
c   control block
      CALL XVOPEN(INU,STATUS,' ')
      CALL XVGET(INU,STATUS,'NL',NL,'NS',NS,' ')
 
      CALL XVPARM('SIZE',SIZE,COUNT,DEF,4)
      IF (DEF.EQ.1) THEN         
         SIZE(1)=1
         SIZE(2)=1
		 SIZE(3)=NL
         SIZE(4)=NS
      ENDIF
C     SIZE(1)=StartLine
C     SIZE(2)=StartSample
C     SIZE(3)=NL 
C     SIZE(4)=NS
	  	  
      CALL XVOPEN(OUTU,STATUS,'OP','WRITE',
     &     'U_NL',SIZE(3),'U_NS',SIZE(4),' ')
 
      CALL XVPARM('POSITION',START,SCOUNT,DEF,2*NP)
      CALL XVPARM('TEXT',TEXT,TCOUNT,DEF,NP)
      IF ( SCOUNT .NE. 2*TCOUNT ) THEN
         CALL XVMESSAGE( 'Number of TEXT strings does not match ',' ' )
         CALL XVMESSAGE( 'number of POSITIONS.',' ' )
         CALL ABEND
      END IF
 
      CALL XVPARM('TALL',TALL,TallCnt,DEF,NP)
      CALL XVPARM('WIDE',WIDE,WideCnt,DEF,NP)
      CALL XVPARM('LOC',LOC,LocCnt,DEF,NP)
      CALL XVPARM('ROTATE',ROTATE,RotCnt,DEF,NP)
      CALL XVPARM('DN',DN,DNCnt,DEF,NP)
      CALL XVPARM('FONT',FONT,FontCnt,DEF,NP)
      CALL XVPARM('THICK',THICK,ThickCnt,DEF,NP)

c...read the DESIRED PORTION OF THE input image into THE output image....
C      WRITE (*,'(A,I6,A,I6)') 'FONT NL=',SIZE(3) ,' NS=',SIZE(4)
 
C	read and write to output image removed

C	Divide the image into "chunks" where each chunk contains
C	NS samples and NLCHUNK lines.


      NLCHUNK = MAX_BYTES/SIZE(4)
      NLCHUNK = MIN(NLCHUNK,SIZE(3))
	  
C	  WRITE (*,'(A,I6)') '  NLCHUNK=',NLCHUNK

C	Now for each chunk....
C	  read it from the image
C	  write all strings into that chunk (sort of)
C	  write chunk to image

C      DO SLCHUNK = 1,SIZE(3),NLCHUNK
C                    from, to, increment
      DO SLCHUNK = SIZE(1),SIZE(3)+SIZE(1),NLCHUNK
C	  DO SLCHUNK = SIZE(2),SIZE(3),NLCHUNK+SIZE(3)
	  
C	   Calculate chunk boundary

C	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+1)
C 	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+SIZE(1)+1) C one extra ??
	 NL = MIN(NLCHUNK, SIZE(3)-SLCHUNK+SIZE(1))
	 NS = SIZE(4)
      SS = 1
	  SL = SLCHUNK
C	  SL = SLCHUNK + SIZE(2)
	  SLOUT = SL - SIZE(1) + 1
C     start at 1 not 0 ??
C      SLOUT = SL
C	 WRITE (*,'(A)') '***** loop start ******** ' 
C	 WRITE (*,'(A)') '** ' 
C	 WRITE (*,'(A,I6,A,I6)') '**   CHUNK SLCHUNK=',SLCHUNK,' NLCHUNK=',NLCHUNK
C	 WRITE (*,'(A,I6,A,I6, A,I6,A,I6)') '**   startLine SIZE(1)=',SIZE(1),' startSample SIZE(2)=',SIZE(2),'  NL SIZE(3)=',SIZE(3),' NS SIZE(4)=',SIZE(4)
C	 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6)') '**   SS=',SS,' SL=',SL,' SLOUT=',SLOUT,'  NL=',NL,' NS=',NS,' TCOUNT=',TCOUNT
C	 WRITE (*,'(A)') '** ' 
C	 WRITE (*,'(A)') '************************** ' 
C	   Read data from image file

	 DO J = 1,NL
	 
	    
C	    IF ( J .EQ. 1) THEN
C		 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   READ J=',J,' IMAGE(',((J-1)*SIZE(2)+1),') LINE=',(SL+J-1),' SAMP=',SIZE(2),' NSAMPS=',NS
C		ELSEIF ( J .EQ. NL) THEN
C		 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   READ J=',J,' IMAGE(',((J-1)*SIZE(2)+1),') LINE=',(SL+J-1),' SAMP=',SIZE(2),' NSAMPS=',NS
C		ENDIF
C      This is the original READ, it takes SIZE into account to resize the image if it is requested
            CALL XVREAD(INU,IMAGE((J-1)*NS+1),STATUS,'LINE',SL+J-1,
     &     	'SAMP',SIZE(2),'NSAMPS',SIZE(4),' ')

C      This is the READ for the second loop
C            CALL XVREAD(INU,IMAGE((J-1)*NS+1),STATUS,
C     &           'LINE',SL+J-1,'SAMP',SS,'NSAMPS',NS,' ')
	 END DO

C	   Draw each text string to image...
         DO I=1,TCOUNT
            DO K = 1, 100
              TEXT1(K) = 0
            END DO
            CALL MVCL(TEXT(I),TEXT1(1),100)
            NC(I) = 0
            DO K = 1, 100
               IF (TEXT1(K) .NE. 32) NC(I) = K
            END DO
c...Add missing parameters
            IF ( I .GT. TallCnt  ) TALL(I)   = TALL  (TallCnt)
            IF ( I .GT. WideCnt  ) WIDE(I)   = WIDE  (WideCnt)
            IF ( I .GT. LocCnt   ) LOC(I)    = LOC   (LocCnt)
            IF ( I .GT. RotCnt   ) ROTATE(I) = ROTATE(RotCnt)
            IF ( I .GT. DNCnt    ) DN(I)     = DN    (DNCnt)
            IF ( I .GT. FontCnt  ) FONT(I)   = FONT  (FontCnt)
            IF ( I .GT. ThickCnt ) THICK(I)  = THICK (ThickCnt)

C...SET STARTING COORDDINATE REFERENCE TO OUTPUT IMAGE..
            II = I*2-1
            JJ = II+1
C      START is Position - could have a flag, either adjust to output coordinates
C      or leave in input image coordinates
C      since we are doing sizing on the fly. add SIZE to the coordinate
C      don't do this? the number is in output coordinates already
C            START(II)=START(II)-SIZE(1)+1
C            START(JJ)=START(JJ)-SIZE(2)+1
		
C      convert position from 			
		   START(II)=START(II)+SIZE(1)+1
C           START(JJ)=START(JJ)+SIZE(2)+1

C	      Skip if no characters in string
C          WRITE (*,'(A,A,A)') '  TEXT(I)=',TEXT(I),' <*'
C          WRITE (*,'(A,I2,A,I6,A,I3)') '   I=',I,' K=',K,'    NC(I)=',NC(I)
		  
            IF(NC(I).EQ.0) GOTO 1			
			
C			WRITE (*,'(A,A,A,I6,A,I6,A)') '   WRITING  TEXT(I)=',TEXT(I),' START(II)=',START(II),' START(JJ)=',START(JJ),'  <*******'
C	      Set up all parameters for this string

            CALL XVTRANS(F2HBUF,FONT(I),FONT1,1)
            STATUS1 = TXTFONT(FONT1)
            CALL XVTRANS(F2HBUF,TALL(I),TALL1,1)
            STATUS1 = TXTSIZE(TALL1,WIDE(I))
            CALL TXTROTATE(ROTATE(I))
            CALL XVTRANS(F2HBUF,DN(I),DN1,1)
            STATUS1 = TXTCOLOR(DN1)
 
c...the starting line and sample within the chunk are calculated
c   and the subroutine that writes the text is called after we
c   assign the thickness of each line in the characters, based on
c   the users request. Thickening is done be re-writing the string with
c   changing the iss and isl as needed.

            FAT=THICK(I)
            FLAG = .TRUE.
            DO J=1,FAT
               ISL=START(II)-SL+1-(J-1)
               DO K=1,FAT
                  ISS=START(JJ)-SS+K
                  CALL XVTRANS(F2HBUF,NL,NL1,1)
                  CALL XVTRANS(F2HBUF,NS,NS1,1)
                  CALL XVTRANS(F2HBUF,ISS,ISS1,1)
                  CALL XVTRANS(F2HBUF,ISL,ISL1,1)
                  CALL XVTRANS(F2HBUF,NC(I),NC1,1)
				  status1 = TXTTEXT(IMAGE,NL1,NS1,ISS1,ISL1,LOC(I),NC1,
     &                 TEXT1(1),FLAG)
               ENDDO
            ENDDO


 1          CONTINUE
         ENDDO 
            ! Text drawing loop

c....data to now be placed in the output data set....but in order to
c    to write into the output in the update mode a XVREAD must be performed
c    on the data set to be written to before the XVWRIT can be.
	
         DO J=1,NL
C		   IF (J .EQ. 1)  THEN
C		     WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   J=',J,'  WRITE IMAGE(',((J-1)*NS+1),') LINE=',SLOUT+J-1,' SAMP=',SS,' NSAMPS=',NS
C           ELSEIF (J .EQ. NL) THEN
C		    WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6)') '   J=',J,'  WRITE IMAGE(',((J-1)*NS+1),') LINE=',SLOUT+J-1,' SAMP=',SS,' NSAMPS=',NS
C		   ENDIF
            CALL XVWRIT(OUTU,IMAGE((J-1)*NS+1),STATUS,
     &           'LINE',SLOUT+J-1,'SAMP',SS,'NSAMPS',NS,' ')
C	         WRITE (*,'(A,I1,A,I4)') '     XVWRIT status ',STATUS,' J=',J
         ENDDO
      
C	 WRITE (*,'(A)') 'XXXX loop END XXXXXX ' 
C	 WRITE (*,'(A)') 'XX ' 
C	 WRITE (*,'(A,I6,A,I6)') 'XX   CHUNK SLCHUNK=',SLCHUNK,' NLCHUNK=',NLCHUNK
C	 WRITE (*,'(A,I6,A,I6, A,I6,A,I6)') 'XX   startLine SIZE(1)=',SIZE(1),' startSample SIZE(2)=',SIZE(2),'  NL SIZE(3)=',SIZE(3),' NS SIZE(4)=',SIZE(4)
C	 WRITE (*,'(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6)') 'XX   SS=',SS,' SL=',SL,' SLOUT=',SLOUT,'  NL=',NL,' NS=',NS,' TCOUNT=',TCOUNT
C	 WRITE (*,'(A)') 'XX ' 
C	 WRITE (*,'(A)') 'XXXXXXXXXXXXXXXXXXXXXX ' 
      ENDDO          ! Chunking loop

C		WRITE (*,'(A)') 'After Chunking loop. Close files '
	  CALL XVCLOSE(INU,STATUS,' ')
      CALL XVCLOSE(OUTU,STATUS,' ')
c.........all format statements below except for # 1.
 2    FORMAT( ' PART OR ALL OF TEXT STRING',I3,
     &     ' WAS WRITTEN OFF THE IMAGE')
 4    FORMAT( ' TEXT STRING',I3,
     &     ' EXCEEDS PROGRAMS SIZE LIMITATIONS.')
 
 9999 RETURN
      END
