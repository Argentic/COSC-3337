       IDENTIFICATION DIVISION.
               PROGRAM-ID.
                  Project4.
             *AUTHOR.
             *    Lindsey Smith.
             *DATE-WRITTEN.
             *    5/4/2012.
             *PROGRAM-DESCRIPTION.
             *    Updates customer files.
               
               ENVIRONMENT DIVISION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                    SELECT CALL-FILE ASSIGN TO "call-file".
                    ORGANIZATION IS RELATIVE
                    ACCESS IS RANDOM.
         	    
         	    SELECT CALL-LIST ASSIGN TO "call-file".
         	    
       
               DATA DIVISION.
               FILE SECTION.
               FD CALL-FILE.
               01 INPUT-REC.
                  05 IDATE PIC X(12).
                  05 ICALLER PIC 9(10).
                  05 ICALLEE PIC 9(10).
                  05 IDUR PIC 9(3).
               
               FD CALL-LIST.
               01 C-LIST.
                 02 LIST-LENGTH PIC 999 COMP.
                 02 POSITION PIC 9(9) COMP OCCURS 124 TIMES.
       
               WORKING-STORAGE SECTION.
       	01 STATS PIC 9.
       	01 RECORD-NUM PIC 9(9).
       	01 USER-INPUT-FLAG  PIC X.
       	01 USER-INPUT-PHONE PIC 9(10).
       	01 USER-INPUT-NAME PIC X(30).
               01 POSITION PIC 9(9) COMP.
               
               PROCEDURE DIVISION.
               MAIN-PARAGRAPH.
               	CALL 'OPEN-CALLS' USING CALL-FILE.
               	CALL 'READ-CALLS' USING (STATS,INPUT-REC, RECORD-NUM)
               	CALL 'OPEN-CALL-INDEX' USING CALL-LIST.
               	
               	
               	 PERFORM UNTIL USER-INPUT-FLAG = "X".
               	 PERFORM 100-USER-INTERFACE.
               
           
               	
               	CALL 'CLOSE-CALLS'.
       		STOP RUN.
       	
               
               100-USER-INTERFACE.
       	        DISPLAY "Please select an action:".
       	        DISPLAY AFTER ADVANCING 1 LINE "Add to List (A)".
       	        DISPLAY AFTER ADVANCING 1 LINE "Search by Numbers (N)"..
       	        DISPLAY AFTER ADVANCING 1 LINE "Exit Program (X)".
       	       	ACCEPT USER-INPUT-FLAG.
       	       	IF USER-INPUT-FLAG = "A" or "N" or "X" then
       	       	   PERFORM 101-CHOICE-MAKER
       	       	ELSE
       	       	   ACCEPT USER-INPUT-FLAG.
       
                101-CHOICE-MAKER.	
               	IF USER-INPUT-FLAG = "A"
               	    DISPLAY "Please enter customer phone"
               	    ACCEPT USER-INPUT-PHONE
               	    CALL'ADD-PHONE' USING (STATS,CALL-LIST, USER-INPUT-PHONE)
                    IF STATS NOT 0 
                      DISPLAY "Addition not successful".
                    CALL 'WRITE-CALLS' USING (STATS, INPUT-REC, RECORD-NUMBER)  
               	     
               	   
               	 ELSE IF USER-INPUT-FLAG = "N"
               	    CALL 'FIND-PHONE' USING STATS, CALL-LIST, USER-INPUT-PHONE
               	    IF STATS NOT = 0 OR STATS NOT = 1 THEN
               	    	DISPLAY "Search not successful."
               	    

      