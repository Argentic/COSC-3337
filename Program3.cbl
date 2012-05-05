       IDENTIFICATION DIVISION.
        PROGRAM-ID.
           Project3.
      *AUTHOR.
      *    Lindsey Smith.
      *DATE-WRITTEN.
      *    5/4/2012.
      *PROGRAM-DESCRIPTION.
      *    Updates customer files.
        
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
             SELECT INPUT-FILE ASSIGN TO "customer-file".
             ORGANIZATION IS RELATIVE
             ACCESS IS RANDOM
  
             SELECT BUCKET-FILE ASSIGN TO "customer-file"
             ORGANIZATION IS RELATIVE
             ACCESS IS RANDOM
            
             SELECT B-INDEX ASSIGN TO "customer-file"
             ORGANIZATION IS RELATIVE
             ACCESS IS RANDOM.

        DATA DIVISION.
        FILE SECTION.
        FD INPUT-FILE.
        01 INPUT-REC.
           05 INAME PIC X(30).
           05 IPHONE PIC 9(10).
           05 IADDRESS PIC X(40).
           05 ICITY PIC X(30).
           05 ISTATE PIC XX.
           05 IZIP PIC 9(5).  
        

        WORKING-STORAGE SECTION.
	01 STATS PIC 9.
	01 RECORD-NUM PIC 9(9).
	01 USER-INPUT-FLAG  PIC X.
	01 USER-INPUT-PHONE PIC 9(10).
	01 USER-INPUT-NAME PIC X(30).
        
        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
        	CALL 'OPEN-CUSTOMERS' USING INPUT-FILE.
        	CALL 'OPEN-CUSTOMER-PHONE' USING BUCKET-FILE.
        	CALL 'OPEN-CUSTOMER-NAME' USING B-INDEX.
        	CALL 'READ-CUSTOMERS' USING (STATS,INPUT-REC, RECORD-NUM).
        	
        	 PERFORM UNTIL USER-INPUT-FLAG = "X".
        	 PERFORM 100-USER-INTERFACE.
        
    
        	
        	CALL 'CLOSE-CUSTOMERS'.
        	CALL 'CLOSE-CUSTOMER-PHONE'.
		CALL 'CLOSE-CUSTOMER-NAME'.
		STOP RUN.
	
        
        100-USER-INTERFACE.
	        DISPLAY "Please select an action:".
	        DISPLAY AFTER ADVANCING 1 LINE "Add an Account (A)".
	        DISPLAY AFTER ADVANCING 1 LINE "Search by Name (N)".
	        DISPLAY AFTER ADVANCING 1 LINE "Search by Phone (P)".
	        DISPLAY AFTER ADVANCING 1 LINE "Exit Program (X)".
	       	ACCEPT USER-INPUT-FLAG.
	       	IF USER-INPUT-FLAG = "A" or "N" or "P" or "X" then
	       	   PERFORM 101-CHOICE-MAKER
	       	ELSE
	       	   ACCEPT USER-INPUT-FLAG.

         101-CHOICE-MAKER.	
        	IF USER-INPUT-FLAG = "A"
        	    DISPLAY "Please enter customer name"
        	    ACCEPT USER-INPUT-NAME
        	    DISPLAY "Please enter customer phone"
        	    ACCEPT USER-INPUT-PHONE
        	    CALL ADD-CUSTOMER-PHONE USING (STATS, USER-INPUT-PHONE, RECORD-NUMBER)
        	    	IF STATS = 1 THEN
        	    	   DISPLAY "Phone already exists in index"
        	    	ELSE IF STATS = 2 THEN
        	    	   DISPLAY "An error occurred"
        	    CALL ADD-CUSTOMER-NAME USING (STATS, USER-INPUT-NAME, RECORD-NUMBER)
        	    MOVE USER-INPUT-NAME TO INAME.
        	    MOVE USER-INPUT-PHONE TO IPHONE.
        	    CALL 'WRITE-CUSTOMERS' USING STATS, CUST-REC, RECORD-NUMBER
        	 
        	 ELSE IF USER-INPUT-FLAG = "P"
        	   CALL 'SEARCH-CUSTOMER-PHONE' USING (POSITION, USER-INPUT-PHONE.
        	   
        	 ELSE IF USER-INPUT-FLAG = "N"
        	    DISPLAY "Please enter customer name".
        	    ACCEPT USER-INPUT-NAME.        	 
        	   CALL 'SEARCH-CUSTOMER-NAME' USING POSITION, USER-INPUT-NAME.
        	     IF POSITION = O THEN
        	        DISPLAY "Name not found!"
        	     ELSE
        	        DISPLAY USER-INPUT-NAME " found at " POSITION
        	        WHILE POSITION NOT 0, LOOP
        	        CALL 'NEXT-CUSTOMER-NAME' USING POSITION, USER-INPUT-NAME
        	        DISPLAY USER-INPUT-NAME " found at " POSITION.
	
      
      

      