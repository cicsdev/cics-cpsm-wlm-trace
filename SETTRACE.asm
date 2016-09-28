*ASM     XOPTS(PROLOG EPILOG SP)                                       
         TITLE 'CPSM WLM Trace Switcher'
***********************************************************************
* Licensed Materials - Property of IBM                                *
*                                                                     *
* SAMPLE                                                              *
*                                                                     *
* (c) Copyright IBM Corp. 2016 All Rights Reserved                    *       
*                                                                     *
* US Government Users Restricted Rights - Use, duplication or         *
* disclosure restricted by GSA ADP Schedule Contract with IBM Corp    *
*                                                                     *
***********************************************************************
*                                                                     *
* MODULE NAME = SETTRACE                                              *
*                                                                     *
* DESCRIPTIVE NAME = CPSM WLM Trace switcher                          *
*                                                                     *
* FUNCTION = Switch on predefined WLM trace levels for a limited      *
*            time period.                                             *
*                                                                     *
* ENTRY POINT = SETTRACE                                              *
*                                                                     *
*     PURPOSE = CICSPlex SM WLM trace switcher                        *
*                                                                     *
*     LINKAGE = From the transaction code associated with this        *
*               program, entered from a 3270 terminal session.        *
*                                                                     *
*     INPUT   = "SETT nn" - where "nn" may be omitted, or take any    *
*               numeric value from 1 to 59, which is the designated   *
*               wait period between activating and de-activating the  *
*               CICSPlex SM workload manager trace.                   *
*                                                                     *
* NOTES :                                                             *
*    DEPENDENCIES = S/370                                             *
*    RESTRICTIONS = None                                              *
*    REGISTER CONVENTIONS =                                           *
*        R0              Work register                                *
*        R1              Work register                                *
*        R2              Work register                                *
*        R3              Work register                                *
*        R4              Work register                                *
*        R5              Work register                                *
*        R6              Work register                                *
*        R7              Work register                                *
*        R8              Work register                                *
*        R9              Internal program tracking                    *
*        R10             Work register                                *
*        R11             EIB pointer                                  *
*        R12             Base Register 1                              *
*        R13             Working Storage pointer                      *
*        R14             Subroutine linkage                           *
*        R15             Subroutine linkage and Work register         *
*                                                                     *
*    MODULE TYPE  = Executable                                        *
*    PROCESSOR    = Assembler                                         *
*    ATTRIBUTES   = Read only, Serially Reusable                      *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
* USAGE NOTES :                                                       *
* This program should ONLY be used under the instruction of the CICS  *
* Service staff. It is intended as an aid to CICSPlex SM diagnostic   *
* collection only.                                                    *
*                                                                     *
* Users of this file will be required to assemble it to make an       *
* executable version. The program must be defined to the CICS         *
* region where it will execute, with an associated CICS Transaction   *
* Code:                                                               *
*                                                                     *
* - Program name: SETTRACE                                            *
*   - Language: Assembler                                             *
*                                                                     *
* - Transaction name "SETT" - or any other four character id that     *
*                             does not clash with installed TranIDs.  *
*   - First program name: SETTRACE                                    *
*   - System Purge: NO                                                *
*   - Terminal Purge: NO                                              *
*                                                                     *
* The transaction should not be purgeable, otherwise the trace        *
* flags could be left on, causing trace flooding in the issueing      *
* region AND its connected CMAS.                                      *
*                                                                     *
* To run it, start a 3270 session where WLM traces are required to be *
* issued (normally a TOR/Routing region) and enter the "SETT"         *
* transaction code. This will cause WLM trace flags 18, 19 and 23     *
* thru 27 to be switched on for 5 seconds, and then all WLM trace     *
* flags will be switched off again.                                   *
*                                                                     *
* To activate the trace flags for a different interval, issue:        *
* "SETT nn"  -  where "nn" is the time period in seconds, from 1 to   *
* 59 seconds. Any other value will be rejected.                       *
*                                                                     *
*---------------------------------------------------------------------*
* Version : 1.0 - 23/08/2016                                          *
* Basic program structure delivered and functional                    *
*---------------------------------------------------------------------*
* Version : 1.1 - 26/08/2016                                          *
* Improvements to parameter validation and wait interval reporting.   *
*---------------------------------------------------------------------*
* Version : 1.2 - 31/08/2016                                          *
* Code tidied up and documented.                                      *
*---------------------------------------------------------------------*
* Version : 1.3 - 28/09/2016                                          *
* Trac flag reset implelented.                                        *
***********************************************************************
R0       EQU   0                                                        
R1       EQU   1                                                        
R2       EQU   2                                                        
R3       EQU   3                                                        
R4       EQU   4                                                        
R5       EQU   5                                                        
R6       EQU   6                                                        
R7       EQU   7                                                        
R8       EQU   8                                                        
R9       EQU   9                                                        
R10      EQU   10                                                       
R11      EQU   11                                                       
R12      EQU   12                                                       
R13      EQU   13                                                       
R14      EQU   14                                                       
R15      EQU   15                                                       
         LCLB  &DEBUG                  DWDEBUG TS Queue control
&DEBUG   SETB  1                       Don't generate debug code
         EJECT                                                          
*        DFHCSAD TYPE=DSECT                                             
         EJECT                                                          
*        DFHSIT TYPE=DSECT                                              
         EJECT                                                          
*        DFHAFCS TYPE=DSECT                                             
         EJECT                                                          
DFHEISTG DSECT                                                          
WRK_PGM    DS  CL8                     Program Id
WRK_THREAD DS  D                       CPSM thread token
WRK_RSLTT  DS  D                       Result set token
*
WRK_EYE    DS  CL4                     Eyecatcher literal
WRK_TRACK  DS  F                       Internal program execution track
WRK_RESP   DS  F                       CICS/CPSM response code
WRK_REAS   DS  F                       CICS/CPSM reason code
WRK_COUNT  DS  F                       Result set record count
WRK_CURR   DS  F                       Result set current record
WRK_LEN    DS  F                       Result set buffer length
*
TRANNAME DS    CL4                     Saved transaction name
*
WRK_JUST       DS    H                 SEND TEXT row number
WRK_TXTLEN     DS    H                 SEND TEXT data length
WRK_INTRVAL    DS    F                 Binary wait interval
*
WRK_CHAR       DS    CL6               Character wait interval
WRK_TIME       DS    PL8               Decimal wait interval
WAITTIME       DS    CL2               Message wait interval
*
WRK_MAS        DS    0CL18             CICSNAME CRITERIA string
               DS    CL9
WRK_MASNAME    DS    CL8               Local MAS name
               DS    CL1
*
WRK_PLEXNAME   DS    CL8               Local CICSplex name
WRK_VER        DS    CL4               Local CPSM version
*
WRK_SAVEFLAGS  DS    CL4               Saved WLM trace flags
WRK_SAVEDEC    DS    PL8               Saved flags in decimal
WRK_SAVECHAR   DS    0CL15             Saved flags in char
               DS    CL5
WRK_SAVED      DS    CL10              SET MODIFY Value
*
WRK_MD2        DS    0CL20
               DS    CL9
WRK_MD2I       DS    CL10              Saved flags insertion
               DS    CL1
*
WRK_SYS        DS    0CL11             SYSID CRITERIA string
               DS    CL6
MYSYSID        DS    CL4               Local SYSID
               DS    CL1 
*
WRK_MSG1       DS    0CL46             MSG1 work buffer
               DS    CL38
WRK_MSG1I      DS    CL8               MSG1 MAS name
*
WRK_MSG2       DS    0CL45             MSG2 work buffer
               DS    CL8
WRK_MSG2I      DS    CL2               MSG2 wait interval
               DS    CL35
*
WRK_DOTS       DS    CL59              Interval popper buffer
*
WRK_ADJFLAG    DS    CL1               Sign conversion flag
*
TIOALEN        DS    H                 TIOA length
MYTIOA         DS    CL80              Input TIOA buffer
*
GETAREA        DS    0F                CPSM I/O buffer
OBJAREA        DS    CL(OBJSTAT_TBL_LEN) OBJSTAT buffer       
MASAREA        DS    CL(MAS_TBL_LEN)   MAS buffer         
               ORG   MASAREA
CMTAREA        DS    CL(CMTPMLNK_TBL_LEN) CMTPMLNK buffer
GETAREAL       EQU   OBJSTAT_TBL_LEN+MAS_TBL_LEN I/O buffer length
               ORG   ,
*                                                                       
         SPACE                                                          
         COPY  EYUA2594                OBJSTAT                          
         SPACE                                                          
         COPY  EYUA2443                MAS                              
         SPACE                                                          
         COPY  EYUA2445                CMTPMLNK                         
         SPACE                                                          
*********************************************************************** 
*        INITIALISATION.                                                
*********************************************************************** 
SETTRACE DFHEIENT CODEREG=(12),DATAREG=13,EIBREG=11                     
SETTRACE AMODE 31                                                       
SETTRACE RMODE ANY                                                      
*                                                                       
         MVC   WRK_PGM,=CL8'SETTRACE'  Save program Id in DSA                                 
         MVC   WRK_EYE,=CL4'EYE>'      Set DSA eyecatcher string           
         XC    WRK_TRACK,WRK_TRACK     Zeroise track variable          
         XC    WRK_COUNT,WRK_COUNT     Zeroise counter variabe           
*                                                                       
         MVC   TRANNAME,EIBTRNID       Save TranID in DSA          
*                                                                       
* Who am I? Get my SYSID
*
         MVC   WRK_SYS,CNST_SYS        Initialise CRITERIA string
         EXEC  CICS ASSIGN SYSID(MYSYSID) NOHANDLE 
*
* Suck in the TIOA. How long do they want trace running?
         MVC   TIOALEN,=AL2(L'MYTIOA)  Initialise TIOA length
         XC    MYTIOA,MYTIOA           and clear it
*
         EXEC CICS RECEIVE INTO(MYTIOA) LENGTH(TIOALEN)
*
         EXEC CICS SEND CONTROL ERASE WAIT TERMINAL
*
         LA   R15,HDR_LEN              Set the SEND TEXT length ...
         STH  R15,WRK_TXTLEN           ... and save it
         LA   R15,1                    Set the screen line number
         STH  R15,WRK_JUST             and save it for SEND
*
         EXEC CICS SEND TEXT FROM(CNST_HDR)                            X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT NOHANDLE
*                                                          
         EXEC CICS SEND PAGE NOHANDLE
*
         MVC   WRK_JUST,=AL2(3)        Set screen line number
         CLC   TIOALEN,=AL2(5)         Just the Transid entered?
         BNH   DEFTIME                 Yes, set default wait time
*
* Ok we got some parameter data to parse. It should be the required
* wait time, somewhere between 1 and 59 seconds.
         LH    R5,TIOALEN              Load the TIOA length
         LA    R6,MYTIOA(R5)           Address end of TIOA
         BCTR  R6,0                    Address last TIOA byte
         SH    R5,=H'5'                Set the parm length
         CH    R5,=H'6'                Parm len GT 6?
         BH    INVPARM                 Yes, bad!
         MVC   WRK_CHAR,=CL6'000000'   Prime pack area
         LA    R7,WRK_CHAR+5           Address last byte of packarea
TIOALOOP DS    0H
         CLI   0(R6),C'0'              Less than zero?
         BL    INVPARM                 Yes, bad!
         CLI   0(R6),C'9'              Higher than 9?
         BH    INVPARM                 Yes, bad!
         MVC   0(1,R7),0(R6)           Move digit to pack area
         BCTR  R6,0                    Back up 1 source byte
         BCTR  R7,0                    Back up 1 target byte
         BCT   R5,TIOALOOP             Scan each parm char
*
* We have all numerics if we drop here. Convert the value to decimal.
         PACK  WRK_TIME,WRK_CHAR       Pack the value        
         CP    WRK_TIME,=PL4'59'       Greater than 59 seconds?
         BH    INVPARM                 Yes, bad!
         CP    WRK_TIME,=PL4'1'        Less than 1 second?
         BL    INVPARM                 Yes, bad! 
         MVC   WAITTIME,WRK_CHAR+4     Save the char form
         B     GOTTIME                 Carry on     
*
* No interval value was specified - default is 5 seconds.
DEFTIME  DS    0H                      
         ZAP   WRK_TIME,=P'5'          Decimal 5 seconds
         MVC   WAITTIME,=CL2'05'       Character 5 seconds
GOTTIME  DS    0H
         CVB   R15,WRK_TIME            Convert interval to binary ...
         ST    R15,WRK_INTRVAL         ... and store it for wait loop
*
         AIF   (&DEBUG).DBUG010
* Clear down and prime the TS Queue that we'll be reporting to. This 
* queue is for debugging purposes - has no functional value.
*
         EXEC CICS DELETEQ TS QUEUE('DWDEBUG')                         X
                             NOHANDLE                                   
*                                                                       
         EXEC CICS WRITEQ TS QUEUE('DWDEBUG')                          X
                             FROM(WRK_THREAD)                          X
                             LENGTH(128)                               X
                             NOHANDLE                                   
*                                                                       
.DBUG010 ANOP
* Perform an unqualifed CPSM CONNECT to plug ourselves into a CMAS
* at the lowest supported CPSM version (0420).
         EXEC CPSM CONNECT VERSION('0420')                             X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*                                                                       
         LA    R9,1                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good CONNECT? 
         BNE   BOOM                    No :-(                 
*
* Now grab the CMTPMLNK record that matches my CICS SYSID
         EXEC CPSM GET OBJECT('CMTPMLNK')                              X
                       COUNT(WRK_COUNT)                                X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       CRITERIA(WRK_SYS)                               X
                       LENGTH(11)                                      X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*                                                                       
         LA    R9,2                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good GET? 
         BNE   BOOM                    No :-(                 
*
* Save the GET response
         AIF   (&DEBUG).DBUG020
         EXEC CICS WRITEQ TS QUEUE('DWDEBUG')                          X
                             FROM(WRK_THREAD)                          X
                             LENGTH(128)                               X
                             NOHANDLE                                   
.DBUG020 ANOP
*                                                                       
         LA    R15,GETAREAL            Set I/O buffer length
         ST    R15,WRK_LEN             Save for FETCH
         ICM   R6,B'1111',WRK_COUNT    Get returned record count
         ST    R6,WRK_CURR             Save the gotten record count
*
         LA    R9,3                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_CURR,=AL4(1)        Should have only one record!
         BNE   BOOM                    No :-(
*
* Now fetch our CMTPMLNK record, and extract our MAS and Plex names
FTCHCMTP DS    0H
         XC    OBJAREA(OBJSTAT_TBL_LEN),OBJAREA Clear I/O buffer
         XC    CMTAREA(CMTPMLNK_TBL_LEN),CMTAREA Clear I/O buffer
*                                                                      
         EXEC CPSM FETCH INTO(GETAREA)                                 X
                       LENGTH(WRK_LEN)                                 X
                       BOTH                                            X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*                                                                       
         LA    R9,4                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good FETCH? 
         BNE   BOOM                    No :-(                 
*                                                                       
         AIF   (&DEBUG).DBUG030
* Stash the gotten CMTPMLNK record in our TS queue.
         EXEC CICS WRITEQ TS QUEUE('DWDEBUG')                          X
                             FROM(GETAREA)                             X
                             LENGTH(WRK_LEN+2)                         X
                             NOHANDLE                                   
.DBUG030 ANOP
*                                                                       
         MVC   WRK_MAS,CNST_MAS        Format CICSNAME Criteria string
         USING CMTPMLNK,CMTAREA
         MVC   WRK_MASNAME,CMTPMLNK_NAME Save the MAS name
         MVC   WRK_PLEXNAME,CMTPMLNK_PLEXNAME Save the Plex name
*                                                                  
* So DISCONNECT and ReCONNECT to the PLEX that we belong to.
*
         EXEC CPSM DISCARD RESULT(WRK_RSLTT)                           X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*
         LA    R9,5                    Program ...       
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good DISCARD? 
         BNE   BOOM                    No :-(                 
*
         XC    WRK_RSLTT,WRK_RSLTT     Clear Result Set Id
*
         EXEC CPSM DISCONNECT                                          X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*
         LA    R9,6                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good DISCONNECT? 
         BNE   BOOM                    No :-(                 
*
         XC    WRK_THREAD,WRK_THREAD   Clear Thread Token
*
         EXEC CPSM CONNECT CONTEXT(WRK_PLEXNAME)                       X
                           VERSION('0420')                             X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*
         LA    R9,7                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good CONNECT? 
         BNE   BOOM                    No :-(                 
*
* Now get the MAS that we want (us!).
         EXEC CPSM GET OBJECT('MAS')                                   X
                       COUNT(WRK_COUNT)                                X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       CRITERIA(WRK_MAS)                               X
                       LENGTH(18)                                      X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,8                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good GET? 
         BNE   BOOM                    No :-(                 
*
         ICM   R6,B'1111',WRK_COUNT    Get returned record count
         ST    R6,WRK_CURR             Save the gotten record count
*
         LA    R9,9                    Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_CURR,=AL4(1)        Should have only one record!
         BNE   BOOM                    No :-(
*
* Now fetch our MAS record after clearing the I/O buffer area.
         XC    OBJAREA(OBJSTAT_TBL_LEN),OBJAREA Clear I/O buffer
         XC    MASAREA(256),MASAREA    Clear I/O buffer ...
         XC    MASAREA+256(MAS_TBL_LEN-256),MASAREA+256 .. and again
         LA    R15,GETAREAL            Set I/O buffer length ...
         ST    R15,WRK_LEN             ...and save it for FETCH
*                                                          
         EXEC CPSM FETCH INTO(GETAREA)                                 X
                       LENGTH(WRK_LEN)                                 X
                       BOTH                                            X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,10                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good FETCH? 
         BNE   BOOM                    No :-(                 
*
* Extract the real CPSM version of the MAS.
         USING OBJSTAT,OBJAREA         Cover the OBJSTAT buffer
         USING MAS,MASAREA             Cover the MAS buffer
         MVC   WRK_VER,MAS_CPSMVER     Save the CPSM version of the MAS
*                                                               
         AIF   (&DEBUG).DBUG040
* Stash the gotten OBJSTAT/MAS record in our TS queue.
         EXEC CICS WRITEQ TS QUEUE('DWDEBUG')                          X
                             FROM(GETAREA)                             X
                             LENGTH(WRK_LEN+2)                         X
                             NOHANDLE                                   
*                                                                       
.DBUG040 ANOP
* So DISCONNECT and ReCONNECT to the PLEX at our MAS's CPSM version.
*
         EXEC CPSM DISCARD RESULT(WRK_RSLTT)                           X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*                                                                       
         LA    R9,11                   Program ...       
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good DISCARD? 
         BNE   BOOM                    No :-(                 
*
         XC    WRK_RSLTT,WRK_RSLTT     Clear Result Set Id
*
         EXEC CPSM DISCONNECT                                          X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*
         LA    R9,12                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good DISCONNECT? 
         BNE   BOOM                    No :-(                 
*
         XC    WRK_THREAD,WRK_THREAD   Clear Thread Token
*
         EXEC CPSM CONNECT CONTEXT(WRK_PLEXNAME)                       X
                           VERSION(WRK_VER)                            X
                           THREAD(WRK_THREAD)                          X
                           RESPONSE(WRK_RESP)                          X
                           REASON(WRK_REAS)                             
*
         LA    R9,13                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good CONNECT? 
         BNE   BOOM                    No :-(                 
*
* Now get our MAS at its correct CPSM version
         EXEC CPSM GET OBJECT('MAS')                                   X
                       COUNT(WRK_COUNT)                                X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       CRITERIA(WRK_MAS)                               X
                       LENGTH(18)                                      X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,14                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good GET? 
         BNE   BOOM                    No :-(                 
*
         ICM   R6,B'1111',WRK_COUNT    Get returned record count
         ST    R6,WRK_CURR             Save the gotten record count
*
         LA    R9,15                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_CURR,=AL4(1)        Should have only one record!
         BNE   BOOM                    No :-(
*
* Now fetch our MAS record after clearing the I/O buffer area.
         XC    MASAREA(256),MASAREA    Clear I/O buffer ..
         XC    MASAREA+256(MAS_TBL_LEN-256),MASAREA+256 .. and again
         LA    R15,MAS_TBL_LEN         Set I/O buffer length for MAS ..
         ST    R15,WRK_LEN             .. and save it for FETCH
*                                                                   
         EXEC CPSM FETCH INTO(MASAREA)                                 X
                       LENGTH(WRK_LEN)                                 X
                       DATA                                            X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,16                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good FETCH? 
         BNE   BOOM                    No :-(                 
*
         AIF   (&DEBUG).DBUG050
* Stash the gotten MAS record in our TS queue.
*                                            
         EXEC CICS WRITEQ TS QUEUE('DWDEBUG')                          X
                             FROM(MASAREA)                             X
                             LENGTH(WRK_LEN+2)                         X
                             NOHANDLE                                   
.DBUG050 ANOP
*
         LA    R15,L'CNST_MSG1         Set length of MSG1 buffer ...
         STH   R15,WRK_TXTLEN          ... and store for SEND
         MVC   WRK_MSG1,CNST_MSG1      Initialise MSG1 buffer
         MVC   WRK_MSG1I,WRK_MASNAME   Insert MAS name
         LH    R15,WRK_JUST            Load the screen line number
         LA    R15,1(,R15)             Bump it
         STH   R15,WRK_JUST            Save it for SEND
*
         EXEC CICS SEND TEXT FROM(WRK_MSG1)                            X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT
*
         EXEC CICS SEND PAGE
*
* So! We have a MAS record in our buffer. Now fiddle with its 
* trace flags!
         MVC   WRK_SAVEFLAGS,MAS_WLMTRACE Save the current WLM flags
         MVI   WRK_ADJFLAG,C'N'           Prime adjustment flag
         TM    WRK_SAVEFLAGS,X'80'        Is top bit on?
         BZ    CONVDEC                    No, carry on
         MVI   WRK_ADJFLAG,C'Y'           Yes, set adjustment flag
         NI    WRK_SAVEFLAGS,X'7F'        Switch off sign bit
CONVDEC  DS    0H
         ICM   R15,B'1111',WRK_SAVEFLAGS  Convert WLM flags value ...
         CVD   R15,WRK_SAVEDEC            ...to decimal
         CLI   WRK_ADJFLAG,C'Y'           Adjusted?
         BNE   GETCHAR                    no, carry on
         AP    WRK_SAVEDEC,CNST_8000      Add back sign adjustment
GETCHAR  DS    0H
         UNPK  WRK_SAVECHAR,WRK_SAVEDEC   Convert to zoned character
         OI    WRK_SAVECHAR+L'WRK_SAVECHAR-1,X'F0' Sort out last byte
*
* and write it back!
         EXEC CPSM SET MODIFY(CNST_MOD)                                X
                       LENGTH(L'CNST_MOD)                              X
                       ALL                                             X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,17                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good SET? 
         BNE   BOOM                    No :-(                 
*
* OK the WLM trace is now running! Wait for a little while. 
         MVC   WRK_MSG2,CNST_MSG2      Initialise MSG2 buffer
         MVC   WRK_MSG2I,WAITTIME      Insert wait time string
         LA    R15,L'WRK_MSG2          Set message buffer length
         STH   R15,WRK_TXTLEN          Store length for SEND
         LH    R15,WRK_JUST            Load the screen line number
         LA    R15,1(,R15)             Bump it
         STH   R15,WRK_JUST            Save it for SEND
*
         EXEC CICS SEND TEXT FROM(WRK_MSG2)                            X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT
*
         EXEC CICS SEND PAGE
*
* Silly flashy bit here, but I can't help myself! :0)
         MVI   WRK_DOTS,C'.'           Make a row of dots
         MVC   WRK_DOTS+1(L'WRK_DOTS-1),WRK_DOTS
         LH    R15,WRK_JUST            Load the screen line number
         LA    R15,1(,R15)             Bump it
         STH   R15,WRK_JUST            Save it for SEND
         LA    R7,1                    Start with a single dot ...
         STH   R7,WRK_TXTLEN           ... stored for SEND
         L     R15,WRK_INTRVAL         Set the loop count
DOTLOOP  DS    0H
         ST    R15,WRK_INTRVAL         Store new loop count
         EXEC CICS SEND TEXT FROM(WRK_DOTS)                            X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT
*
         EXEC CICS SEND PAGE
*
         EXEC CICS DELAY FOR SECONDS(1)
*
         LA    R7,1(R7)                Bump the TIOA buffer length
         STH   R7,WRK_TXTLEN           and store it back
         L     R15,WRK_INTRVAL         Load our loop count
         BCT   R15,DOTLOOP             and go around again till expiry
*
* Now get our modified MAS record
         EXEC CPSM GET OBJECT('MAS')                                   X
                       COUNT(WRK_COUNT)                                X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       CRITERIA(WRK_MAS)                               X
                       LENGTH(18)                                      X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,18                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good GET? 
         BNE   BOOM                    No :-(                 
*
         ICM   R6,B'1111',WRK_COUNT    Get returned record count
         ST    R6,WRK_CURR             Save the gotten record count
*
         LA    R9,19                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_CURR,=AL4(1)        Should have only one record!
         BNE   BOOM                    No :-(                 
*
* Now fetch our MAS record after clearing the I/O buffer area.
         XC    MASAREA(256),MASAREA                           
         XC    MASAREA+256(MAS_TBL_LEN-256),MASAREA+256 
         LA    R15,MAS_TBL_LEN         Set I/O buffer length for MAS ..
         ST    R15,WRK_LEN             .. and save it for FETCH
*                                                                       
         EXEC CPSM FETCH INTO(MASAREA)                                 X
                       LENGTH(WRK_LEN)                                 X
                       DATA                                            X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,20                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good FETCH? 
         BNE   BOOM                    No :-(                 
*
         AIF   (&DEBUG).DBUG060
* Stash the gotten MAS record in our TS queue.
*                                                                       
         EXEC CICS WRITEQ TS QUEUE('DWDEBUG')                          X
                             FROM(MASAREA)                             X
                             LENGTH(WRK_LEN+2)                         X
                             NOHANDLE                                   
.DBUG060 ANOP
*
* Now reset the WLM trace flags back to their orginal setting.
         MVC   MAS_WLMTRACE,WRK_SAVEFLAGS Restore the flags
         MVC   WRK_SAVED,WRK_SAVECHAR+(L'WRK_SAVECHAR-L'WRK_SAVED)
         MVC   WRK_MD2,CNST_MD2        Set Modify string
         MVC   WRK_MD2I,WRK_SAVED      Set flag value
*                                                                       
* and write it back!
         EXEC CPSM SET MODIFY(WRK_MD2)                                 X
                       LENGTH(L'WRK_MD2)                               X
                       ALL                                             X
                       THREAD(WRK_THREAD)                              X
                       RESULT(WRK_RSLTT)                               X
                       RESPONSE(WRK_RESP)                              X
                       REASON(WRK_REAS)                                 
*
         LA    R9,21                   Program ...                           
         ST    R9,WRK_TRACK            ... tracking                            
         CLC   WRK_RESP,EYUVALUE(OK)   Good SET? 
         BNE   BOOM                    No :-(                 
*
         LA    R15,L'CNST_MSG3         Set length of MSG3 buffer ...
         STH   R15,WRK_TXTLEN          ... and store for SEND
         LH    R15,WRK_JUST            Load the screen line number
         LA    R15,1(,R15)             Bump it
         STH   R15,WRK_JUST            Save it for SEND
*
         EXEC CICS SEND TEXT FROM(CNST_MSG3)                           X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT NOHANDLE
*
         LA    R15,L'CNST_MSG4         Set length of MSG4 buffer ...
         STH   R15,WRK_TXTLEN          ... and store for SEND
         LH    R15,WRK_JUST            Load the screen line number
         LA    R15,1(,R15)             Bump it
         STH   R15,WRK_JUST            Save it for SEND
*
         EXEC CICS SEND TEXT FROM(CNST_MSG4)                           X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT NOHANDLE
*
         EXEC CICS SEND PAGE NOHANDLE
*
*
*
DISCONN  DS    0H                                                       
         EXEC CPSM DISCONNECT                                          X
                   THREAD(WRK_THREAD)                                  X
                   RESPONSE(WRK_RESP)                                  X
                   REASON(WRK_REAS)                                     
*                                                                       
         AIF   (&DEBUG).DBUG070
* We've ended successfully - we don't need the TS queue contents.
         EXEC CICS DELETEQ TS QUEUE('DWDEBUG')                         X
                             NOHANDLE                                   
.DBUG070 ANOP
*                                                                       
*********************************************************************** 
*        RETURN                                                         
*********************************************************************** 
RETURN   EXEC CICS RETURN                                              
*                                                                       
INVPARM  DS    0H                      Bad parameter value
         LA    R15,INV_LEN             Set length of INVALID msg buffer
         STH   R15,WRK_TXTLEN          ... and store for SEND
         LH    R15,WRK_JUST            Load the screen line number
         LA    R15,1(,R15)             Bump it
         STH   R15,WRK_JUST            Save it for SEND
*
         EXEC CICS SEND TEXT FROM(CNST_INV)                            X
                             LENGTH(WRK_TXTLEN)                        X
                             JUSTIFY(WRK_JUST)                         X
                             TERMINAL WAIT NOHANDLE
*                                                          
         EXEC CICS SEND PAGE NOHANDLE
*
         B     RETURN
*
***********************************************************************
*        BOOM - Something went wrong. Use the value in R9 to          *
*               determine where and why. The DWDEBUG TS queue may     *
*               also be worth browsing ("CEBR DWDEBUG").              *
***********************************************************************
BOOM      DC   F'0'                                                    
*                                                                      
PROGNAME  DC   CL8'SETTRACE'                                           
*
CNST_8000 DC   PL8'2147483648'         Negative sign adjustment value 
*
CNST_MAS  DC   CL18'CICSNAME=xxxxxxxx.'
CNST_SYS  DC   CL11'SYSID=xxxx.'
*
* CNST_MOD will switch on WLM 18, 19, and 23 through 27. 
CNST_MOD  DC   CL15'WLMTRACE=25568.'
* CNST_MD2 resets the flags to their previous setting. 
CNST_MD2  DC   CL20'WLMTRACE=xxxxxxxxxx.'
* CNST_MD3 will switch on WLM level 26 only. 
CNST_MD3  DC   CL12'WLMTRACE=64.'
*
CNST_HDR  DC   C'Set CICSPlex SM Workload Manager trace '
          DC   C'- Version 1.3'
HDR_LEN   EQU  *-CNST_HDR
* 
CNST_MSG1 DC   CL46'Switching on WLM trace flags for MAS: xxxxxxxx'
CNST_MSG2 DC   CL45'Waiting xx seconds before deactivating trace:'
CNST_MSG3 DC   CL39'WLM trace flags have been switched off,'
CNST_MSG4 DC   CL50'use the CETR transaction to switch off CICS trace.'
*
CNST_INV  DC   C'The parameter value must specify a wait interval to '
          DC   C'leave the trace running. The value must be numeric '
          DC   C'in the range from 1 to 59, representing the number '
          DC   C'of seconds to wait.                                '
INV_LEN   EQU  *-CNST_INV
*
***********************************************************************
*        Literal pool                                                 *
***********************************************************************
         LTORG                                                         
         END                                                           