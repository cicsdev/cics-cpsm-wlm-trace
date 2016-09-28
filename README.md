# CICSPlex SM Workload Manager trace flag switcher

From time to time, CICSPlex SM (CPSM) Service has to instruct customers how to switch on the internal Workload Manager (WLM) trace flags to assist in the diagnosis of dynamic transaction routing queries using CPSM WLM.  These instructions require the customer to enter the CPSM debugging environment, and to activate the appropriate levels of WLM trace whilst their workloads are running. The problem is that this is not simple process for the service team to explain to customers, and is fraught with problems - notably that if the trace flags are not switched off in a timely manner, then the performance of significant areas of their CICSplex can degrade within minutes.

To ease the complexity of switching the correct flags on, and to reduce the risk of them being left active for too long, the SETTRACE program has been written. By running the CICS transaction code associated with the program (from a 3270 session logged on to a WLM routing region), users may cause the activation of the WLM trace flags required by CPSM Service for a defined period of time. The program will then count down an expiry period, which will cause the trace flags to be switched off again when that time period expires. At that point, the customer may use the CICS supplied CETR transaction to close the auxiliary trace datasets in the region, and arrange for them to be shipped to the CPSM service team for analysis. Using this transaction to complete this activity provides users with an easy method for implementing the Service team requirements, and keeps the risk of impacting their runtime production environment down to a minimum. As such, we must make is clear the the execution of the SETTRACE program should only be performed in response to instructions from the CPSM Service team.

The SETTRACE program is shipped in source form, and must be extracted from this GitHub repository, extracted from the .zip file, and uploaded to the z/OS environment, where it must be assembled with the CPSM source code libraries included in the Assembly step SYSLIB DD sequence. Ensure that the resultant object file is link-edited into a load library that is referable by the MASes in which you want to run the program.


## Program definition
To run the program in a MAS, you need to define the SETTRACE program, and a CICS transaction code to execute it, for example: "SETT". You may use the CICS CEDA transaction, or CPSM BAS for this purpose. "Other proprietary CICS Resource Definition tools are available" - and may also be used. The salient points of each definition are:

+ Program: SETTRACE
  + Language: Assembler

+ Transaction: SETT (or whatever non-clashing transaction name that you choose)
  + First program name: SETTRACE
  + System purgeable: NO
  + Terminal purgeable: NO

The transaction should not be purgeable, because otherwise the trace flags could be left on, causing trace flooding in the issuing region AND its connected CMAS. All other Program and Transaction definitions can be left to default, or to conform to your local CICSplex standards.


## Running the program
To run the program, start a 3270 session where WLM traces are required to be issued (normally a TOR/Routing region) and enter the "SETT" (or whatever code you chose) transaction code. This will cause WLM trace flags 18, 19 and 23 through 27 to be switched on for 5 seconds, and then all WLM trace flags will be reverted to their original setting. In a busy workload, five seconds is normally enough to generate hundreds of trace entries.

If you desire to override the five second interval with a different period - which should only be done under advice from the CPSM Service Engineers - then you may issue: "SETT nn"  -  where "nn" is the time period in seconds, from 1 to 59 seconds. Any other value will be rejected.

On completion of the transaction, the associated CICS auxiliary trace datasets will be closed ready for analysis under the instruction of CPSM Service.


## Points to note
This program is simplistic in its operation. To aid debugging, it writes data to a "DWDEBUG" Temporary Storage (TS) queue. If you experience security exceptions when the program attempts to write to this queue then:
+ If the program  completed successfully they may be ignored,
+ Or you could simply comment out the EXEC CICS DELETEQ and EXEC CICS WRITEQ TS commands in the source code,
+ Or you can authorise the transaction to have full TS Queue access.

It should be self-evident that this program will cause CPSM WLM to write trace entries to the CICS Auxiliary Trace datasets in the local CICS region (MAS).  A feature of the CPSM trace function is that it also copies local trace entries to the trace datasets of the CMAS that the local region is connected to. For trace entries to be recorded in the local trace datasets, then CICS Master User Trace recording must be switched ON - the program will activate CICS Auxiliary Trace recording after activation the CPSM trace flags. If the Master User trace flag is not switched on, then the local trace dataset will not record the progress of your dynamic routing traffic even though the CPSM WLM trace flags will be activated. However, all CPSM trace entries generated during this time (not just those arising from the SETTRACE execution) will still be sent to the MAS's parent CMAS, and should be recorded in that CMAS's trace datasets.  On completion of the SETTRACE program, the local CICS Auxiliary Status will be set to STOPPED, so that the Auxiliary Trace datasets may be safely copied for analysis by CPSM Service. Once the files are safely extracted, then the Auxiliary Trace Status should be STARTED again (using the CETR transaction), to ensure that any subsequent CPSM exception conditions are captured during the operation of your CICSplex. 


## License

This project is licensed under Apache License Version 2.0.
