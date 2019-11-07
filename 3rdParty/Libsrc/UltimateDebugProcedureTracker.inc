    INCLUDE('UltimateDebug.inc'),ONCE
 
!--------------------------------------------------------------------------------
UltimateDebugProcedureTracker   CLASS,TYPE,MODULE('UltimateDebugProcedureTracker.CLW'),LINK('UltimateDebugProcedureTracker.CLW')   !,_ABCLinkMode_),DLL(_ABCDllMode_)
!
!Properties
!----------
!     
udReferenceObj                      &UltimateDebug
CriticalSection                     &ICriticalSection   
TraceOn                             LONG
ProcedureID                         LONG

!Methods
!-------
Init                                PROCEDURE(UltimateDebug pUD,<STRING pProcedure>,<STRING pModuleName>,<STRING pAppName>,<STRING pModified>)
Kill                                PROCEDURE()

Construct                           PROCEDURE()
Destruct                            PROCEDURE()  

Wait                                Procedure(Long pId),Virtual
Release                             Procedure(Long pId),Virtual
Trace                               Procedure(string pStr),Virtual
                                END


