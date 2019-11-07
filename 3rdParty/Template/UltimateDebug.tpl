#TEMPLATE(UltimateDebug,'UltimateDebug Template (v18.09.15.1)'),FAMILY('ABC'),FAMILY('Clarion'),FAMILY('cw20')
#!-----------------------------------------------------------------------------------------------------
#SYSTEM
#EQUATE(%CLSkelTPLVersion,'18.12.31.1, Released 2018-12-31')
#!
#! SystemIdle (Global Extension)
#!
#EXTENSION(UltimateDebugGlobal, 'UltimateDebug (Global Extension)'), APPLICATION(ProcedureInfoExtension(UltimateDebugGlobal))
#PREPARE
  #INSERT(%CalcFamily, %CLSkelFamily)
#ENDPREPARE
#!
#!#BOXED('Information')
#!#INSERT(%CopyrightInfo)
#!#ENDBOXED
#!
#DISPLAY
#PROMPT('Disable Ultimate Debug template',CHECK),%CLSkelAppDisable,AT(10),DEFAULT(0)
#DISPLAY
#!-------------------------------------------------------------------------
#! RA.2014.03.28 - Added Debuger code generation options prompts.
#! Ties in with the additional templates for mass debugging procedures.
#!-------------------------------------------------------------------------
#BUTTON('Ultimate Debug &Generation Options'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400),AT(,,180,20)
#DISPLAY('UltimateDebug'),AT(10,0),PROP(PROP:FontStyle,700),PROP(PROP:FontName,'Tahoma')
#DISPLAY('Version ' & %CLSkelTPLVersion),AT(10,10),PROP(PROP:FontStyle,700),PROP(PROP:FontName,'Tahoma')
#DISPLAY('')
#DISPLAY('')
#DISPLAY('')
#DISPLAY('')
#SHEET,HSCROLL #!AT(,,288),HSCROLL
#TAB('General')
  #BOXED(' Ultimate Debug Generation Options ') #!,AT(,,280)
    #ENABLE(~%CLSkelAppDisable)
      #BOXED(' Application Generation Options ')
        #DISPLAY('')
        #PROMPT('Create global information procedure? ',CHECK),%gDumpTpl,DEFAULT(0),AT(10)
        #ENABLE(%gDumpTpl)
          #PROMPT('Dump global information? '          ,CHECK),%zDumpTpl,DEFAULT(0),AT(25)
        #ENDENABLE
        #DISPLAY('')
        #PROMPT('Create global variables procedure? '  ,CHECK),%gDumpVar,DEFAULT(0),AT(10)
        #ENABLE(%gDumpVar)
          #PROMPT('Dump global variables? '            ,CHECK),%zDumpVar,DEFAULT(0),AT(25)
        #ENDENABLE
        #PROMPT('Enable LineWrap (# of Chars)',@N_3),%LineWrap,DEFAULT(0)
        #DISPLAY('')
      #ENDBOXED
    #ENDENABLE
  #ENDBOXED
  #!#DISPLAY('Copyright © 1999-2014 by Roberto Artigas')
#ENDTAB
#TAB('Proc Info')
  #BOXED('') #!,AT(,,280)
    #DISPLAY('')
    #ENABLE(~%CLSkelAppDisable)
      #PROMPT('Disable This Feature',CHECK),%DisableProcedureDebug,AT(10),DEFAULT(0)
      #ENABLE(~%DisableProcedureDebug)
          #!PROMPT ('Alert Key:',KEYCODE),%UDProcAlert,DEFAULT('CtrlShiftP'),AT(100,,180) 
          #!PROMPT ('Report Keystate:',@S200),%UDReportKeyState,DEFAULT('KeyStateUD:Shift'),AT(100,,180)
          #PROMPT ('Alert Key:',KEYCODE),%UDProcAlert,DEFAULT('CtrlShiftP'),AT(100,,80) 
          #PROMPT ('Report Keystate:',@S200),%UDReportKeyState,DEFAULT('KeyStateUD:Shift'),AT(100,,80)
          #DISPLAY('')
          #DISPLAY('Available Keystates are:')
          #DISPLAY('KeyStateUD:Shift')                         
          #DISPLAY('KeyStateUD:Ctrl')                          
          #DISPLAY('KeyStateUD:Alt') 
          #DISPLAY('')
          #DISPLAY('You can use combinations by adding them.')
          #DISPLAY('Example:')
          #DISPLAY('KeyStateUD:Shift + KeyStateUD:Ctrl')   
          #DISPLAY('')
          #DISPLAY('For continuous display of Procedure Info in your Debug View window')                       
          #PROMPT (' Display Entry and Exit into procedure',CHECK),%UDEntryExit,AT(10),DEFAULT(1)
      #ENDENABLE
    #ENDENABLE
    #DISPLAY('')
  #ENDBOXED                       
#ENDTAB
#INSERT(%TabPurpose1) 
#INSERT(%TabInstructions1)
#INSERT(%TabLimitations1)
#INSERT(%TabTesting1)
#ENDSHEET
#ENDBUTTON
#!-------------------------------------------------------------------------
#DISPLAY
#SHEET,HSCROLL #!AT(,,288),HSCROLL
#TAB('General')
  #DISPLAY
  #PROMPT('Global Class:',@S40),%CLSkelClass,AT(90,,95,10),REQ,DEFAULT('UD')
  #PROMPT('This is part of a Multi-DLL program',CHECK),%CLSkelMultiDLL,AT(10),DEFAULT(0)
  #ENABLE(%CLSkelMultiDLL=1),CLEAR
    #PROMPT('Declaration:',DROP('Declared in another App[0]|Declared in this app[1]')),%CLSkelMultiDLLData,DEFAULT(0),AT(90,,95,10)
  #ENDENABLE
  #BOXED('Settings'),WHERE(%ProgramExtension = 'EXE')
      #ENABLE(%ProgramExtension = 'EXE')
          #PROMPT('Prefix:',@S20),%CLDebugPrefix,DEFAULT('!')
          #PROMPT('Turn Debug Off Variable:',@S20),%DebugOffVariable,DEFAULT(0)
          #PROMPT('Save To File Flag:',@S40),%SaveToFileVariable,DEFAULT(0)
          #PROMPT('Log File Name:',@S120),%CLLogFileName,DEFAULT('DebugLog.txt') 
          #PROMPT('Save To JSON File Flag:',@S40),%SaveToJSONFileVariable,DEFAULT(0)
          #PROMPT('JSON Log File Name:',@S120),%CLJSONLogFileName,DEFAULT('JSONDebugLog.txt') 
          #PROMPT('Do Not Split Lines',CHECK),%DoNotSplitLines,DEFAULT(1)  
          #ENABLE(%DoNotSplitLines = 0)
            #PROMPT('  Characters Per Line:',@N3),%LineWrap,DEFAULT(80)
          #ENDENABLE
      #ENDENABLE
  #ENDBOXED
  #INSERT(%TabCopyright)
#ENDTAB 
#INSERT(%TabPurpose)
#INSERT(%TabInstructions)
#INSERT(%TabContributors)
#INSERT(%TabClarionVer)
#INSERT(%TabTesting)
#ENDSHEET
#!
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
#ATSTART
  #DECLARE(%CLSkelDataExternal)
  #DECLARE(%CLSkelClassProfile)
  #SET(%CLSkelClassProfile,%CLSkelClass & 'Profile')
  #IF(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=0)
    #SET(%CLSkelDataExternal,',EXTERNAL,DLL(dll_mode)')
  #ENDIF
  #DECLARE (%ListFile),Multi,Unique
  #IF(varexists(%DefaultExport)<>1)
    #Declare(%DefaultExport)
  #EndIf
  #If(%DefaultGenerate=1 or %DefaultExport=1)
    #FOR(%File)
      #ADD (%ListFile,%File)
    #ENDFOR
  #ELSE
    #FOR(%File)
      #If(%OverrideGenerate=1)
        #ADD (%ListFile,%File)
      #EndIf
      #If (%ProgramExtension='EXE')
        #IF ((Instring('CONFIG,',upper(%FileUserOptions),1,1) <> 0) or (Instring(',CONFIG',upper(%FileUserOptions),1,1) <> 0) or (upper(%FileUserOptions) = 'CONFIG'))
          #ADD (%ListFile,%File)
        #ENDIF
      #ENDIf
    #ENDFOR
    #FOR (%Procedure)    
      #ADD (%ListFile,%Primary)
      #FOR(%Secondary)
        #ADD (%ListFile,%Secondary)
      #ENDFOR
      #FOR(%OtherFiles)
        #ADD (%ListFile,%OtherFiles)
      #ENDFOR
      #FOR(%ActiveTemplate)
        #FOR(%ActiveTemplateInstance)
          #ADD (%ListFile,%Primary)
          #FOR(%Secondary)
            #ADD (%ListFile,%Secondary)
          #ENDFOR
        #ENDFOR
      #ENDFOR
    #ENDFOR
    #LOOP,Times(10)
      #FOR (%ListFile)
        #FIX(%File,%ListFile)
        #FOR(%Relation)
           #IF((%FileRelationType='1:MANY') and ((%RelationConstraintUpdate <> '') or (%RelationConstraintDelete<> '')))
             #ADD (%ListFile,%Relation)
           #ENDIF
        #ENDFOR
      #ENDFOR
    #ENDLOOP
  #ENDIF
#ENDAT

#AT(%AfterGlobalIncludes),WHERE(~%CLSkelAppDisable)
  INCLUDE('UltimateDebug.INC'),ONCE
  INCLUDE('UltimateDebugProcedureTracker.INC'),ONCE
#ENDAT

#AT(%CustomGlobalDeclarations),WHERE(~%CLSkelAppDisable)
  #INSERT(%CalcFamily, %CLSkelFamily)
  #IF(%CLSkelFamily='LEGACY')
  #PROJECT('UltimateDebug.CLW')
  #ENDIF
#ENDAT

#AT(%GlobalData),WHERE(~%CLSkelAppDisable)
CLSkel_TplVersion    CSTRING('v%CLSkelTPLVersion')%CLSkelDataExternal
 #IF(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=0)  
%CLSkelClass         CLASS(UltimateDebug),EXTERNAL,DLL(dll_mode)
                     END   
 #ELSE
%CLSkelClass         CLASS(UltimateDebug)  
                     END
 #ENDIF   
 !Stuff
 !CLSkelMultiDLLData  %CLSkelMultiDLLData
 !CLSkelMultiDLL      %CLSkelMultiDLL
 !GlobalExternal      %GlobalExternal
 
#!INSERT(%DeclareClass,%CLSkelDataExternal)
#ENDAT

#AT(%DLLExportList),WHERE(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=1 AND ~%CLSkelAppDisable)  
  #IF(NOT (%GlobalExternal))
  $CLSkel_TplVersion  @?
  $%CLSkelClass  @?
  #ENDIF
#ENDAT

#AT(%ProgramSetup),WHERE(~%CLSkelAppDisable),PRIORITY(0001),DESCRIPTION('UltimateDebug - Global Initialization')
#COMMENT(90)
#IF(%ProgramExtension = 'EXE')
  %CLSkelClass.DebugOff       =  %DebugOffVariable
  %CLSkelClass.DebugPrefix    =  '%CLDebugPrefix'
  %CLSkelClass.SaveToFile     =  %SaveToFileVariable
  %CLSkelClass.ASCIIFileName  =  '%CLLogFileName'
  %CLSkelClass.SaveToJson     =  %SaveToJSONFileVariable
  %CLSkelClass.JSONFileName   =  '%CLJSONLogFileName'
  %CLSkelClass.DebugNoCR      =  %DoNotSplitLines
  %CLSkelClass.LineWrap       =  %LineWrap 
#ENDIF
  #IF(%gDumpTpl AND %zDumpTpl)                                                #! Dump GLOBAL information - end
DebugABCGlobalInformation_%Application()      #<! Dump GLOBAL information
  #END                                                                        #! Dump GLOBAL information - begin
  #IF(%gDumpVar AND %zDumpVar)                                                #! Dump GLOBAL data - begin
DebugABCGlobalVariables_%Application()        #<! Dump GLOBAL variables
  #ENDIF                                                                      #! Dump GLOBAL data - end
#COMMENT(60)
#ENDAT
#!-------------------------------------------------------------------------
#! RA.2010.12.10 - Additional procedures to dump basic information.
#! Nice to know where everything is and what global templates are being used.
#!-------------------------------------------------------------------------
#AT (%GlobalMap),PRIORITY(9000)
#!  #IF(~%CLSkelAppDisable)
    #INDENT(-5)

#COMMENT(90)
DebugABCGlobalInformation_%Application PROCEDURE() #<! DEBUG Prototype
DebugABCGlobalVariables_%Application PROCEDURE() #<! DEBUG Prototype
#COMMENT(60)

    #INDENT(+5)
#!  #ENDIF
#ENDAT
#!-------------------------------------------------------------------------
#AT(%ProgramProcedures),WHERE(~%CLSkelAppDisable),PRIORITY(9000)
 
!BOE: DEBUG Global
DebugABCGlobalInformation_%Application PROCEDURE()

#INSERT(%DeclareClass)
                     
  CODE
  
  udpt.Init(%CLSkelClass,'DebugABCGlobalInformation_%Application')
  
 #IF(~%CLSkelAppDisable)
  #IF(%gDumpTpl)
  %CLSkelClass.Debug('----------------> APPLICATION INFORMATION')
  %CLSkelClass.Debug('Information Generated on: '& FORMAT(TODAY(),@D010) & ' - ' & FORMAT(CLOCK(),@T04))
    #IF(%ProgramExtension = 'EXE')
  %CLSkelClass.Debug('CW Version: Lib ' & system{prop:libversion} & ' Exe ' & system{prop:exeversion} & '')
    #ENDIF
  %CLSkelClass.Debug('Application Name: %Application ')
    #IF (%ApplicationDebug = %True)
  %CLSkelClass.Debug('Compiled in DEBUG mode.')
    #ENDIF
    #IF (%ApplicationLocalLibrary = %TRUE)
  %CLSkelClass.Debug('Compiled with LOCAL option.')
    #ENDIF
    #IF (%Target32 = %True)
  %CLSkelClass.Debug('Application is 32 bits.')
    #ELSE
  %CLSkelClass.Debug('Application is 16 bits.')
    #ENDIF
  %CLSkelClass.Debug('First procedure: %FirstProcedure')
  %CLSkelClass.Debug('Program Extension: %ProgramExtension')
  %CLSkelClass.Debug('Dictionary Name: %DictionaryFile')
  %CLSkelClass.Debug('Installation Path: ' & LONGPATH(PATH()))
    #IF(ITEMS(%ApplicationTemplate))
  %CLSkelClass.Debug('----------------> GLOBAL TEMPLATES')
      #FOR(%ApplicationTemplate)
  %CLSkelClass.Debug('Global Templates: %ApplicationTemplate ')
      #ENDFOR
    #ENDIF
  %CLSkelClass.Debug('----------------> ')
  #ENDIF
 #ENDIF    
#!#!  %CLSkelClass.Kill()
 
  RETURN

DebugABCGlobalVariables_%Application PROCEDURE()

#INSERT(%DeclareClass)

  CODE
  
  udpt.Init(%CLSkelClass,'DebugABCGlobalVariables_%Application')
  
 #IF(~%CLSkelAppDisable)
  #IF(%gDumpVar)
    #DECLARE (%Prefix)
    #DECLARE (%VarName)
    #DECLARE (%PrefixStart)
    #DECLARE (%PrefixEnd)
    #DECLARE (%DataStmt)
  %CLSkelClass.Debug('----------------> GLOBAL VARIABLES')
    #FOR(%GlobalData)
      #SET(%DataStmt,QUOTE(%GlobalDataStatement))
      #IF (INSTRING('QUEUE',%GlobalDataStatement,1,1) OR INSTRING('GROUP',%GlobalDataStatement,1,1))
        #SET(%PrefixStart,INSTRING('PRE(',%GlobalDataStatement,1,1)+4)
        #SET(%PrefixEnd  ,INSTRING(')',%GlobalDataStatement,1,%PrefixStart))
        #IF (%PrefixStart)
          #SET(%Prefix,SUB(%GlobalDataStatement, %PrefixStart, %PrefixEnd-%PrefixStart) & ':')
          #IF (LEN(%Prefix) = 1)
            #SET(%Prefix,'')
          #ENDIF
        #ENDIF
  %CLSkelClass.Debug('Only the active record of a group or queue is displayed.')
#!  %CLSkelClass.Debug('Global data: %[23]GlobalData %[17]DataStmt')  ! & ' Records: ' & RECORDS(%GlobalData))
  %CLSkelClass.Debug('Global data: %[23]GlobalData %[17]DataStmt')
      #ELSE
        #IF (INSTRING('END',%GlobalDataStatement,1,1) OR INSTRING('FILE',%GlobalDataStatement,1,1))
          #SET (%Prefix,'')
  %CLSkelClass.Debug('Global Data: %[23]GlobalData %[17]DataStmt')
        #ELSE
	  #! RA.2014.04.19 - No ARRAYS are allowed or supported. 
	  #IF(INSTRING(',DIM',UPPER(%DataStmt))>0)
  %CLSkelClass.Debug('Global Data: %[23]GlobalData is an ARRAY variable and NOT SUPPORTED.')
	  #ELSIF(INSTRING('&',UPPER(%DataStmt))>0) #! RA.2014.04.27 - Reference variable
  %CLSkelClass.Debug('Global Data: %[23]GlobalData is a REFERENCE variable and NOT SUPPORTED.')          
          #ELSE  
  %CLSkelClass.Debug('Global Data: %[23]GlobalData %[17]DataStmt Value: ''' & CLIP(%Prefix%GlobalData) & '''')
          #ENDIF
	  #! RA.2014.04.19 - No ARRAYS are allowed or supported. 
        #ENDIF
      #ENDIF
    #ENDFOR
  %CLSkelClass.Debug('---------------->')
  #ENDIF
 #ENDIF  
#!#!  %CLSkelClass.Kill()
  RETURN
!EOE: DEBUG Global

#ENDAT

#AT(%LocalDataAfterClasses),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#INSERT(%DeclareClass)
#ENDAT
!
#AT(%DataSection),PRIORITY(100),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#INSERT(%DeclareClass)
#ENDAT
!
#AT(%DataSectionBeforeWindow),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#INSERT(%DeclareClass)
#ENDAT
!
#AT(%DeclarationSection),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#INSERT(%DeclareClass)
#ENDAT


#!---------------------------------------------------------------------
#AT(%ProcessedCode),WHERE(~%CLSkelAppDisable),PRIORITY(1) 
#!IF(%CLSkelFamily = 'LEGACY')
    #INSERT(%ShowEnteringTheProcedure)
#!ENDIF
#ENDAT
#!---------------------------------------------------------------------
#AT(%ProcedureSetup),WHERE(~%CLSkelAppDisable),PRIORITY(1)
  #IF(%CLSkelFamily = 'LEGACY' AND (%ProcedureTemplate = 'UnivProcess' OR %ProcedureTemplate ='UnivReport' OR %ProcedureTemplate = 'UnivAbcReport'))
    #INSERT(%ShowEnteringTheProcedure)
  #ENDIF                       
#ENDAT
#!---------------------------------------------------------------------
#AT(%ProcedureInitialize),WHERE(~%CLSkelAppDisable),PRIORITY(0010)
  #IF(%CLSkelFamily = 'LEGACY')
    #INSERT(%ShowEnteringTheProcedure)
  #ENDIF
#ENDAT
#!---------------------------------------------------------------------
#! RA.2014.06.06 - ABC Support
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),WHERE(~%CLSkelAppDisable),PRIORITY(0010)
  #INSERT(%ShowEnteringTheProcedure)
#ENDAT
#!---------------------------------------------------------------------
#AT(%AfterWindowOpening),WHERE(~%CLSkelAppDisable),PRIORITY(0005)
 #IF(~%CLSkelAppDisable AND ~%DisableProcedureDebug)
  #IF (%Window <> '')
%Window{Prop:Alrt,255} = %UDProcAlert
  #ENDIF
 #ENDIF
#ENDAT         
#!---------------------------------------------------------------------
#AT(%AcceptLoopBeforeEventHandling),WHERE(~%CLSkelAppDisable),PRIORITY(6320) 
#IF(~%CLSkelAppDisable AND ~%DisableProcedureDebug)  
 #IF(%CLSkelFamily = 'LEGACY')
  #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
   IF KEYCODE()=%UDProcAlert  
  #INSERT(%ShowTheProcedureInfo)
     CYCLE
   END
  #ENDIF
 #ENDIF
#ENDIF
#ENDAT 
#!---------------------------------------------------------------------
#! RA.2014.06.06 - ABC support
#!---------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeEvent','(),BYTE'),WHERE(~%CLSkelAppDisable),PRIORITY(6320)
#IF(~%CLSkelAppDisable AND ~%DisableProcedureDebug)
 #IF(%CLSkelFamily = 'ABC')
  #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
   IF KEYCODE()=%UDProcAlert AND EVENT() = Event:PreAlertKey
     CYCLE
   END
   IF KEYCODE()=%UDProcAlert  
  #INSERT(%ShowTheProcedureInfo)
     CYCLE
   END
  #ENDIF
 #ENDIF
#ENDIF
#ENDAT
#!---------------------------------------------------------------------
#!#AT(%ProcessedCode),PRIORITY(9990) 
#!AT(%ProcessedCode),PRIORITY(9990)
#!IF(%CLSkelFamily = 'LEGACY')
  #!INSERT(%ShowLeavingTheProcedure)
 #!ENDIF
 #!ENDAT
#!
#AT(%EndOfProcedure),WHERE(~%CLSkelAppDisable),PRIORITY(9990)
#IF(%CLSkelFamily = 'LEGACY')
  #INSERT(%ShowLeavingTheProcedure)
#!
 #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
 #ELSE
IF BAND(Keystate(),%UDReportKeyState) 
  #INSERT(%ShowTheProcedureInfo)
END
 #ENDIF
#ENDIF
#ENDAT
#!---------------------------------------------------------------------
#! RA.2014.06.06 - ABC Support
#AT(%WindowManagerMethodCodeSection,'Kill','(),BYTE'),WHERE(~%CLSkelAppDisable),PRIORITY(9999)
 #INSERT(%ShowLeavingTheProcedure)
#!
 #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
 #ELSE
IF BAND(Keystate(),%UDReportKeyState) 
  #INSERT(%ShowTheProcedureInfo)
END
 #ENDIF
#ENDAT 
#AT(%ProcessedCode),PRIORITY(8000) 
  #INSERT(%ShowLeavingTheProcedure)
#ENDAT
#!-------------------------------------------------------------------------
#INCLUDE('UltimateDebug.TPW')
#INCLUDE('UltimateDebug2.TPW') #! RA.2014.03.28 - Global Debug ALL procedures
#INCLUDE('UltimateDebug3.TPW') #! RA.2014.04.12 - Tracing Source (*.CLW) files
#INCLUDE('UltimateDebug4.TPW') #! RA.2014.06.04 - John Hickey's procedure track
#!------------------------------------------------------------------------- 
#!------------------------------------------------------------------------- 
#!*****************************************************************************
#GROUP(%ProcStillNeedsUltDB),AUTO  
#IF(~%CLSkelAppDisable)
  #EQUATE(%CustomFlag, 'UltDB|'& %Procedure)
  #IF(~INLIST(%CustomFlag, %CustomFlags))
    #ADD(%CustomFlags, %CustomFlag)
    #RETURN(%True)
  #ENDIF
#ENDIF  
  #RETURN(%False)
#!*****************************************************************************
#GROUP(%DeclareClass,%ExternalAttr='')  
#IF(~%CLSkelAppDisable)
udpt            UltimateDebugProcedureTracker
#ENDIF
#!*****************************************************************************
