F9::						; AUTOMATIZAÇÃO DE ENTRADA DE EXTRATOS CONTÁBEIS MENSAIS
StartTime := A_TickCount
CoordMode, Mouse, Window
IfWinExist, Gestão Empresarial
	{
		InputBox, LoteInput, Nº Lote, Entre com o Nº do lote:,,200,100
		if ErrorLevel 
			{
				MsgBox, Operação cancelada.
				return
			}
		WinActivate
		WinWait, Gestão Empresarial
		Send, {BACKSPACE}
		MouseClick, left, 166,94
		Send, %LoteInput%
		Send,{TAB 3}{ENTER}
		Sleep 500
		Send, {DOWN}
	}
Else 
	{
		MsgBox, Error, Senior not running.
		return
	}
; InputBox, DataInput, Data, Por favor entrar com o mês e ano,,200,100
; if ErrorLevel 
; 	{
; 		MsgBox, Operação cancelada.
; 		return
; 	}	
; else if (StrLen(DataInput)<7)
; 		{
; 			MsgBox, Data Inválida.
; 			return
; 		}
; else
; 	MsgBox, Mês e ano definido como: "%DataInput%"
Sleep, 2000

CoordMode, Mouse, Window
SetTitleMatchMode, 2

IfWinNotExist, Notepad++
	Run C:\Program Files\Notepad++\notepad++.exe
WinWait, Notepad++
IfWinExist, Notepad++
	{
		WinActivate
		WinMaximize
	}
IfWinNotActive, DADOS.txt
	{
		Send, {CONTROL DOWN}{o}{CONTROL UP}
		WinWait, Abrir
		Send, C:\Users\Processos\Desktop\DADOS.txt
		MouseClick, Left, 508, 446
	}
MsgBox,4,Warning, Os dados estão corretos?
IfMsgBox No
	Return
Sleep, 30
Send, {CONTROL DOWN}{HOME}{CONTROL UP}
Send, {HOME}{SHIFT DOWN}{END}{SHIFT UP}{DEL}{DEL}				
MouseClick, Left, 86, 41
MouseMove, 230, 317
Sleep, 750
MouseMove, 505,0,,Relative
MouseClick, Left, 705, 473
Send, {CONTROL DOWN}{s}{CONTROL UP}
Loop, Read, C:\Users\Processos\Desktop\DADOS.txt
	{
		if (BreakLoop = 1)
			{
				break 
			}
		total_lines = %A_Index%
	}
Send, {CONTROL DOWN}{HOME}{CONTROL UP}
Send, {UP}
Loop, %total_lines%
	{
		if (BreakLoop = 1)
			{
				break 
			}
		Send, {CONTROL DOWN}{F9}{CONTROL UP}
	}


Send, {CONTROL DOWN}{h}{CONTROL UP}
WinWait, Substituir
Send, "
Send, {TAB}{BACKSPACE}
MouseClick, Left, 476, 134
Sleep, 100
Send, {ESC}
Send, {CONTROL DOWN}{HOME}{CONTROL UP}
Array := []
ArrayCount := 0
SetTitleMatchMode, 2
SetKeyDelay 1,0

Loop % total_lines * 5
	{
		if (BreakLoop = 1)
			{
				break 
			}
		Sleep, 15			
		if WinExist("DADOS.txt - Notepad++")
			{
				ArrayCount += 1
				WinActivate  ; Automatically uses the window found above.
				Send, {CONTROL DOWN}{Home}{CONTROL UP}
				Sleep, 10
				Send, {SHIFT DOWN}{END}{SHIFT UP}
				Sleep, 10
				Send, {CONTROL DOWN}{x}{CONTROL UP}
				Send, {Delete}
				Array[A_Index] := clipboard
			}
	}
counter := 0
Sleep, 200
if WinExist("Senior |")
	{
		SetKeyDelay 20,15
		WinActivate  ; Automatically uses the window found above.
		Sleep, 300
		WinWaitActive
		Loop, % total_lines
			{
				if (BreakLoop = 1)
					{
						break 
					}
				counter := % counter + 1
				Sleep, 7
				Send, {Enter}
				Sleep, 50
				Send % Value := Array[counter]		;INSERÇÃO DA CONTA DEBITADA
				Sleep, 50
				Send, {Enter}
				counter := % counter + 1
				Send % Value := Array[counter]		;INSERÇÃO DA CONTA CREDITADA
				Sleep, 50
				Send, {Enter}
				counter := % counter + 1
				Send % Value := Array[counter]   ;INSERÇÃO DO VALOR
				Sleep, 50
				Send, {Enter}
				counter := % counter + 1
				Send % Value := Array[counter]			;INSERÇÃO DA DATA
				Sleep, 50
				Send, {Enter}
				Sleep, 50
				counter := % counter + 1
				Send, {Enter}
				Sleep, 50
				SetKeyDelay 0,0
				Send % Value := Array[counter]		;INSERÇÃO  DO MOTIVO
				SetKeyDelay 20,15
				Sleep, 50
				Send,  {Enter}
				Sleep, 50
				Send,  {Enter}
				Sleep, 50
			}
	}

;
ElapsedTime := A_TickCount - StartTime
MsgBox,  %ElapsedTime% milliseconds have elapsed.
Return

#^R::
MsgBox, 4,Script, Reload?
IfMsgBox, No
	{
		MsgBox, Operation Cancelled.
	}
IfMsgBox, Yes
	{	
		FileDelete, C:\Users\Processos\Desktop\DADOS.csv
		MsgBox, Script Reloaded.
		Reload
		Return
	}
	
F10::
BreakLoop = 1                  ; PARA SAIR DO LOOP, MODIFICAR PARA A TECLA DE PREFERENCIA
return
