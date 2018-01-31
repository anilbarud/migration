$PBExportHeader$uo_mixcase.sru
$PBExportComments$Will convert a line that was passed from any case to mixedcase
forward
global type uo_mixcase from u_dw_online
end type
end forward

global type uo_mixcase from u_dw_online
int Width=494
int Height=361
int TabOrder=1
string DataObject="d_mixcase"
boolean LiveScroll=true
end type
global uo_mixcase uo_mixcase

forward prototypes
public function string wf_convert_line (string line_value)
end prototypes

public function string wf_convert_line (string line_value);String	vls_rv

this.SetItem(1,"line",line_value)
vls_rv = this.GetItemString(1,"d_line")

Return vls_rv
end function

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

