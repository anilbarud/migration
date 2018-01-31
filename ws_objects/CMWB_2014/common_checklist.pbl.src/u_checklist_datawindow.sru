$PBExportHeader$u_checklist_datawindow.sru
forward
global type u_checklist_datawindow from u_datawindow
end type
end forward

global type u_checklist_datawindow from u_datawindow
end type
global u_checklist_datawindow u_checklist_datawindow

forward prototypes
public function string uf_getitemstring (ref u_datawindow adw_datawindow, long al_row, string as_column)
public function long uf_getitemnumber (ref u_datawindow adw_datawindow, long al_row, string as_column)
end prototypes

public function string uf_getitemstring (ref u_datawindow adw_datawindow, long al_row, string as_column);STRING		ls_return

ls_return = adw_datawindow.GetItemString(al_row,as_column)

RETURN ls_return
end function

public function long uf_getitemnumber (ref u_datawindow adw_datawindow, long al_row, string as_column);LONG		ll_return

ll_return = adw_datawindow.GetItemNumber(al_row,as_column)

RETURN ll_return
end function

on u_checklist_datawindow.create
call super::create
end on

on u_checklist_datawindow.destroy
call super::destroy
end on

event itemchanged;call super::itemchanged;//dwitemstatus ldwis
//
//ldwis = this.getitemstatus( row, 0, Primary!)
//
//choose case ldwis
//	case NotModified!
//		messagebox('u_checklist_datawindow','NotModified!')
//		
//	case DataModified!
//		messagebox('u_checklist_datawindow','DataModified!')
//		
//	case NewModified!
//		messagebox('u_checklist_datawindow','NewModified!')
//		
//	case New!
//		messagebox('u_checklist_datawindow','New!')
//end choose
//
//
//this.SetItemStatus(row,0,Primary!,DataModified!)
//
//choose case ldwis
//	case NotModified!
//		messagebox('u_checklist_datawindow','NotModified!')
//		
//	case DataModified!
//		messagebox('u_checklist_datawindow','DataModified!')
//		
//	case NewModified!
//		messagebox('u_checklist_datawindow','NewModified!')
//		
//	case New!
//		messagebox('u_checklist_datawindow','New!')
//end choose
//
//
//messagebox('u_checklist_datawindow',AncestorReturnValue)
end event

