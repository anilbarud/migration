$PBExportHeader$w_mdiwindow.srw
forward
global type w_mdiwindow from window
end type
type mdi_1 from mdiclient within w_mdiwindow
end type
end forward

global type w_mdiwindow from window
integer width = 4754
integer height = 2276
boolean titlebar = true
string title = "MDI Frame"
string menuname = "m_mdimenu"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = mdidockhelp!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
mdi_1 mdi_1
end type
global w_mdiwindow w_mdiwindow

on w_mdiwindow.create
if this.MenuName = "m_mdimenu" then this.MenuID = create m_mdimenu
this.mdi_1=create mdi_1
this.Control[]={this.mdi_1}
end on

on w_mdiwindow.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.mdi_1)
end on

event open;w_sheet lw_sheet

OpenSheetAsDocument(lw_sheet,"w_sheet",this,"w_sheet_1")
end event

type mdi_1 from mdiclient within w_mdiwindow
long BackColor=268435456
end type

