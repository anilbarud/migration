$PBExportHeader$w_startup.srw
forward
global type w_startup from window
end type
type cb_cleanup_viewer_files from commandbutton within w_startup
end type
type lb_delete_temp_files from listbox within w_startup
end type
type st_2 from statictext within w_startup
end type
type st_1 from statictext within w_startup
end type
type p_handy from picture within w_startup
end type
end forward

global type w_startup from window
integer x = 1691
integer y = 888
integer width = 1285
integer height = 1268
boolean titlebar = true
string title = " One moment please ..."
windowtype windowtype = popup!
cb_cleanup_viewer_files cb_cleanup_viewer_files
lb_delete_temp_files lb_delete_temp_files
st_2 st_2
st_1 st_1
p_handy p_handy
end type
global w_startup w_startup

event open;
SetPointer (HourGlass!)


end event

on w_startup.create
this.cb_cleanup_viewer_files=create cb_cleanup_viewer_files
this.lb_delete_temp_files=create lb_delete_temp_files
this.st_2=create st_2
this.st_1=create st_1
this.p_handy=create p_handy
this.Control[]={this.cb_cleanup_viewer_files,&
this.lb_delete_temp_files,&
this.st_2,&
this.st_1,&
this.p_handy}
end on

on w_startup.destroy
destroy(this.cb_cleanup_viewer_files)
destroy(this.lb_delete_temp_files)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.p_handy)
end on

type cb_cleanup_viewer_files from commandbutton within w_startup
boolean visible = false
integer x = 987
integer y = 188
integer width = 247
integer height = 108
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = " "
end type

event clicked;long ll_total_items,ll_window_handle,ll_rtn, ll_loop
STRING ls_new_temp_tiff_directory, ls_file_to_delete, ls_current_path
boolean lb_success
string ls_null

setnull(ls_null)

gs_view_temp_directory = ProfileString(vgs_ini_filename,"WANG VIEWER","TempTif",             " ")

if gs_view_temp_directory = "" then
	MessageBox("System Error","Could not determine Viewer Run Time Application Terminating. Call Help Desk.",StopSign!)
	Return -1 
End If

/* IF WORKBENCH IS NOT RUNNING REMOVE ALL TIF FILES */

ll_window_handle = FindWindowA(ls_null,"Case Management Work Bench")

if ll_window_handle = 0 then
		
	ls_new_temp_tiff_directory = MID(gs_view_temp_directory,1,(len(gs_view_temp_directory) - 1) )
	
	ll_rtn = CanAccess(ls_new_temp_tiff_directory, 00)
	IF ll_rtn <> 0 THEN
		ll_rtn = CreateDirectory(ls_new_temp_tiff_directory)
		IF ll_rtn <> 0 THEN
			MessageBox("ERROR","Problem creating Tif directory " + ls_new_temp_tiff_directory  ,StopSign!)
			return -1
		END IF
	END IF

	ls_current_path = GetCurrentDirectory( )
	
	lb_delete_temp_files.dirlist(gs_view_temp_directory + '*.tif',0)

	ChangeDirectory(ls_current_path)

	ll_total_items = lb_delete_temp_files.totalitems()

	FOR ll_loop = 1 TO ll_total_items
		
	  		ls_file_to_delete = lb_delete_temp_files.text(ll_loop)
	 		if fileexists(gs_view_temp_directory + ls_file_to_delete ) then
				/* WILL NOT DELETE IF FILE IS IN USE */
				lb_success = filedelete(gs_view_temp_directory + ls_file_to_delete)

			end if
	NEXT
end if

RETURN 0 



end event

type lb_delete_temp_files from listbox within w_startup
boolean visible = false
integer x = 992
integer y = 420
integer width = 494
integer height = 360
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_startup
integer y = 828
integer width = 1262
integer height = 220
integer textsize = -14
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Matura MT Script Capitals"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "Case Management Workbench"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_1 from statictext within w_startup
integer y = 732
integer width = 1262
integer height = 120
integer textsize = -14
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Matura MT Script Capitals"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "Welcome to the "
alignment alignment = center!
boolean focusrectangle = false
end type

type p_handy from picture within w_startup
integer x = 283
integer y = 48
integer width = 622
integer height = 696
string picturename = "handy.bmp"
boolean focusrectangle = false
end type

