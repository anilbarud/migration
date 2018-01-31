$PBExportHeader$u_image_append.sru
forward
global type u_image_append from userobject
end type
type ole_image_print from olecustomcontrol within u_image_append
end type
type dw_filename from u_dw_online within u_image_append
end type
type ole_image from olecustomcontrol within u_image_append
end type
end forward

global type u_image_append from userobject
integer width = 1335
integer height = 388
boolean border = true
long backcolor = 67108864
long tabtextcolor = 33554432
long tabbackcolor = 268435456
long picturemaskcolor = 536870912
ole_image_print ole_image_print
dw_filename dw_filename
ole_image ole_image
end type
global u_image_append u_image_append

type variables
long al_claim_no
string is_option
string is_document_type_desc
string is_document_type
Long il_num_rows
Datastore   ids_document_type

LONG  il_reference_no
STRING is_doc_id
end variables

forward prototypes
public function integer of_init (long al_doc_id)
public function string of_get_file_type ()
public subroutine of_set_option ()
public function integer of_append_image (long al_doc_id, string as_hst_flag)
public function integer of_run_viewer (string as_new_document_path, string as_hst_flag)
public function integer of_run_viewer (string as_new_document_path)
public function integer of_imatotiff (ref string as_file_name, string as_org_file_name)
public function integer of_copy_tiff (ref string as_file_name, string as_org_file_name)
public function integer of_append_image (long al_doc_id)
public function integer of_print_image (string as_document_path)
end prototypes

public function integer of_init (long al_doc_id);

il_num_rows = dw_filename.retrieve(al_doc_id)

IF ImageTrans.nf_handle_error('w_sheet', 'dw_filename', 'on Retrieve') < 0 THEN
	MessageBox("Document Path Error", "Could Not Determine the Document's Path.", StopSign!)
	Return -1
END IF


return il_num_rows
end function

public function string of_get_file_type ();
return Right(dw_filename.GetItemString(1,"filename"),3)
end function

public subroutine of_set_option ();is_option = "INDEX"
end subroutine

public function integer of_append_image (long al_doc_id, string as_hst_flag);long ll_row,ll_pos,ll_position,ll_rc
string ls_document_path, ls_doc_type, ls_doc_name
integer li_rtn
string ls_string,ls_new_document_path



ids_document_type = create datastore

ids_document_type.Dataobject = 'd_document_type'

ids_document_type.settransobject(ImageTrans)

ll_rc = ids_document_type.retrieve(al_doc_id)

if ll_rc > 0 then	
	is_document_type_desc = ids_document_type.getitemstring(1,"document_type_code_type_desc")
	is_document_type = ids_document_type.getitemstring(1,"document_type_code_type_code")
end if



/* reset random # generator */
randomize(0)
	


for ll_row = 1 to il_num_rows

	ls_document_path = dw_filename.GetItemString(ll_row,"filename")

	// Get the document type (EXCEL, WORD, IMAGE).
	ls_doc_type = Right(ls_document_path,3)

	
	/* strip off the path to get just the file name */
	ll_pos = Pos( ls_document_path, '\', 1 )
			
	do while ll_pos <> 0 
		
		ll_pos = Pos( ls_document_path, '\', ll_pos + 1 )
		IF ll_pos <> 0 then
			ll_position = ll_pos
		END IF
	
		IF isnull(ll_pos) then
		  ll_pos = 0
		END IF
					
	loop			
						
	IF ll_position > 0 then
		ls_doc_name = mid(ls_document_path,ll_position + 1)
	END IF
		
		
	if ls_doc_type = 'IMA' then			
		li_rtn = of_imatotiff(ls_doc_name ,ls_document_path )
	else
		li_rtn = of_copy_tiff(ls_doc_name ,ls_document_path )		
	end if			
	
	if li_rtn = -1 then
		return -1
	end if
	
	
	if ll_row = 1 then		
		ls_new_document_path = gs_view_temp_directory + ls_doc_name 
		ole_image.object.image = gs_view_temp_directory + ls_doc_name	
	else
		ole_image.object.Append(gs_view_temp_directory + ls_doc_name ,1,1)
		filedelete(gs_view_temp_directory + ls_doc_name)
	end if
next
	

/* get claim # to pass to viewer */
al_claim_no = dw_filename.getitemnumber(1,'document_index_claim_no')

		
		
if of_run_viewer(ls_new_document_path, as_hst_flag)		< 0 then
	return -1
end if
	

	

	
			
return il_num_rows
end function

public function integer of_run_viewer (string as_new_document_path, string as_hst_flag);string ls_command

if isnull(is_option) or is_option = "" then
	is_option = "NORMAL"
END IF

if isnull(al_claim_no) then
	al_claim_no = 0
end if

ls_command = gs_view_runtime_directory  + " " + is_option + ';' + as_new_document_path + ";" + string(al_claim_no) + ';' + &
is_document_type + ';' + is_document_type_desc + ";" + as_hst_flag + ";"
	
return Run(ls_command, Normal!)	
		
end function

public function integer of_run_viewer (string as_new_document_path);string ls_command,ls_set_settings,ls_save_registry

if isnull(is_option) or is_option = "" then is_option = "NORMAL"

if isnull(al_claim_no) then al_claim_no = 0

/* grab the user id and check the job position code if it is the appropriate type
   then set the global flag to Y
*/
SELECT registry_viewer_setting_flag INTO :ls_set_settings
FROM User_Profile
WHERE user_id = :vgst_user_profile.user_id
USING SQLCA;

sqlca.nf_handle_error("u_image_append","of_run_viewer","SELECT job_position_code")

IF ls_set_settings <> "Y" THEN 
	ls_save_registry = "FALSE"
ELSE
	ls_save_registry = "TRUE"
END IF

/* with the new login.pbl we capture the application name it is a property 
   of gnv_sqlserver we can use it here to send to the viewer as an argument
	of the run command
*/
ls_command = gs_view_runtime_directory  + " " + is_option + ';' + as_new_document_path + ";" + string(al_claim_no) + ';' + &
is_document_type + ';' + is_document_type_desc + ";" + "N" + ";" + string(il_reference_no) + ";" + is_doc_id + ";" + ls_save_registry + ";" + gs_appname + ";"

return Run(ls_command, Normal!)	
		
end function

public function integer of_imatotiff (ref string as_file_name, string as_org_file_name);long ll_length, ll_pos,ll_position
integer li_rtn

/* change to "tif" */
ll_length = len(as_file_name)
			
as_file_name = mid(as_file_name,1,ll_length - 3) + 'TIF'
	
long ll_num		
ll_num = rand(1000)			
		
as_file_name = Mid ( as_file_name, 1 , len(as_file_name) - 4 ) + string(ll_num) + '.tif'

IF fileexists(gs_view_temp_directory + as_file_name ) then
	/* close window */
	Messagebox("Error","You already are viewing this file - If you're not please call help desk",StopSign!) 
	RETURN -1
end if

/* convert to tiff */
li_rtn = IMAtoTIFF(as_org_file_name,gs_view_temp_directory + as_file_name)
								
IF li_rtn <> 0 then
	Messagebox('Error', 'Call Help Desk - There is an error converting the ima to tIF '  & 
	+ 'file name ' +  as_org_file_name+ ' to ' + gs_view_temp_directory + as_file_name) 
	RETURN -1
END IF

return 0
end function

public function integer of_copy_tiff (ref string as_file_name, string as_org_file_name);
integer li_rtn	
long ll_num		
ll_num = rand(10000)			
		
as_file_name = Mid ( as_file_name, 1 , len(as_file_name) - 4 ) + string(ll_num) + '.tif'

IF fileexists(gs_view_temp_directory + as_file_name ) then
	/* close window */
	Messagebox("You already are viewing this file","Application is closing") 
	RETURN -1
end if	
	
li_rtn = f_copyfile(as_org_file_name, gs_view_temp_directory + as_file_name)

IF li_rtn = -1 THEN
	messagebox("ERROR", "CALL HELP DESK - An error occurred while trying to copy file FROM " &
	+ as_org_file_name + ' TO ' + gs_view_temp_directory +  as_file_name)
	return  -1
END IF

return 0


end function

public function integer of_append_image (long al_doc_id);long ll_row,ll_pos,ll_position,ll_rc
string ls_document_path, ls_doc_type, ls_doc_name
integer li_rtn
string ls_string,ls_new_document_path


ids_document_type = create datastore

ids_document_type.Dataobject = 'd_document_type'

ids_document_type.settransobject(ImageTrans)

ll_rc = ids_document_type.retrieve(al_doc_id)

if ll_rc > 0 then	
	is_document_type_desc = ids_document_type.getitemstring(1,"document_type_code_type_desc")
	is_document_type = ids_document_type.getitemstring(1,"document_type_code_type_code")
end if

/* there may not be a record so set the reference number to 0 to start. */
il_reference_no = 0

/* need to grab the batch_id from the Batch_Docid_Xref table */
SELECT reference_no INTO :il_reference_no FROM Docid_Reference_Xref
WHERE docid = :al_doc_id
USING ImageTrans;

ImageTrans.nf_handle_error("u_image_append","of_append_image","SELECT reference_no")

/* if a reference number does not appear here select it from DOCUMENT_INDEX */
IF ISNULL(il_reference_no) OR il_reference_no = 0 THEN 
	
	SELECT reference_no INTO :il_reference_no FROM DOCUMENT_INDEX
	WHERE docid = :al_doc_id
	USING ImageTrans;
	
	ImageTrans.nf_handle_error("u_image_append","of_append_image","SELECT reference_no")
END IF

IF NOT isnull(al_doc_id) THEN is_doc_id = STRING(al_doc_id)
IF ISNULL(il_reference_no) THEN il_reference_no = 0

/* reset random # generator */
randomize(0)
	
for ll_row = 1 to il_num_rows

	ls_document_path = dw_filename.GetItemString(ll_row,"filename")

	// Get the document type (EXCEL, WORD, IMAGE).
	ls_doc_type = Right(ls_document_path,3)

	
	/* strip off the path to get just the file name */
	ll_pos = Pos( ls_document_path, '\', 1 )
			
	do while ll_pos <> 0 
		
		ll_pos = Pos( ls_document_path, '\', ll_pos + 1 )
		IF ll_pos <> 0 then
			ll_position = ll_pos
		END IF
	
		IF isnull(ll_pos) then
		  ll_pos = 0
		END IF
					
	loop			
						
	IF ll_position > 0 then
		ls_doc_name = mid(ls_document_path,ll_position + 1)
	END IF
		
		
	if ls_doc_type = 'IMA' then			
		li_rtn = of_imatotiff(ls_doc_name ,ls_document_path )
	else
		li_rtn = of_copy_tiff(ls_doc_name ,ls_document_path )		
	end if			
	
	if li_rtn = -1 then
		return -1
	end if
	

	if ll_row = 1 then		
		ls_new_document_path = gs_view_temp_directory + ls_doc_name 
		ole_image.object.image = gs_view_temp_directory + ls_doc_name	
	else
		ole_image.object.Append(gs_view_temp_directory + ls_doc_name ,1,1)
		filedelete(gs_view_temp_directory + ls_doc_name)
	end if
next
	
/* get claim # to pass to viewer */
al_claim_no = dw_filename.getitemnumber(1,'document_index_claim_no')
		
if of_run_viewer(ls_new_document_path)		< 0 then
	return -1
end if
			
return il_num_rows
end function

public function integer of_print_image (string as_document_path);
int li_page_count, li_rtn


ole_image_print.object.image = as_document_path				

li_page_count = ole_image_print.object.PageCount()
If  li_page_count > 0 then
	//there is no return value for this image edit function
	ole_image_print.object.PrintImage(1, li_page_count, 2)
ELSE
	// a 1 means something wrong, couldn't get a page count
	li_rtn = 1
END IF

return li_rtn
end function

on u_image_append.create
this.ole_image_print=create ole_image_print
this.dw_filename=create dw_filename
this.ole_image=create ole_image
this.Control[]={this.ole_image_print,&
this.dw_filename,&
this.ole_image}
end on

on u_image_append.destroy
destroy(this.ole_image_print)
destroy(this.dw_filename)
destroy(this.ole_image)
end on

type ole_image_print from olecustomcontrol within u_image_append
event keydown ( integer keycode,  integer shift )
event keyup ( integer keycode,  integer shift )
event keypress ( integer keyascii )
event mousedown ( integer button,  integer shift,  long ocx_x,  long ocx_y )
event mousemove ( integer button,  integer shift,  long ocx_x,  long ocx_y )
event mouseup ( integer button,  integer shift,  long ocx_x,  long ocx_y )
event click ( )
event dblclick ( )
event ocx_error ( integer number,  string description,  long scode,  string source,  string helpfile,  long helpcontext,  boolean canceldisplay )
event ocx_close ( )
event markend ( long left,  long top,  long ocx_width,  long ocx_height,  integer marktype,  string groupname )
event toolselected ( integer toolid )
event selectionrectdrawn ( long left,  long top,  long ocx_width,  long ocx_height )
event tooltip ( integer index )
event toolpalettehidden ( long left,  long top )
event scroll ( )
event markselect ( integer button,  integer shift,  long left,  long top,  long ocx_width,  long ocx_height,  integer marktype,  string groupname )
event pastecompleted ( )
event load ( double zoom )
event markmove ( integer marktype,  string groupname )
event pagepropertiesclose ( )
event checkcontinueprinting ( long pagesprinted,  long currentpage,  integer status )
event hyperlinkgotopage ( long page )
event errorsavingundoinformation ( long ocx_error )
event straightenpage ( )
event hyperlinkgotodoc ( string link,  long page,  boolean handled )
event editingtextannotation ( boolean editing )
event magnifierstatus ( integer hwnd,  long status,  long magnifierzoom,  long left,  long top,  long ocx_width,  long ocx_height )
event pasteclip ( long mode )
event baddocumentfiletype ( long page,  boolean errorout,  boolean skippage,  boolean overwritepage )
event nextpage ( )
event prevpage ( )
event overlayfilenotfound ( string overlayfilename,  boolean btryagain )
event imagedraganddrop ( )
event zoomchanged ( real zoom )
event beforemarkmove ( boolean bcancelmove )
event readystatechange ( long readystate )
integer x = 23
integer y = 204
integer width = 475
integer height = 160
integer taborder = 30
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "u_image_append.udo"
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type dw_filename from u_dw_online within u_image_append
integer x = 210
integer y = 16
integer width = 1033
integer height = 160
integer taborder = 20
string dataobject = "d_filename"
end type

event constructor;this.settransobject(ImageTrans)
end event

type ole_image from olecustomcontrol within u_image_append
integer x = 9
integer y = 8
integer width = 155
integer height = 136
integer taborder = 10
long backcolor = 268435456
string binarykey = "u_image_append.udo"
integer binaryindex = 1
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
05u_image_append.bin 
2600000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffefffffffe00000004fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff000000010000000000000000000000000000000000000000000000000000000051febb7001ce797b00000003000003800000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000029900000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff000000036d94028011ce9f116002fd838ac03e8c0000000051febb7001ce797b51febb7001ce797b000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000b00000086000000000000000100000002000000030000000400000005000000060000000700000008000000090000000afffffffe0000000c0000000dfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000fffe000201056d94028011ce9f116002fd838ac03e8c00000001fb8f0821101b01640008ed8413c72e2b00000030000002690000001000000100000000880000010100000090000001020000009800000103000000a000000104000000a800000105000000b000000106000000c400000107000000cc00000108000000d400000109000000dc0000010a000000e40000010b000000ec0000010c000000f40000010d000000fc0000010e00000104000000000000010c00000003000200090000000300000ac00000000300000422000000030000006000000002000000010000001e0000000945676d4931746964000000000000000b000000000000000b0000ffff000000020000000400000002000000030000000b00000000000000031dc6c2ff0000000b0000ffff000000035f4d1b58000000035f4d1b5800000010000000000000000100010d0000000b0065726c0076726573003164650000010e0000000b7365726c65767265090032640d000001690000006567616d656c617000657474000001060000001a656c65736f6974636365726e676e61746e65656c656c626101070064000c00007561000065726f747365726601030068000c0000735f00006b636f74706f727001040073000c00006f620000726564726c797473010a006500170000637300006c6c6f72726f687374756374616e657364656c6200010800000016007369640079616c706c616373676c61657469726f05006d680d000001690000006567616d746e6f63006c6f7200000101000000097478655f78746e65000102000000090078655f00746e6574010c0079001300006f660000666563726c656c69696b6e697831676e00010b0000000f00646e75006675626f7372656600657a6900000100000000097265765f6e6f697300720000005300200072006300700069000000740069005700650064004c002000740061006e00690002000900000ac0000004220000006000010001000000006d490800696445670000317400000100c8000000020000420000ff0000ffff00ff000000ff00000000000000000100000300040100000100000100ff03000000ff000000001dc6c2000000000000010000000000384f0000384f5f4000005f40000000000000000000000001006f0100006b006300650077006c006c00430020006e006f006500640073006e00640065005200000063006f0077006b006c00650000006c006100520065006700490020006100740069006c000000630065005000700072007400650061007500540020007400690069006c0067006e004d0020000000540065005000700072007400650061007500500000006c00610063006100200065006300530069007200740070004d002000000054095452680956f688000000000000000000000a00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
24ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff000000010000000000000000000000000000000000000000000000000000000051febb7001ce797b00000003000001c00000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000102001affffffff00000002ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000012000000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000001001affffffffffffffff00000003009541a0101c3b810204f392029c00240000000051febb7001ce797b51febb7001ce797b000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000050000007a0000000000000001000000020000000300000004fffffffe00000006fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000fffe00020105009541a0101c3b810204f392029c002400000001fb8f0821101b01640008ed8413c72e2b00000030000000f00000000700000100000000400000010100000048000001020000005000000103000000580000010400000060000001050000006800000000000000700000000300030002000000030000038400000003000003840000000300000000000000030000000000000003000000000000000700000000000000010001050000000d00697270006e65746e6761706401030065000c0000735f00006b636f74706f727001040073000f00007270000073746e697472617465676170000101000000090078655f00746e65740102007800090000655f00006e65747800007974090000015f00000073726576006e6f691dc6c2ff0000000b0000ffff000000035f4d1b58000000035f4d1b580000001000030002000003840000038400000000000001000000000000000000000000000101000000000000000000000000000001000300000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000100000000020000030000000000030000010000000001030001000c0000735f00006b636f74706f727001040073000c00006f620000726564726c79747309500065098a73e80000000000000000154fbcf400100000006a00000103ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
15u_image_append.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
