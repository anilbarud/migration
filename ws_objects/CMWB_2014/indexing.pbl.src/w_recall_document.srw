$PBExportHeader$w_recall_document.srw
forward
global type w_recall_document from window
end type
type uo_image_append from u_image_append within w_recall_document
end type
type cb_view from commandbutton within w_recall_document
end type
type cb_ok from commandbutton within w_recall_document
end type
type dw_carry_forward from u_dw_online within w_recall_document
end type
end forward

global type w_recall_document from window
integer width = 2021
integer height = 1300
boolean titlebar = true
string title = "Recalled Document"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
uo_image_append uo_image_append
cb_view cb_view
cb_ok cb_ok
dw_carry_forward dw_carry_forward
end type
global w_recall_document w_recall_document

type variables
LONG	il_docid

end variables

on w_recall_document.create
this.uo_image_append=create uo_image_append
this.cb_view=create cb_view
this.cb_ok=create cb_ok
this.dw_carry_forward=create dw_carry_forward
this.Control[]={this.uo_image_append,&
this.cb_view,&
this.cb_ok,&
this.dw_carry_forward}
end on

on w_recall_document.destroy
destroy(this.uo_image_append)
destroy(this.cb_view)
destroy(this.cb_ok)
destroy(this.dw_carry_forward)
end on

event open;/* grab the docid from the last indexed document from this user */

LONG				  ll_return
S_WINDOW_MESSAGE lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*
Information provided in the indexing section

SELECT  DOCUMENT_INDEX.docid, DOCUMENT_INDEX.claim_no ,
        DOCUMENT_INDEX.imaged_document_flag, DOCUMENT_INDEX.type_code ,
        DOCUMENT_INDEX.date_on_document, DOCUMENT_INDEX.comment ,
        DOCUMENT_INDEX.source_code, DOCUMENT_INDEX.sent_flag ,
        DOCUMENT_INDEX.service_provider_no, DOCUMENT_INDEX.english_flag ,
        DOCUMENT_INDEX.date_received, DOCUMENT_INDEX.reference_no ,
        message_info = '                               ',
        service_provider_name = '                                        ',
        document_desc = '                                             ',
        lost_time='N', DOCUMENT_INDEX.service_provider_type_code ,
        fldid = 0, paid_flag='N'    
   FROM DOCUMENT_INDEX      
  WHERE ( DOCUMENT_INDEX.docid = :al_docid )   
*/

/* grab the last DOCUMENT_INDEX.doc_id created by this user */
SELECT docid INTO :il_docid FROM USER_LAST_INDEXED_DOC
 WHERE create_user_id = :vgst_user_profile.user_id
 USING ImageTrans;

SQLCA.nf_handle_error("w_carry_forward","open event","SELECT docid INTO....") 

/* make sure we have a valid value */
IF ISNULL(il_docid) OR il_docid < 1 THEN 
	messagebox("No Rows Returned","The Application could not determine the last document that you indexed")
	cb_ok.postevent(clicked!)
	RETURN
END IF 
	
/* retrieve the requested information if there is any */
dw_carry_forward.settransobject(ImageTrans)
ll_return = dw_carry_forward.retrieve(il_docid)

/* make sure we have a valid value */
IF ISNULL(ll_return) OR ll_return < 1 THEN 
	messagebox("No Rows Returned","The Application could not determine the last document that you indexed")
	cb_ok.triggerevent(clicked!)
	RETURN
END IF 


end event

type uo_image_append from u_image_append within w_recall_document
integer x = 338
integer y = 552
integer taborder = 20
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cb_view from commandbutton within w_recall_document
integer x = 1061
integer y = 1076
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&View"
end type

event clicked;STRING ls_doc_type

/* open the viewer application so that the user can see the last image opened */
IF dw_carry_forward.rowcount() < 1 THEN RETURN

/* disable this button */
THIS.enabled = FALSE

// View the document
IF uo_image_append.of_init(il_docid)	<= 0 THEN
	//do something here
END IF
		
ls_doc_type =  uo_image_append.of_get_file_type()
		
CHOOSE CASE ls_doc_type
/*  Imaged document */ 
	CASE 'IMA', 'TIF'
		uo_image_append.of_set_option()
		IF uo_image_append.of_append_image(il_docid) < 0 THEN RETURN
	CASE ELSE	
END CHOOSE




		

	
	
	
end event

type cb_ok from commandbutton within w_recall_document
integer x = 599
integer y = 1076
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Ok"
end type

event clicked;S_WINDOW_MESSAGE		lstr_message
datetime 				ldtm_null_date
LONG                 ll_result

setnull(ldtm_null_date)

/* check the objects to see if they have been selected, if they have then we will 
   send them through to the calling window in order to populate the informaion there
*/
IF dw_carry_forward.rowcount() > 0 THEN
	IF dw_carry_forward.getitemstring(1,"claim_cb") = "1" THEN
		lstr_message.al_doubleparm[1] = dw_carry_forward.getitemnumber(1,"claim_no")
	ELSE
		lstr_message.al_doubleparm[1] = 0
	END IF
	
	IF dw_carry_forward.getitemstring(1,"provider_cb") = "1" THEN
		lstr_message.al_doubleparm[2] = dw_carry_forward.getitemnumber(1,"service_provider_no")
		lstr_message.as_stringparm[5] = dw_carry_forward.getitemstring(1,"service_provider_type_code")
	ELSE
		lstr_message.al_doubleparm[2] = 0
	    lstr_message.as_stringparm[5] = ""
	END IF
		
	IF dw_carry_forward.getitemstring(1,"date_on_doc_cb") = "1" THEN
		lstr_message.adtm_datetimeparm[1] = dw_carry_forward.getitemdatetime(1,"date_on_document")
	ELSE
		lstr_message.adtm_datetimeparm[1] = ldtm_null_date
	END IF
	
	IF dw_carry_forward.getitemstring(1,"type_code_cb") = "1" THEN
		lstr_message.as_stringparm[1] = dw_carry_forward.getitemstring(1,"type_code")
		lstr_message.as_stringparm[3] = dw_carry_forward.getitemstring(1,"doc_class_code")
		lstr_message.as_stringparm[4] = dw_carry_forward.getitemstring(1,"doc_subtype_code")
	ELSE 
		lstr_message.as_stringparm[1] = ""
		lstr_message.as_stringparm[3] = ""
		lstr_message.as_stringparm[4] = ""
	END IF 
	
	IF dw_carry_forward.getitemstring(1,"date_received_cb") = "1" THEN
		lstr_message.adtm_datetimeparm[2] = dw_carry_forward.getitemdatetime(1,"date_received")
	ELSE
		lstr_message.adtm_datetimeparm[2] = ldtm_null_date
	END IF 
	
	IF dw_carry_forward.getitemstring(1,"comment_cb") = "1" THEN
		lstr_message.as_stringparm[2] = dw_carry_forward.getitemstring(1,"comment")
	ELSE
		lstr_message.as_stringparm[2] = ""
	END IF 
	
ELSE
	lstr_message.al_doubleparm[1]     = 0
	lstr_message.al_doubleparm[2]     = 0
	lstr_message.adtm_datetimeparm[1] = ldtm_null_date
	lstr_message.as_stringparm[1]     = ""
	lstr_message.adtm_datetimeparm[2] = ldtm_null_date
	lstr_message.as_stringparm[2]     = ""
	lstr_message.as_stringparm[3]     = ""
	lstr_message.as_stringparm[4]     = ""
	lstr_message.as_stringparm[5] = ""
END IF 

/* close the viewer(s) */
	ll_result = 1
	Do While ll_result > 0
		ll_result = f_close_viewer()
	Loop

/* close the window with the return */
closewithreturn(parent,lstr_message)
end event

type dw_carry_forward from u_dw_online within w_recall_document
integer y = 20
integer width = 1975
integer height = 1004
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "d_recall_document"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;/* we have added a check box that will control the values in the
   the check boxes for all the other column values - it will  
	either be on or off
*/
CHOOSE CASE dwo.name
	CASE 'select_all_cb'
		IF data = "1" THEN 
			/* change all of the other checkboxes to be the same - turn them on */

			THIS.setitem(row,"claim_cb","1")
			THIS.setitem(row,"provider_cb","1")
			THIS.setitem(row,"date_on_doc_cb","1")
			THIS.setitem(row,"type_code_cb","1")
			THIS.setitem(row,"date_received_cb","1")
			THIS.setitem(row,"comment_cb","1")
		ELSE
			/* change all of the other checkboxes to be the same - turn them off */
			THIS.setitem(1,"claim_cb","0")
			THIS.setitem(1,"provider_cb","0")
			THIS.setitem(1,"date_on_doc_cb","0")
			THIS.setitem(1,"type_code_cb","0")
			THIS.setitem(1,"date_received_cb","0")
			THIS.setitem(1,"comment_cb","0")
			
		END IF 
	CASE ELSE	
END CHOOSE
end event

