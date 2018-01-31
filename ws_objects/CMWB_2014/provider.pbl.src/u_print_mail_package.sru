$PBExportHeader$u_print_mail_package.sru
forward
global type u_print_mail_package from datawindow
end type
end forward

global type u_print_mail_package from datawindow
integer width = 686
integer height = 400
string title = "none"
string dataobject = "dw_rtf"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type
global u_print_mail_package u_print_mail_package

forward prototypes
public function integer uf_print_mail_package (string as_token_id)
end prototypes

public function integer uf_print_mail_package (string as_token_id);/*
**		Type  		: Functon
**		Name 		: uf_print_mail_package
**		Arguments 	: string - as_token_id
**		Returns 		:
**		Purpose		: Prints out the mail package for a given token_id.  I would really like to use a datastore but they lack certain visual properties needed to make this work so I am
**                         using a datawindow.  
**
**                         Note the array ls_provider_letters contains the dw names for the RTF letters and the signature delimeter text is contained in the
**                         variable ls_signature_marker
**		Date			: 2014/08/18
**		Author		: David Worboys
**	
**		Modifications
*/
INTEGER li_loop					= 0
INTEGER li_num_chars			= 0
INTEGER li_return					= 0
STRING	ls_error					= ""
STRING 	ls_provider_letters[]	= {"d_provider_letter_english","d_provider_letter_french"} //Pop the french provider letter dw here and all should be fine.
STRING 	ls_rtf						= ""
STRING 	ls_signature_marker	= "thesignaturegoeshere" //Text added to provider letter to delimit where the signature should be placed.
STRING 	ls_signature_rtf_tag	= ""
STRING	ls_user_id				= ""
n_rtf 		ln_rtf

//get the current user id of logged in is user
f_user_id(ls_user_id)

//Now do the RTF boogie - first up get the signature as an RTF tag
ls_signature_rtf_tag = ln_rtf.uf_get_user_signature_rtf_tag( ls_user_id)


FOR li_loop = 1 TO UPPERBOUND(ls_provider_letters[])
	
	THIS.DataObject = ls_provider_letters[li_loop] //Load provider letter DW

	//Grab the letter as RTF text
	ls_rtf = THIS.copyrtf(FALSE,Detail!)
	
	//Put the signature RTF tag into the DW tag
	ls_rtf = ln_rtf.uf_insert_rtf_tag( ls_rtf, ls_signature_marker,ls_signature_rtf_tag)
	
	//Replace letter RTF text with new massaged RTF text		
	THIS.selecttextall( Detail!) 
	THIS.clear()

	li_num_chars = THIS.pastertf(ls_rtf)		
	
	//Now suck back the required details from the database
	THIS.SetTransObject(SQLCA)	
	li_return = THIS.retrieve(as_token_id)
	SQLCA.nf_handle_error('w_paper_mail_package','cb_print',"THIS.retrieve()")			

	//And print the letters out.		
	if li_return > 0 then
		THIS.print()
	else
		ls_error = "Problem Printing Mail Package"
		EXIT
	end if
NEXT

RETURN li_return
end function

on u_print_mail_package.create
end on

on u_print_mail_package.destroy
end on

