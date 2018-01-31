$PBExportHeader$n_auto_letter.sru
$PBExportComments$User Object to send Correspondence to Remote Mail.
forward
global type n_auto_letter from nonvisualobject
end type
end forward

global type n_auto_letter from nonvisualobject
event ue_compose pbm_custom01
end type
global n_auto_letter n_auto_letter

type variables
Protected:
s_corr_data	istr_corr_data
U_word		iuo_ole_word
n_remote_print	invo_n_remote_print
datetime		idt_time
long		il_rcpnt_ctr
string		is_addrsee_name,is_addr_line[6]

// Structure to store the recipients of a letter
s_addressee            is_addressee[]
string		is_saved_path,is_drive
integer		ii_rc
long		il_package_no,il_win_handle

Public:
W_SEND		iw_parentWindow

n_imaging	inv_imaging
String			is_action = 'HM'

end variables

forward prototypes
public function integer uf_insert_original (long al_addressee_id)
public function integer uf_init ()
public function integer uf_begin_letter (ref window aw_parentwin)
public function long uf_get_file_name ()
protected function boolean uf_get_address_line (long al_addr_no)
public function integer uf_compose_letter ()
public function integer uf_send_doc (long al_claim, long al_event_no, string as_reminder_type_code)
end prototypes

event ue_compose;// Load WORD server_parameters,write remote data information, disconnect Word

Integer rc

ii_rc	= uf_init()
If ii_rc >= 0 then ii_rc = uf_compose_letter()

rc = iuo_ole_word.uf_disconnect()
If ii_rc >= 0 then
	If ISValid (invo_n_remote_print) then ii_rc = invo_n_remote_print.nf_eval_All()
END IF		
	




end event

public function integer uf_insert_original (long al_addressee_id);///* Add the original to line  */
//
//
//string	ls_tab=	"Chr$(9)",ls_text_line = "Original Sent To:",ls_value
//
//	If Upper(istr_corr_data.language_code)	= "F" then	ls_text_line =	"Copie - Original Envoyé À"
//	iuo_ole_word.uf_go_to_bookmark("endaddress")
//	iuo_ole_word.uf_insert("Insertpara")
//	ls_value = 	ls_tab + "+" + ls_text_line
//	iuo_ole_word.uf_insert(ls_value)
//	ls_value = uf_get_address_line(al_addressee_id,'A')
//	ls_value =  ls_tab + '+' + ls_tab + '+' + ls_tab + '+"'  + ls_value
//	iuo_ole_word.uf_insert(ls_value)
//	

Return 0

end function

public function integer uf_init ();Integer rc,i
String  ls_name,ls_ini_file_name,ls_full_template_path
Boolean lb_exist

// Get the document paths needed to compose a letter
// Find out the name of the directory to save the files. It should be in the INI file
is_saved_path = ProfileString(vgs_ini_filename,"Correspondence","RemoteLetter","None")
	
If is_saved_path = 'None' then Return(-520)

// Go start the server application and get a connection
Rc =iuo_ole_word.uf_connect()

If rc <> 0 then Return(-500)

// Find out how many recipients of the letter
il_rcpnt_ctr = iw_parentwindow.vii_total_rows

For i = 1 to il_rcpnt_ctr
	// If you don't want to print this guy then skip him
	IF iw_parentwindow.dw_list_recipients.GetItemString(i,"select_flag") = "N" then CONTINUE
	is_addressee[i].rcpnt_id = iw_parentwindow.dw_list_recipients.GetItemNumber(i,"correspond_recipient_id")
	is_addressee[i].rcpnt_addressee_type = iw_parentwindow.dw_list_recipients.GetItemString(i,"correspond_recipient_type_code") 
	is_addressee[i].rcpnt_addressee_role	= iw_parentwindow.dw_list_recipients.GetItemString(i,"correspondence_recipient_addressee_role_")
	is_addressee[i].rcpnt_addressee_location =	iw_parentwindow.dw_list_recipients.GetItemString(i, "recipient_list_address_locat")	
	is_addressee[i].claim_no = iw_parentwindow.dw_list_recipients.GetItemNumber(i, "claim_no")
	is_addressee[i].correspond_recipient_id = iw_parentwindow.dw_list_recipients.GetItemNumber(i,"correspond_recipient_id")
	
	CHoose Case is_addressee[i].rcpnt_addressee_location
		Case 'C'
			// Do nothing
		Case 'P' // Provider get the subtype code and provider number
			is_addressee[i].rcpnt_id =	iw_parentwindow.dw_list_recipients.GetItemNumber(i,"recipient_list_recipient_no")
			is_addressee[i].rcpnt_provider_code = iw_parentwindow.dw_list_recipients.GetItemString(i,"correspond_recipient_subtyp_cd") 
		Case 'E'
			is_addressee[i].rcpnt_id =	iw_parentwindow.dw_list_recipients.GetItemNumber(i,"recipient_list_recipient_no")
	End Choose
Next

Return(0)		
end function

public function integer uf_begin_letter (ref window aw_parentwin);/* You need a window handle to use DDE  */
If ISValid(aw_parentwin) then 	il_win_handle = Handle(aw_parentwin) ELSE Return -100
If il_win_handle < 0 then return - 100

/* Assign Reference Variable to Parent Window   */
iw_parentwindow = aw_parentwin

this.TriggerEvent("ue_compose")
Return(ii_rc)
end function

public function long uf_get_file_name ();int li_sql_rc
long 	ll_file_no

SQLCA.nf_begin_transaction()

	SQLCA.nf_set_display_mess_flag(False)
	UPDATE	Last_Auto_File_No
	SET		last_auto_file_no = last_auto_file_no + 1
	USING		SQLCA;

	If (SQLCA.nf_handle_error(" Update Last_Auto_File_No","User Object N_letter","nf_get_filename")) &
		< 0  Or SQLCA.SQLNROWS <> 1 Then
		SQLCA.nf_rollback_transaction()
		Return -17
	END IF 


	SELECT	last_auto_file_no
	INTO		:ll_file_no
	FROM		Last_Auto_File_No

	USING		SQLCA;
	li_sql_rc = SQLCA.nf_handle_error("SQl select on Last_Auto_File","User Object N_letter","in nf_get_filename")
	IF  li_sql_rc < 0 or li_sql_rc =100 THEN 
		Return -17
	Else
		SQLCA.nf_commit_transaction()		
	END IF

	 
Return(ll_file_no)
end function

protected function boolean uf_get_address_line (long al_addr_no);// Function to store the recipient addressess for a Letter 
//	Parameters:  Array Value to structure uostr_addressee
Long    ll_accident_employer_operation_no
Integer i
String  ls_name, ls_contact_name, ls_addr1, ls_addr2, ls_addr3, ls_city, ls_prov, ls_pc, ls_country
String  ls_value, ls_line1, ls_line2, ls_line3, ls_line4, ls_line5, ls_line6, ls_employer_type_code

// Get the next available value in the array of addresses
SetNull(ls_value)

IF is_addressee[al_addr_no].rcpnt_id = 0 THEN RETURN(FALSE)

SQLCA.nf_set_display_mess_flag(FALSE)

CHOOSE CASE is_addressee[al_addr_no].rcpnt_addressee_location
	CASE ("C")
	// Retrieve the claimant information
		SELECT name1, name2, address_line1, address_line2, address_line3, city, province, country, postal_code
		  INTO :ls_name, :ls_contact_name, :ls_addr1, :ls_addr2, :ls_addr3, :ls_city, :ls_prov, :ls_country, :ls_pc
		  FROM RECIPIENT_ADDRESS
		 WHERE correspond_recipient_id = :is_addressee[al_addr_no].rcpnt_id
		   AND active_flag = 'Y'
		 USING SQLCA ;

		SQLCA.nf_handle_error("SQL select on RECIPIENT_ADDRESS","n_auto_letter","in uf_get_address_line")											 
	CASE ("E") 
		SELECT C.accident_employer_operation_no, E.employer_type_code  
		  INTO :ll_accident_employer_operation_no, :ls_employer_type_code 
		  FROM CLAIM C, EMPLOYER E   
		 WHERE C.claim_no = :is_addressee[al_addr_no].claim_no 
		   AND C.accident_employer_no = E.employer_no ;
		SQLCA.nf_handle_error("SQL select on CLAIM","n_auto_letter","in uf_get_address_line")				

		IF ls_employer_type_code = "S" THEN
			SELECT E.employer_legal_name, EA.address_line1, EA.address_line2, EA.address_line3, EA.city, 
					 EA.prov_state_code, EA.postal_code, EA.country_code, O.operation_name 
			  INTO :ls_name, :ls_addr1, :ls_addr2, :ls_addr3, :ls_city, :ls_prov, :ls_pc, :ls_country, :ls_contact_name
			  FROM EMPLOYER_ADDRESS EA, EMPLOYER E, OPERATION O 
			 WHERE EA.employer_no = :is_addressee[al_addr_no].rcpnt_id
				AND EA.address_type_code = "BA" 
				AND EA.employer_no = E.employer_no 
				AND E.employer_no = O.employer_no 
				AND O.operation_no = :ll_accident_employer_operation_no ;
				
			SQLCA.nf_handle_error("SQL select on Employer","n_auto_letter","in uf_get_address_line")				
		ELSE
			SELECT E.employer_legal_name, EA.address_line1, EA.address_line2, EA.address_line3, EA.city, 
					 EA.prov_state_code, EA.postal_code, EA.country_code, ""
			  INTO :ls_name, :ls_addr1, :ls_addr2, :ls_addr3, :ls_city, :ls_prov, :ls_pc, :ls_country, :ls_contact_name
			  FROM EMPLOYER_ADDRESS EA, EMPLOYER E
			 WHERE EA.employer_no = :is_addressee[al_addr_no].rcpnt_id
				AND EA.address_type_code = "BA" 
				AND EA.employer_no = E.employer_no ;

			SQLCA.nf_handle_error("SQL select on Employer","n_auto_letter","in uf_get_address_line")				
		END IF
	CASE ('P') 
		SELECT name, address_line1, address_line2, city, prov_state_code,
				 postal_code, country_code, contact_name
		  INTO :ls_name, :ls_addr1, :ls_addr2, :ls_city, :ls_prov, :ls_pc, :ls_country, :ls_contact_name
		  FROM PROVIDER
		 WHERE provider_no = :is_addressee[al_addr_no].rcpnt_id 
		   AND provider_type_code = :is_addressee[al_addr_no].rcpnt_provider_code 
		   AND active_flag = 'Y' 
		 USING SQLCA ;

		SQLCA.nf_handle_error("SQL Select on Provider","n_auto_letter","in uf_get_address_line")				
END CHOOSE

IF ls_name <> "" THEN is_addr_line[1] = TRIM(Upper(ls_name)) ELSE SetNull(is_addr_line[1])
IF ls_contact_name <> "" THEN is_addr_line[2] = TRIM(Upper(ls_contact_name)) ELSE SetNull(is_addr_line[2])
IF	ls_addr1 <> ""	THEN is_addr_line[3] = TRIM(Upper(ls_addr1)) ELSE SetNull (is_addr_line[3])
IF ls_addr2 <> ""	THEN is_addr_line[4] = TRIM(Upper(ls_addr2))	ELSE SetNull(is_addr_line[4])
IF	ls_addr3 <> ""	THEN is_addr_line[5] = TRIM(Upper(ls_addr3)) ELSE SetNull (is_addr_line[5])
is_addr_line[6] = TRIM(Upper(ls_city)) + "," +TRIM(Upper(ls_prov)) + " " +TRIM(UPPER(ls_pc)) 
IF is_addr_line[6] = "" THEN SetNull(is_addr_line[6])

IF is_addressee[al_addr_no].rcpnt_addressee_role = 'A' THEN 
	is_addrsee_name = is_addr_line[1]
	IF Trim(is_addr_line[2]) <> "" THEN &
		is_addrsee_name = is_addrsee_name + "^" + is_addr_line[2] /* Should be contact name  */
END IF
	
RETURN(TRUE)

end function

public function integer uf_compose_letter ();datetime	ldte_date
long     ll_file_no, li_max_rcpnt, ll_rc, ll_handle, ll_claim_no, ll_format_type
string   ls_physical_file_name, ls_value, ls_tab, ls_new_line, ls_addressee, ls_name, &
			ls_drive, ls_bookmark_list[], ls_extension
int      rc, i, j, k, li_bookmark_count, li_bookmarks, li_found_bookmark
boolean	lb_found, lb_stop

s_addressee	lstr_addressee[]

lstr_addressee[] = is_addressee[]

// Initialization 
idt_time = f_server_dateTime()

// Get the drive letter which is needed by remote print object
ls_drive	= Left(is_saved_path,Pos(is_saved_path,':')-1)

// We only want the drive letter 
If LEN(ls_drive) > 1 then Return(-520)	
ll_claim_no = iw_parentwindow.VISTR_CORRESPOND_CLAIM.claim_no

// Get the type of document(regular vs. macro enabled)
ls_extension = iw_parentwindow.vistr_correspond_claim.corr.template_extension
IF UPPER(ls_extension) = 'DOCM' THEN
	ll_format_type = 13
ELSE
	ll_format_type = 12
END IF

// Set the Print Package, once for each original 
rc = iuo_ole_word.uf_file_open(iw_parentwindow.VISTR_CORRESPOND_CLAIM.corr.document_name,false,true,2)
IF rc < 0 then	Return (-530)

// Make sure that there are bookmarks in the document
iuo_ole_word.uf_list_bookmarks(ls_bookmark_list)
li_bookmark_count = UpperBound(ls_bookmark_list)
If li_bookmark_count < 2 then Return (-580) 

// If you have more than one recipient then you must ensure the two 
// replacement addressee bookmarks are in the original document.
If il_rcpnt_ctr > 1 then
	For i =1 to li_bookmark_count
		If ls_bookmark_list[i] = "startaddress" then li_bookmarks++ 
		If ls_bookmark_list[i] = "endaddress"		then li_bookmarks++
	NEXT
	If li_bookmarks < 1 then Return(-590)
END IF

// Initialization for  the Remote print objects
IF invo_n_remote_print.nf_Init() < 0 then Return (-300)
If invo_n_remote_print.nf_Set_Print_Package(ll_claim_no,'R','CD',&
	iw_parentwindow.VISTR_CORRESPOND_CLAIM.CORR.document_type,&
	iw_parentwindow.VISTR_CORRESPOND_CLAIM.CORR.comments,&
	iw_parentwindow.VISTR_CORRESPOND_CLAIM.CORR.language_code) < 0 then
		Return (-300)
END IF

// Make sure you have an addressee
li_max_rcpnt = UpperBound(is_addressee)
I = 1
DO
	If is_addressee[i].rcpnt_addressee_role = "A" then lb_found = True
	i++
Loop Until (i >= li_max_rcpnt or lb_found)

If not lb_found then return(-130)



/* Everything is OK start processing    */
For i = 1 to li_max_rcpnt
	CHoose Case (is_addressee[i].rcpnt_addressee_role )
	Case "A"  /* Addressee */
		If Not uf_get_address_line(i) then Return(-130)
		IF ( invo_n_remote_print.nf_Set_Print_Item ("O"," ","P",'C',is_addressee[i].correspond_recipient_id,idt_time,iw_parentwindow.VISTR_CORRESPOND_CLAIM.corr.document_name)) < 0 then Return(-320)
		lb_stop = False
	Case "C"  /* Carbon_copy */
		ll_rc =	uf_get_file_name()
		If ll_rc < 0  THEN	Return (ll_rc)
		ls_physical_file_name = string(ll_rc)+ '.' + iw_parentwindow.vistr_correspond_claim.corr.template_extension
		IF not	uf_get_address_line(i) then Return -430
		
		li_found_bookmark = iuo_ole_word.uf_go_to_bookmark('startaddress')
		iuo_ole_word.uf_new_paragraph(1,13)
		If li_found_bookmark = 0 THEN
			RETURN -1
		END IF
		iuo_ole_word.uf_Extend_line()
		
		j = UpperBound(is_addr_line[]) // maximum of six lines of address
		
		For k = 1 to  j 
			If is_addr_line[k] = " " then 	SETNULL(is_addr_line[k])
			If NOT ISNULL(is_addr_line[k])  THEN	/* SKIP IF BLANK LINES  */
				iuo_ole_word.uf_set_font("Times New Roman",11)
				ll_rc = iuo_ole_word.uf_del_bookmark('endaddress')
				IF ll_rc < 0 THEN
					RETURN ll_rc
				END IF
				IF k = 1 THEN
					iuo_ole_word.uf_type(is_addr_line[k])
				ELSE
					iuo_ole_word.uf_insert(is_addr_line[k])
					iuo_ole_word.uf_set_font("Times New Roman",11)
				END IF
				iuo_ole_word.uf_end_of_line()
				iuo_ole_word.uf_add_bookmark('endaddress')
				iuo_ole_word.uf_new_paragraph(1,13) // Go down to the next line
			END IF
		Next
		/* You only need to add original line only once  */
		IF Not lb_stop then
			lb_stop = True	
			iuo_ole_word.uf_new_paragraph(1,13) // Go down to the next paragraph
			ls_value = 'COPY - ORIGINAL TO:'
			If UPPER(istr_corr_data.language_code)	='F' then ls_value = 'COPIE - ORIGINAL ENVOYÉ À :'
			iuo_ole_word.uf_extend(true)
			iuo_ole_word.uf_set_font("Times New Roman",11) 
			iuo_ole_word.uf_insert(ls_value)
			iuo_ole_word.uf_extend(true) 
			iuo_ole_word.uf_new_paragraph(1,9) // Go tab 
			rc = POS(is_addrsee_name,"^") 
			IF rc > 0 then
				ls_value = left(is_addrsee_name,rc - 1) /* Don't want the ^  */
				iuo_ole_word.uf_insert(ls_value)
			Else
				iuo_ole_word.uf_insert(is_addrsee_name )
			END IF
		ELSE
			li_found_bookmark = iuo_ole_word.uf_go_to_bookmark('endaddress')
			If li_found_bookmark = 0 THEN
				RETURN -1
			ELSE
				iuo_ole_word.uf_delete()
			END IF
		END IF
		
		rc =	iuo_ole_word.uf_file_save_as(is_saved_path+ls_physical_file_name, ll_format_type)
		
		If rc <	0 then	Return(-400)
		
		/* Need to put the file path in the last parameter */
		If (invo_n_remote_print.nf_Set_Print_Item	('C'," ",'P','C',is_addressee[i].correspond_recipient_id,idt_time,&
			is_saved_path+ls_physical_file_name)) < 0 then Return(-320)
	Case Else

		// Not intrested in any others
	END Choose	
Next 
/* The original was never closed  */
iuo_ole_word.uf_file_close()
Return(0)
end function

public function integer uf_send_doc (long al_claim, long al_event_no, string as_reminder_type_code);LONG		ll_fldid, ll_selected_row,	ll_catid, ll_setid, ll_docid, ll_results, ll_claim_no, ll_event_no, ll_count, ll_set_id
STRING   ls_action, ls_action_date, ls_keyword, ls_claimant_name
STRING   ls_last_name, ls_given_names, ls_admin_region_code
DATETIME	ldtm_action_date


/* Create an instance of the user object for the imaging functions.
*/
inv_imaging = CREATE n_imaging

/* Get variables, and validate.
*/

SELECT i.last_name, i.given_names, c.admin_region_code
INTO   :ls_last_name, :ls_given_names, :ls_admin_region_code
FROM 	 INDIVIDUAL i, CLAIM c
WHERE  c.claim_no = :al_claim
AND	 i.individual_no = c.individual_no
USING SQLCA;

SQLCA.nf_handle_error("n_auto_letter","uf_send_doc","SELECT FROM INDIVIDUAL")

ls_claimant_name =  TRIM(ls_given_names) + " " + TRIM(ls_last_name)
ll_claim_no		  = al_claim
ls_action        = is_action

SELECT 	set_id
INTO		:ll_set_id
FROM		Admin_Region_Sets_Xref
WHERE	admin_region_code = :ls_admin_region_code
USING	ImageTrans;

ImageTrans.nf_handle_error("n_auto_letter","uf_send_doc","SELECT FROM Admin_Region_Sets_Xref")


// Check to make sure the proper Reminder bucket exists
SELECT  COUNT(*)
INTO    :ll_count
FROM 	Reminder_Routing
WHERE   Reminder_Routing.setid = :ll_set_id
AND		Reminder_Routing.reminder_type_code = :as_reminder_type_code
USING   ImageTrans;

ImageTrans.nf_handle_error("n_auto_letter","uf_send_doc","SELECT FROM Reminder_Routing")

IF ll_count = 0 THEN
	MessageBox('ERROR','The '+as_reminder_type_code+ ' Reminder bucket has not been set-up. The Event Log just created will not be automatically sent..',Exclamation!,ok!)
	RETURN -1
END IF

/* Get the category id number for the Reminder */
SELECT	CAT.catid, CAT.setid
INTO   	:ll_catid, :ll_setid
FROM  	CAT,
       	  	Reminder_Routing 
WHERE	CAT.catid = Reminder_Routing.catid
AND		Reminder_Routing.reminder_type_code = :as_reminder_type_code
AND		Reminder_Routing.setid = :ll_set_id
USING	ImageTrans;

ImageTrans.nf_handle_error("n_auto_letter","uf_send_doc","SELECT FROM CAT")

IF ll_setid = 0 THEN
	MessageBox('WARNING',"The auto event just created can not be sent to the '+as_reminder_type_code+' Reminder Bucket.")
END IF	

Select TOP 1 (DateAdd(MONTH,1,getdate()))
Into	:ldtm_action_date
From sysobjects;

SQLCA.nf_handle_error('n_auto_letter','uf_send_doc','Select  TOP 1 (DateAdd(MONTH,1,getdate()))')

ldtm_action_date = Datetime(Date(ldtm_action_date))
ls_keyword       = "See Claim event "+String(al_event_no) 

/* Create a work folder.
*/

ImageTrans.nf_begin_transaction()
	
// this function begins & commits its own transaction, then inserts data outside of txn,
// so it must be enclosed within its own txn

ll_fldid = inv_imaging.nf_create_workfolder("uf_send_doc",ll_catid)
IF ll_fldid = -1 THEN
	// signal error if cannot create work folder
	Error.Text        = ImageTrans.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Unable to create work folder.' + &
                          '~r~nCat ID: ' + String(ll_catid) + '.' 
	ELSE
		Error.Text        = Error.Text + '.~r~nUnable to create work folder.' + &
                          '~r~nCat ID: ' + String(ll_catid) + '.' 
	END IF
	Error.Object      = 'n_auto_letter'
	Error.is_database = 'IMARA_DB'
	Error.il_dbcode   = ImageTrans.SQLDBCode
	Error.ObjectEvent = 'uf_send_doc'
	SignalError()	
END IF

ImageTrans.nf_commit_transaction()

// start another txn
ImageTrans.nf_begin_transaction()

/* Index the work folder.
*/
INSERT INTO CLAIM_WORKING
(folderid , claim_no , action_code, action_date,action_note)
Values (:ll_fldid,:ll_claim_no,:ls_action,:ldtm_action_date,:ls_keyword)
USING ImageTrans;

ImageTrans.nf_handle_error("n_auto_letter","uf_send_doc","Update CLAIM_WORKING")


/* Update the folder with the new name.
*/
UPDATE FLD
	SET fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
 WHERE fldid = :ll_fldid
USING ImageTrans;

ImageTrans.nf_handle_error("n_auto_letter", "", "uf_send_doc - Update FLD")


ImageTrans.nf_commit_transaction()


RETURN 0

end function

event constructor;iuo_ole_word = Create u_Word

If Not ISValid(iuo_ole_word) then
	// signal error if cannot create u_word
	Error.Text        = 'Unable to create OLE object needed for Word documents.'
	Error.Object      = 'n_auto_letter'
	Error.is_database = 'IMARA_DB'
	Error.ObjectEvent = 'constructor'
	SignalError()
END IF	

invo_n_remote_print = Create n_remote_print

If Not ISVALID(invo_n_remote_print) then
	// signal error if cannot create n_remote_print
	Error.Text        = 'Unable to create Remote object needed for remote printing.'
	Error.Object      = 'n_auto_letter'
	Error.is_database = 'IMARA_DB'
	Error.ObjectEvent = 'constructor'
	SignalError()
END IF	
end event

event destructor;If ISValid(iuo_ole_word) then destroy iuo_ole_word
If ISValid(invo_n_remote_print) then destroy (invo_n_remote_print)


end event

on n_auto_letter.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_auto_letter.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

