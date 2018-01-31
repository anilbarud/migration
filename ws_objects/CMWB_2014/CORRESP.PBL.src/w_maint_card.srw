$PBExportHeader$w_maint_card.srw
$PBExportComments$Descendent window used for detail screen
forward
global type w_maint_card from w_maint_dwo
end type
type cb_search from commandbutton within w_maint_card
end type
end forward

global type w_maint_card from w_maint_dwo
integer x = 997
integer y = 616
integer width = 2743
integer height = 1824
string title = "Maintain Card File"
string menuname = "m_cmwb_notools"
boolean minbox = true
boolean maxbox = true
event ue_search pbm_custom09
cb_search cb_search
end type
global w_maint_card w_maint_card

type variables
Private:

DataWindowChild	   vidw_category	
DataWindowChild	  vidwc_city
DataWindowChild	  vidwc_province
DataWindowChild	  vidwc_country
string		  vis_recipient_type,vis_country_code

m_cmwb_notools vim_menu
s_cardfile_parms	vis_cardfile_parms	
boolean I_Authorized_Access  //flag to check window security
Public:
Long		  vil_recipient_id
end variables

forward prototypes
public subroutine wf_clear_fields ()
public function string wf_check_item ()
private subroutine wf_validate_data ()
end prototypes

on ue_search;call w_maint_dwo::ue_search;int  lw_parent_x,lw_parent_y,vll_rownum
string ls_category_cd,vls_find_expression
Window  lw_act_sheet,lw_window
lw_act_sheet = GetActiveSheet()
lw_parent_x =this.x
lw_parent_y = this.y
/* Category Must be correct
*/
lw_window  = w_maint_card
ls_category_cd =dw_detail.GeTItemString(1,"recipient_type_cd")
vls_find_expression = "correspond_recipient_type_cd = '"+ls_category_cd+"'"
vll_rownum = vidw_category.Find(vls_find_expression, 1, vidw_category.RowCount())
IF vll_rownum >0  THEN &
	vis_cardfile_parms.recipient_type =vidw_category.GetItemString(vll_rownum,"correspond_recipient_type_cd")

	
vis_cardfile_parms.parent_window = lw_window 

OpenwithParm(w_search_cardfile,vis_cardfile_parms)

vil_recipient_id = Message.DoubleParm

IF vil_recipient_id > 0 THEN
	this.TriggerEvent( "ue_retrieve_data" )
	END IF

dw_detail.Setfocus()

end on

public subroutine wf_clear_fields ();cb_add.enabled=true
cb_save.enabled = false
cb_delete.enabled = true
dw_detail.reset()
dw_detail.insertrow(0)




end subroutine

public function string wf_check_item ();dwitemstatus l_status
Integer      i, ll_row_num, il_colcount
String       ls_colvarible, ls_colname, ls_colvalue

// Get rowcount and value 
il_colcount = integer(dw_detail.Describe("datawindow.column.count"))

// Set up all the initial values for string items that can be updated in the datawindow
//	Find the column that's been flagged as not being modified and determine that it's not null or blank
FOR i = 1 TO il_colcount
	ls_colvarible ='#'+string(i)+'.name'
	ls_colname =dw_detail.Describe(ls_colvarible)
	
	CHOOSE Case ls_colname
		CASE  "city", "province", "country", "name1", "address_line1", "recipient_type_cd", "postal_code"
			l_status = dw_detail.GETITEMSTATUS(1,ls_colname,Primary!)

			IF l_status = NotModified! THEN ls_colvalue = dw_detail.GetItemString(1,ls_colname)

			If NOT ISNULL(ls_colvalue) THEN 
				IF match(ls_colvalue,"[%+]") THEN
					IF ls_colvalue = ' ' then Return(ls_colname)
				END IF
			END IF
		case Else

	 END CHOOSE
NEXT

SETNULL(ls_colname)
Return(ls_colname)

end function

private subroutine wf_validate_data ();Long   ll_rc,vll_rownum
String ls_country, ls_province, ls_city, vls_find_expression, ls_pc

// The country description is a non-editabled field
ls_country 	= UPPER(dw_detail.GetItemString(1,'country'))
ls_province = UPPER(dw_detail.GetItemString(1,'province'))
ls_city		= UPPER(dw_detail.GetItemString(1,'city'))
ls_pc			= dw_detail.GetItemString(1,'postal_code')

// The country description is a non-editabled field used to find the country code
If ISNULL(ls_province) or ls_province = "" then ls_province = ' '
If ISNULL(ls_city) or ls_city = "" then  ls_city = ' ' 

IF(ls_city = ' ' or ISNULL(ls_city) or ls_city="") = TRUE then
	vib_data_ok = False			
	MessageBox("Invalid City", "Missing value for City.  Please correct.", Exclamation!)
	Return
END IF

// If Canada validiate postal code
IF ls_country = "CAN" THEN
	IF NOT (IsNulL(ls_pc) OR Trim(ls_pc) = '') THEN
		IF Len(ls_pc) <> 7 OR NOT (IsNumber(Mid(ls_pc,2,1)) AND IsNumber(Mid(ls_pc,5,1)) AND IsNumber(Mid(ls_pc,7,1)))&
			OR IsNumber(Mid(ls_pc,1,1)) OR IsNumber(Mid(ls_pc,3,1)) OR IsNumber(Mid(ls_pc,6,1)) OR NOT (Mid(ls_pc,4,1) = ' ') THEN
			MessageBox("Invalid Postal Code", "The postal code entered is not in the correct format.")
			vib_data_ok = False
			Return
		END IF
	ELSE
		MessageBox("Invalid Postal Code", "The postal code entered is not in the correct format.")
		vib_data_ok = False
		Return
	END IF
END IF

// If New Brunswick must be a valid city
IF ls_province = "NB" and vis_country_code = "CAN" then 
	vls_find_expression = "location_desc1 = '" + ls_city + "' AND prov_state_code = 'NB' AND country_code = 'CAN'" 
	vll_rownum 		=	vidwc_city.Find(vls_find_expression,1,vidwc_city.Rowcount())
	
	IF vll_rownum = 0 THEN
		MessageBox("Invalid City", "The city entered is not currently valid in NB.  Please correct.", Exclamation!)
		vib_data_ok = False
		ReTurn
	END IF
ELSEIF vis_country_code = "CAN" then			
	vls_find_expression	= 	"location_code = '" + ls_province + "' AND country_code = 'CAN'"	
	vll_rownum 		=	vidwc_province.Find(vls_find_expression,1,vidwc_province.Rowcount())	
	
	IF vll_rownum = 0 THEN
		dw_detail.uf_set_pbmessage ( True )
		vib_data_ok = False
		MessageBox("Invalid Province", "The province entered is not currently valid in Canada.  Please correct.", Exclamation!)
		Return			
	END IF
ELSEIF vis_country_code = 'USA' THEN  // IF US must be a valid state
	vls_find_expression = "location_code = '" + ls_province + "' AND country_code = 'USA'" 
	vll_rownum = vidwc_province.Find(vls_find_expression, 1, vidwc_province.RowCount())      	
	IF vll_rownum = 0 THEN
		vib_data_ok = False
		MessageBox("Invalid State", "The state entered is not currently valid in the USA.  Please correct.", Exclamation!)
		Return
	END IF
ELSEIF ls_province <> ' ' then
	vib_data_ok = False
	MessageBox("Invalid State/Province", "State/Province must be blank for all countries, except USA or Canada.", Exclamation!)
	Return
END IF
vib_data_ok = True

end subroutine

on ue_retrieve_data;call w_maint_dwo::ue_retrieve_data;int		vli_return_code
/* Why isn't CB_ADD disabled   */
dw_detail.Retrieve(vil_recipient_id)
vli_return_code = SQLCA.nf_handle_error(dw_detail.DataObject,"w_maint_card","ue_retrive_data event")
If vli_return_code < 0 then return
cb_delete.enabled= True
cb_save.enabled = True
cb_add.enabled	= False


end on

on open;call w_maint_dwo::open;//////////////////////////////////////////////////////////////////////////////////
////
//Purpose: This is a window Object (Descendant) used to maintain card file
//			  entries. It has been inherited from w_mintain which contains
//			  some of the functionality needed to run.
//
//Log:
//
//			Date			Who				What
//-------....--------...------------....------------------------------------------
//		 96/01/09		B.Burton			Initial Version
//
// Open Event
//////////////////////////////////////////////////////////////////////////////////
//	
//-----APPLICATION SECURITY CODE-----
G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True              //declared as an instance variable

long	vll_result





// Set up the instance variable for the menu so that we can refer to the frames menu later

	vim_menu = m_cmwb_notools

	w_maint_card.setredraw(false)
//
//
// Ensure that the menu attached to this sheet (or this instance of this sheet) always shows the
// current status of the workbench option (ie. it is grey when vii_sheet_counter = vii_max_number_sheets)
//
	vim_menu.m_workbench.m_print.Enabled = False
	vim_menu.m_workbench.m_print.toolbaritemvisible = False
	vim_menu.m_workbench.m_worksheet.Enabled = True
	vim_menu.m_workbench.m_print.Enabled = False
	vim_menu.m_workbench.m_print.toolbaritemvisible = False
	vim_menu.m_workbench.m_close.Enabled = True
	vim_menu.m_workbench.m_close.toolbaritemvisible = True
	wf_clear_fields()
	cb_delete.enabled=false

	
	
	w_maint_card.setredraw(true)

	
end on

event ue_postopen;call super::ue_postopen;/* Get the Data Window Child for Correspond Recipient Category
*/
dw_detail.GetChild("recipient_type_cd",vidw_category)

// Get the Data Window Child for Municipality
dw_detail.GetChild("city",vidwc_city)

// Get the Data Window Child for Province/State
dw_detail.GetChild("province",vidwc_province)

// Get the Data Window Child for Country
dw_detail.GetChild("country",vidwc_country)

// Add retrieve and any additinal logic that can be done after the window is displayed.

dw_detail.SETFOCUS()


end event

on ue_set_fields;call w_maint_dwo::ue_set_fields;//******************************************************************
///
// The recipient id is fetched from the ancestor window.
// The remaining fields are ones that are not visible to
// users.
//
/// Make sure the rowstatus is set for insert
long    ll_recipient_id
boolean lb_data_ok
dwitemSTATUS lb_status

dw_detail.SetItem(vil_curr_row, "correspond_recipient_id", vil_recipient_id)
dw_detail.SetItem(vil_curr_row, "correspond_recipient_subtyp_cd", " ")
dw_detail.SetItem(vil_curr_row, "card_file_flag", "Y")
dw_detail.SetItem(vil_curr_row, "active_flag", "Y")
lb_status = dw_detail.GETITEMSTATUS(1,0,Primary!)
lb_data_ok =vib_data_ok
ll_recipient_id = vil_recipient_id
end on

on w_maint_card.create
int iCurrent
call super::create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_search=create cb_search
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_search
end on

on w_maint_card.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_search)
end on

type dw_detail from w_maint_dwo`dw_detail within w_maint_card
integer x = 9
integer y = 48
integer width = 2217
integer height = 1608
string dataobject = "d_cardfile_data"
end type

event dw_detail::itemchanged;call super::itemchanged;
int		vli_rc, vli_nbr_rows
long		vll_rownum
string	vls_gettext, vls_into_string,ls_city,ls_province,ls_country,ls_pc,ls_category_cd
string	vls_mod_str, vls_country, vls_find_expression 
/* Get the value from the dropdown data window
*/

//vll_rownum					=	GetRow()
//vls_gettext					=	GetText()
vls_gettext					= 	LeftTrim(data) /* Retrieves data even in dddw not the display value */
/*  Use object function to set the actioncode to Reject the data and suppress any PowerBuilder Messages
*/
this.uf_set_pbmessage ( True )
//// Check if any double quotes exists in the string
/* Bug in PB 5.02 with null as a value in either argument, changing null to ' '  */ 
IF ISNULL(vls_gettext) then vls_gettext = ' '
IF Match(vls_gettext, "~"+") THEN	
		MessageBox(title,"Cannot enter double quotes, please re-enter.")
		return 2
END IF	


/* Get any values already set
*/

//ls_city 		= 		UPPER(this.GetItemString(1, "city"))			
//ls_province =		UPPER(this.GetItemString(1,"province"))
//ls_country 	= 		UPPER(this.GetItemString(1,"country"))
ls_city 		= 		UPPER(this.object.city[row])		// change from this.object	
ls_province =		UPPER(this.object.province[row])
ls_country 	= 		UPPER(this.object.country[row])


IF (IsNull(ls_city) or ls_city = "" or   TRIM(ls_city) = ' ') = TRUE THEN 		ls_city = ' '

IF (IsNull(ls_province) or  ls_province ="" or TRIM(ls_province) = ' ') = TRUE THEN	ls_province = ' '

IF (IsNull(ls_country) or ls_country=""  or Trim(ls_country) = ' ') = TRUE THEN		ls_country = ' '

/* Need to find the country code for Canada rather than its description - see postal code and province check */	
//IF ls_country <> " " then
//	vls_find_expression = "location_desc1 = '"+ls_country+"'"
//	vll_rownum = vidwc_country.Find(vls_find_expression, 1, vidwc_country.RowCount())
//	IF vll_rownum > 0 then
//		ls_country = vidwc_country.GetItemString(vll_rownum,'location_code')
//		vis_country_code = ls_country
//	END IF
//END IF		
//
//CHOOSE Case GetColumnName()

CHOOSE Case	dwo.Name
		
Case "recipient_type_cd"
			ls_category_cd = vls_gettext
			vls_find_expression = "correspond_recipient_type_cd = '"+ls_category_cd+"'"
			vll_rownum = vidw_category.Find(vls_find_expression, 1, vidw_category.RowCount())
		
			IF vll_rownum >0  THEN
				vis_cardfile_parms.recipient_type =vidw_category.GetItemString(vll_rownum,"correspond_recipient_type_cd")
			else
				MessageBox(parent.title, "Invalid category selected for Recipient",Information!)
				vib_data_ok = False
				Return (1)

			END IF

Case	'postal_code'
	ls_pc			= vls_gettext

	IF ls_country = "CAN" THEN

   	IF NOT (IsNulL(ls_pc) OR Trim(ls_pc) = '') THEN
      	IF Len(ls_pc) <> 7 OR NOT (IsNumber(Mid(ls_pc,2,1)) AND IsNumber(Mid(ls_pc,5,1)) AND IsNumber(Mid(ls_pc,7,1)))&
         	OR IsNumber(Mid(ls_pc,1,1)) OR IsNumber(Mid(ls_pc,3,1)) OR IsNumber(Mid(ls_pc,6,1)) OR NOT (Mid(ls_pc,4,1) = ' ') THEN
      	// invalid postal code
         	MessageBox("Invalid Postal Code", "The postal code entered is not in the correct format.")
				vib_data_ok = False
				Return(1)
			END IF

   	END IF

	END IF

/* city inside NB must be CANADA. The Recipient_address table must store the textual descripition of city
even if the city is in New Brunswick.  We are not using a lookup code to deipher the city name. 
*/
Case 'city'
	ls_city = UPPER(vls_gettext)
	IF(ls_city = ' ' or ISNULL(ls_city) or ls_city="") = TRUE then
			this.uf_set_pbmessage (True)	
			vib_data_ok = False			
  			MessageBox("Invalid City", "Missing value for City.  Please correct.", Exclamation!)
			Return(1)
	END IF
	
	vll_rownum = vidwc_city.Find("location_desc1 ='" + ls_city+"'",1,vidwc_city.Rowcount())
	
	IF vll_rownum > 0 THEN
			
			/* Set the Province and City  */
//			this.Object.province[row] = vidwc_city.Object.prov_state_code[vll_rownum]
			this.Object.province[row] = vidwc_city.GetItemString(vll_rownum,'prov_state_code')
//			this.SetItem(row,"province",vidwc_city.GetItemString(vll_rownum,'prov_state_code'))
			/* Use object.column[row] */
			ls_province	=	this.Object.province[row]
			
//			ls_province = vidwc_city.GetItemString(vll_rownum,'prov_state_code')
			this.Object.country[row] = vidwc_city.GetItemString(vll_rownum,'country_code')
			ls_country = this.Object.country[row]
			
//			this.SetItem(row,"country",vidwc_city.GetItemString(vll_rownum,'country_code'))
//			ls_country = vidwc_city.GetItemString(vll_rownum,'country_code') 
	END IF

Case 'province'
	
	ls_province = UPPER(vls_gettext)

	If NOT ls_province = " "  then
		vll_rownum = vidwc_province.Find("location_code = '" + ls_province+"'",1,vidwc_province.Rowcount())
		IF vll_rownum > 0 THEN
			/* Set the country code  */
				this.Object.country[row] = vidwc_province.GetItemString(vll_rownum,"country_code")
//				This.SetItem(row,"country",vidwc_province.GetItemString(vll_rownum,"country_code"))
				ls_country = this.Object.country[row]
//				ls_country =vidwc_province.GetItemString(vll_rownum,"country_code")
				
		END IF
	END IF
	

Case 'country'
 /* validate country outside North America there should be no province or state
 	 
 */
    	ls_country = UPPER(vls_gettext)
//		vll_rownum = vidwc_country.Find("location_desc1 = '" + ls_country + "'" ,1,vidwc_country.Rowcount())
//		IF vll_rownum = 0 then
//				this.uf_set_pbmessage(TRUE)
//        		MessageBox("Invalid Country", "Unrecognizable country.  Please correct!", Exclamation!)
//				vib_data_ok = False
//        		Return(1)
//		END IF
//					
//  		
//		ls_country =  vidwc_country.GetItemString(vll_rownum,'location_code')
//		If ls_country <>  'USA' or ls_country <> 'CAN' then 	This.SetItem(row,"province"," ")
		If ls_country <>  'USA' or ls_country <> 'CAN' then 	This.Object.province[row] = " " 

Case  'name1'
	UPPER(vls_gettext)
	IF IsNull( vls_gettext ) OR match(vls_gettext,"[%+]")  THEN 
				MessageBOX("Invalid Name","Name cannot be blank or begin with a %!")
				vib_data_ok = False
				RETURN(1)
		END IF
	
Case 'address_line1'

	IF (Isnull(vls_gettext)  or vls_gettext ="" or vls_gettext  = ' ') = True Then
		MessageBOX("Invalid Address","Address line 1 cannot be blank !")
		vib_data_ok=False
		RETURN(1)
	END IF
		 	
END CHOOSE
vis_country_code = ls_country 



end event

type cb_save from w_maint_dwo`cb_save within w_maint_card
integer x = 2341
integer y = 880
integer width = 297
integer height = 92
integer taborder = 50
end type

event cb_save::clicked;long   ll_rc,vll_rownum
string ls_country, ls_province, ls_city, vls_find_expression, ls_pc

// SEE IF THE USER USED SAVE INSTEAD OF ADD TO MODIFY A RECORD, didn't get an id yet	
vib_data_ok = false
IF dw_detail.uf_accept_dw() = 1 then
	IF vil_recipient_id = 0 then 
 		MessageBox("SAVE ERROR",  "This is a new record, use ADD to Insert a new record" &
           +"~n~r Use SEARCH then SAVE to update an existing record", INFORMATION!)
		w_maint_card.SetRedraw(False)
		wf_clear_fields()
		cb_save.enabled = false	
		cb_add.enabled = true
		w_maint_card.enabled = TRUE
		w_maint_card.SetRedraw(True)
		Return
	END IF
	/* beginning of new insert stuff
		verify any changed data still meets all rules
	*/
	wf_validate_data()
		
	if vib_data_ok then call w_maint_dwo `cb_save::clicked // Must be an existing id, go ahead and save
	/* What should you do if you get an error  */
END IF

If not vib_data_ok then return
// Clear the datawindow
w_maint_card.SetRedraw(False)
wf_clear_fields()
cb_delete.enabled=false
cb_add.enabled = true
cb_search.enabled = true
w_maint_card.enabled = True
w_maint_card.SetRedraw(True)

end event

type cb_delete from w_maint_dwo`cb_delete within w_maint_card
integer x = 2350
integer y = 712
integer width = 297
integer height = 92
end type

event cb_delete::clicked;this.enabled=false	
// Before calling Ancestor script determine if row can be deleted. It can't be
// deleted if its used as a recipient or has been sent Correspondence.

// Declare  SQLhost variables
	long ll_claim_no
	int	ll_nbrows

// Declare program variables
	long ll_curr_row,ll_recipient_id,  ll_rc 
	string ls_recipient_type, ls_name
w_maint_card.enabled = false
w_maint_card.SetRedraw(False)

IF MessageBox('Delete Warning','There might be Correspondence addressed to this Recipient. Do you want to continue?',Question!,YesNo!,2) = 2 THEN
	THIS.enabled = TRUE
	w_maint_card.enabled = TRUE
	w_maint_card.SetRedraw(TRUE)
	RETURN
END IF

if dw_detail.uf_accept_dw() = 1 then
	
// SET UP CHECKS FOR REFERENCES IN OTHER TABLES
		ll_curr_row 		= dw_detail.GetRow()
		ls_recipient_type	= dw_detail.GETITEMSTRING(ll_curr_row,'recipient_type_cd')
		ll_recipient_id	= vil_recipient_id
		ls_name				= dw_detail.GETITEMSTRING(ll_curr_row,'name1')




		// Check to see if recipient is set up as a potentail recipient in a claim
	 		 SELECT COUNT(RECIPIENT_LIST.claim_no) 
   	 		INTO :ll_nbrows 
    	 		FROM RECIPIENT_LIST  
   		WHERE ( RECIPIENT_LIST.correspond_recipient_id = :ll_recipient_id ) AND  
         (RECIPIENT_LIST.correspond_recipient_type_cd = :ls_recipient_type )   
		USING SQLCA;
		ll_rc = SQLCA.nf_handle_error("d_cardfile_data","w_maint_card"," cb_delete clicked during retrieve of RECIPIENT_LIST ")	
		
		If ll_nbrows > 0 then

			MessageBox("DELETE ERROR",  "CANNOT DELETE BECAUSE " +ls_name &
					+"~n~r IS SELECTED AS A RECIPIENT IN AT LEAST ONE CLAIM.",STOPSIGN!)
			wf_clear_fields()
			this.enabled=false	
			w_maint_card.enabled = true
			w_maint_card.SetRedraw(True)
			Return
		END IF

	
// CHECK to SEE IF THE RECIPIENT EXISTS SINCE THE DATA COULD HAVE CHANGED
	 
	 SELECT RECIPIENT_ADDRESS.correspond_recipient_id  
    INTO :ll_recipient_id  
    FROM RECIPIENT_ADDRESS  
   	WHERE ( RECIPIENT_ADDRESS.correspond_recipient_id = :ll_recipient_id ) AND  
         ( RECIPIENT_ADDRESS.correspond_recipient_type_cd = :ls_recipient_type ) AND  
         ( RECIPIENT_ADDRESS.card_file_flag = 'Y' ) AND  
         ( RECIPIENT_ADDRESS.active_flag = 'Y' )   ;
	ll_rc = SQLCA.nf_handle_error("d_cardfile_data ","descendant w_maint_card"," on cb_clicked during retreive of recipient")	
	If ll_rc = 100 or ll_rc < 0	 then
		
		MessageBox("RETRIVE/DELETE ERROR","CANNOT DELETE BECAUSE "+ls_name+ " "+ls_recipient_type+ &
				+"~n~r DOES NOT EXIST",STOPSIGN!)
			wf_clear_fields()
			this.enabled=false
			w_maint_card.enabled = true
			w_maint_card.SetRedraw(True)
			Return
	END IF
	IF ll_nbrows = 0 AND ll_rc = 0  then call w_maint_dwo `cb_delete::clicked // Delete only if none found

	// CHECK to SEE IF THE RECIPIENT EXISTS SINCE THE DATA COULD HAVE CHANGED
			
END IF	
w_maint_card.enabled = true
this.enabled=false	
cb_save.enabled=false
cb_search.enabled = true
cb_add.enabled = true
w_maint_card.SetRedraw(True)			

end event

type cb_add from w_maint_dwo`cb_add within w_maint_card
integer x = 2345
integer y = 568
integer width = 297
integer height = 92
end type

event cb_add::clicked;int ll_rc
string ls_name,ls_column_text_name
long  ll_nbrows,ll_recipient_id
dwitemSTATUS lb_status

w_maint_card.SetRedraw(True)

vib_data_ok = True
// ANY CHANGE to name1 or recipient type must be treated as a new record not an update
IF dw_detail.uf_datachanged() < 1 THEN
	MessageBox(parent.title,"You haven't changed anything, please re-enter.")
	dw_detail.SETFOCUS()
	Return

ElseIF dw_detail.accepttext() = -1 THEN   // only if everthing is validated else use validation error messages
	dw_detail.SETFOCUS()
	Return
END IF

ls_name = wf_check_item()
If NOT ISNULL(ls_name) THEN
	ls_column_text_name = ls_name + "_t.Text"
	ls_column_text_name = dw_detail.Describe(ls_column_text_name)
	If ls_column_text_name = "!" or ls_column_text_name = "" Then 
		ls_column_text_name = ls_name
	Else
		If right(ls_column_text_name,1) = ":" Then ls_column_text_name = left(ls_column_text_name,len(ls_column_text_name)-1)
	End IF

	vib_data_ok= False
	MESSAGEBOX(parent.title,"Invalid value for " +ls_column_text_name+".  Please enter!",Exclamation!)
	dw_detail.Setcolumn(ls_name)
	dw_detail.Setfocus()
	Return
End If

// SET UP CHECKS FOR valid data and REFERENCES IN OTHER TABLES	
wf_validate_data()
IF NOT vib_data_ok then Return
ls_name = dw_detail.GETITEMSTRING(dw_detail.GetROW(),'name1')

// get the claimant name and type and make sure they don't already exist  
SELECT COUNT(*)  
	INTO :ll_nbrows  
	FROM RECIPIENT_ADDRESS  
	WHERE ( RECIPIENT_ADDRESS.name1 = :ls_name ) AND  
			( RECIPIENT_ADDRESS.correspond_recipient_type_cd = :vis_cardfile_parms.recipient_type ) 
	USING SQLCA  ;

ll_rc = SQLCA.nf_handle_error("w_maint_dwo"," ","on clicked of cb_add")

if ll_nbrows >  0 then
	 CHOOSE CASE MessageBox("ADD",  " Recipient, "+ls_name + " in category, " +vis_cardfile_parms.recipient_type  &
		+"~n~r Already exists. Are you sure you want to add this record?",Question!,YesNo!)

		case 2
			cb_add.enabled=false
			cb_save.enabled = true
			cb_delete.enabled = true
			cb_search.enabled = true
			w_maint_card.enabled = true
			w_maint_card.SetRedraw(True)
			return
		case 1 // do nothing
	end choose			
end if

vil_recipient_id = f_get_next_rcpnt_id()	
If vil_recipient_id < 0 then vib_data_ok = false

// Set the row status to NEWMODIFIED to ensure Insert takes place
dw_detail.SETITEMSTATUS(1,0,Primary!,NewModified!)

call w_maint_dwo `cb_add::clicked //
If Not vib_data_ok Then RETURN

// reset default buttons
cb_add.enabled=true
cb_search.enabled = true
cb_delete.enabled=false
cb_save.enabled=false
w_maint_card.enabled = true
w_maint_card.SetRedraw(True)
end event

type cb_search from commandbutton within w_maint_card
integer x = 2350
integer y = 240
integer width = 297
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Searc&h"
end type

on clicked;
parent.TriggerEvent ( "ue_search" )
end on

