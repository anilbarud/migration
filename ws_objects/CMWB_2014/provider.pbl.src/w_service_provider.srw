$PBExportHeader$w_service_provider.srw
forward
global type w_service_provider from w_ancestor
end type
type tab_portal from tab within w_service_provider
end type
type tabpage_provider from userobject within tab_portal
end type
type cb_add from commandbutton within tabpage_provider
end type
type cb_cancel from commandbutton within tabpage_provider
end type
type cb_save from commandbutton within tabpage_provider
end type
type cb_bank from commandbutton within tabpage_provider
end type
type idw_provider_maintenance from u_dw_online within tabpage_provider
end type
type tabpage_provider from userobject within tab_portal
cb_add cb_add
cb_cancel cb_cancel
cb_save cb_save
cb_bank cb_bank
idw_provider_maintenance idw_provider_maintenance
end type
type tabpage_entity_registration from userobject within tab_portal
end type
type u_print_mail_pacakge from u_print_mail_package within tabpage_entity_registration
end type
type cb_cancel_invite from commandbutton within tabpage_entity_registration
end type
type cb_reprint from commandbutton within tabpage_entity_registration
end type
type cb_produce_mail_package from commandbutton within tabpage_entity_registration
end type
type dw_token from datawindow within tabpage_entity_registration
end type
type st_2 from statictext within tabpage_entity_registration
end type
type dw_registration_code from datawindow within tabpage_entity_registration
end type
type pb_portal_help from picturebutton within tabpage_entity_registration
end type
type tabpage_entity_registration from userobject within tab_portal
u_print_mail_pacakge u_print_mail_pacakge
cb_cancel_invite cb_cancel_invite
cb_reprint cb_reprint
cb_produce_mail_package cb_produce_mail_package
dw_token dw_token
st_2 st_2
dw_registration_code dw_registration_code
pb_portal_help pb_portal_help
end type
type tabpage_entity_administrator from userobject within tab_portal
end type
type dw_group_member from datawindow within tabpage_entity_administrator
end type
type tabpage_entity_administrator from userobject within tab_portal
dw_group_member dw_group_member
end type
type tab_portal from tab within w_service_provider
tabpage_provider tabpage_provider
tabpage_entity_registration tabpage_entity_registration
tabpage_entity_administrator tabpage_entity_administrator
end type
type dw_default_physio_programs from u_dw_online within w_service_provider
end type
type dw_provider_rehab_task from u_dw_online within w_service_provider
end type
type dw_next_provider_no from u_dw_online within w_service_provider
end type
type uo_search from u_service_provider_search within w_service_provider
end type
end forward

global type w_service_provider from w_ancestor
integer x = 1893
integer y = 48
integer width = 3250
integer height = 2720
string title = ""
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
tab_portal tab_portal
dw_default_physio_programs dw_default_physio_programs
dw_provider_rehab_task dw_provider_rehab_task
dw_next_provider_no dw_next_provider_no
uo_search uo_search
end type
global w_service_provider w_service_provider

type variables
n_service_provider inv_service_provider
w_sheet iw_sheet
Long    il_provider_no, il_search, il_search_provider_no
String  is_provider_type_code, is_provider_sub_type_code, is_name, is_city, is_type
Boolean ib_read_only_mode, ib_opened_read_only


u_dw_online     indw_provider_maintenance

string is_user_id


u_ds ids_token_print 



end variables

forward prototypes
public function integer wf_read_only ()
public subroutine wf_clear_identifier ()
public subroutine wf_set_provider_no (long al_provider_no, string as_provider_type_code)
public function integer wf_blow_entity_members_away (long al_entity_no)
public function integer wf_configure_tabs ()
end prototypes

public function integer wf_read_only ();/*	protect all columns of visible dw's
*/
	indw_provider_maintenance.uf_protect_allattributes(TRUE)
	dw_next_provider_no.uf_protect_allattributes(TRUE)

	dw_next_provider_no.uf_set_backcolor()
	indw_provider_maintenance.uf_set_backcolor()

/*	disable the buttons
*/
	tab_portal.tabpage_provider.cb_add.enabled = FALSE
	tab_portal.tabpage_provider.cb_cancel.enabled = FALSE
	tab_portal.tabpage_provider.cb_save.enabled = FALSE
	tab_portal.tabpage_provider.cb_bank.Enabled = FALSE

	
	wf_configure_tabs() //2014-09-02 David Worboys
	
	
Return 0
end function

public subroutine wf_clear_identifier ();/*	This function clears the current service provider.
*/
	 indw_provider_maintenance.Reset()
end subroutine

public subroutine wf_set_provider_no (long al_provider_no, string as_provider_type_code);// wf_set_provider_no - If a provider number was found, retrieve the data into the bottom dw 
//                      Otherwise, reset the bottom dw.
//
Long    ll_count
Integer li_rtn
string  ls_language 
string  ls_provider_sub_type_code
integer li_rc
LONG ll_provider_no 
long  ll_token_status_code
	
				 
il_provider_no = al_provider_no
is_provider_type_code = as_provider_type_code


IF il_provider_no > 0 OR is_provider_type_code <> '' THEN

    //set the reprint and cancel token functionality off 
    tab_portal.tabpage_entity_registration.cb_cancel_invite.enabled = false
	tab_portal.tabpage_entity_registration.cb_reprint.enabled = false
						
						
	inv_service_provider.nf_retrieve(il_provider_no, is_provider_type_code)
     ls_provider_sub_type_code = indw_provider_maintenance.getitemstring(1,'provider_sub_type_code')

	ll_provider_no = indw_provider_maintenance.getitemnumber(1,'provider_no')

     //set the default tab to the Provider tab
	 tab_portal.selectedtab = 1
		
     // check to see if entity has entity admin - show tab page if they do
     li_rc = tab_portal.tabpage_entity_administrator.dw_group_member.retrieve(indw_provider_maintenance.getitemnumber(1,'provider_no'),'P')
     SQLCA.nf_handle_error('w_service_provider','wf_set_provider_no',"dw_group_member.retrieve()")	
	
	//if there are Portal Entity Admin then we want to display that tab
	if (li_rc > 0) then
			tab_portal.tabpage_entity_administrator.enabled = true		
	else
			tab_portal.tabpage_entity_administrator.enabled = false	
	end if
	
	
	
	//check to see if they entity has been set up the builtin goup table- if they are they can receive invite token to join the web portal- show tab page if they are and the provider is active		
	 select isnull(COUNT(*),0)
	 into :ll_count
	 from Wif_Builtin_Group a
	 where a.wif_entity_type_code = 'P' and
				a.wif_entity_subtype_code = : ls_provider_sub_type_code
	 using SQLCA;
	 
     SQLCA.nf_handle_error("w_service_provider", "", "wf_set_provider_no - SELECT COUNT(*) FROM Wif_Builtin_Group")
	 

	 // if the entity is 'Active' and the Provider is set in the built in group
	 if (  indw_provider_maintenance.getitemstring(1,'active_flag') = 'Y' and  ll_count > 0 )  then

                  //produce a Paper Mail Package tab become visible in invite is allowed
		         tab_portal.tabpage_entity_registration.enabled = true			
			 
			     //retrieve all invite token for that provider
			     li_rc = tab_portal.tabpage_entity_registration.dw_token.retrieve(indw_provider_maintenance.getitemnumber(1,'provider_no'))		
			     SQLCA.nf_handle_error('w_service_provider','wf_set_provider_no',"dw_token()")	
	
	              //check to see if any token found
			     if li_rc > 0 then
			
			            //grab the first one - the pending token will be first based on sort order
	                     ll_token_status_code = tab_portal.tabpage_entity_registration.dw_token.getitemnumber(1,'token_status_code')
			
                          //if the token is pending- enable the cancel and reprint functionality			
						if  ll_token_status_code = 0 then
							tab_portal.tabpage_entity_registration.cb_cancel_invite.Enabled = true
							tab_portal.tabpage_entity_registration.cb_reprint.Enabled = true
							 tab_portal.tabpage_entity_registration.dw_token.selectrow(1,true)
						else
							 tab_portal.tabpage_entity_registration.cb_cancel_invite.Enabled = false
							 tab_portal.tabpage_entity_registration.cb_reprint.Enabled = false 
						end if
		
				         tab_portal.tabpage_entity_registration.dw_token.visible = true
				         tab_portal.tabpage_entity_registration.cb_cancel_invite.visible = true
			             tab_portal.tabpage_entity_registration.cb_reprint.visible= true           
			    else
                     tab_portal.tabpage_entity_registration.cb_cancel_invite.visible = false
			        tab_portal.tabpage_entity_registration.cb_reprint.visible= false
			        tab_portal.tabpage_entity_registration.dw_token.visible = false
			    end if
			
			
			  //insert an empty row to collect the registration code
			   if  tab_portal.tabpage_entity_registration.dw_registration_code.rowcount() = 0 then	
				    tab_portal.tabpage_entity_registration.dw_registration_code.insertrow(0)
			   end if
			
				
				tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'provider_no', indw_provider_maintenance.getitemnumber(1,'provider_no'))
				tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'provider_name', indw_provider_maintenance.getitemstring(1,'name'))
				 tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'address_line1', indw_provider_maintenance.getitemstring(1,'address_line1'))
				 tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'address_line2', indw_provider_maintenance.getitemstring(1,'address_line2'))
				 tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'location_desc2', indw_provider_maintenance.getitemstring(1,'location_desc2'))
				 tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'prov_state_code', indw_provider_maintenance.getitemstring(1,'prov_state_code'))
				 tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'country_code', indw_provider_maintenance.getitemstring(1,'country_code'))
				 tab_portal.tabpage_entity_registration.dw_registration_code.setitem(1,'postal_code', indw_provider_maintenance.getitemstring(1,'postal_code'))
			
	else
	
		 tab_portal.tabpage_entity_registration.enabled = false
	end if
		 
			 
		 
	// A service provider that is set up to receive automated payments must not be updated.
	IF (il_provider_no > 0 AND ib_read_only_mode = FALSE AND indw_provider_maintenance.RowCount() > 0) THEN
		SELECT COUNT(*) 
		  INTO :ll_count 
		  FROM App_Document_Index_Parameter 
		 WHERE service_provider_no = :il_provider_no 
		 USING imagetrans ; 
	
		li_rtn = imagetrans.nf_handle_error("w_service_provider", "", "wf_set_provider_no - SELECT COUNT(*) FROM App_Document_Index_Parameter")

		 
		IF ll_count > 0 THEN
			wf_read_only()
		ELSE									
			indw_provider_maintenance.Modify("name.protect=0 name.Background.Color='16777215'")
			indw_provider_maintenance.Modify("sort_name.protect=0 sort_name.Background.Color='16777215'")
			indw_provider_maintenance.Modify("contact_name.protect=0 contact_name.Background.Color='16777215'")
			indw_provider_maintenance.Modify("inactive_reason_desc.protect=0 inactive_reason_desc.Background.Color='16777215'")
//			idw_provider_maintenance.Modify("provider_type_code.protect=0 provider_type_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("provider_sub_type_code.protect=0 provider_sub_type_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("address_line1.protect=0 address_line1.Background.Color='16777215'")
			indw_provider_maintenance.Modify("address_line2.protect=0 address_line2.Background.Color='16777215'")
			indw_provider_maintenance.Modify("location_desc2.protect=0 location_desc2.Background.Color='16777215'")
			indw_provider_maintenance.Modify("prov_state_code.protect=0 prov_state_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("country_code.protect=0 country_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("postal_code.protect=0 postal_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("telephone_no.protect=0 telephone_no.Background.Color='16777215'")
			indw_provider_maintenance.Modify("fax_no.protect=0 fax_no.Background.Color='16777215'")
			indw_provider_maintenance.Modify("active_flag.protect=0 active_flag.Background.Color='16777215'")
			indw_provider_maintenance.Modify("nbms_early_filing_bonus_flag.protect=0 nbms_early_filing_bonus_flag.Background.Color='16777215'")
			indw_provider_maintenance.Modify("cellphone_no.protect=0 cellphone_no.Background.Color='16777215'")
			indw_provider_maintenance.Modify("email_address.protect=0 email_address.Background.Color='16777215'")
			indw_provider_maintenance.Modify("contact_name.protect=0 contact_name.Background.Color='16777215'")
			indw_provider_maintenance.Modify("service_offered_language_code.protect=0 service_offered_language_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("preferred_correspond_language_code.protect=0 preferred_correspond_language_code.Background.Color='16777215'")
			indw_provider_maintenance.Modify("physio_contract_flag.protect=0 physio_contract_flag.Background.Color='16777215'")
			indw_provider_maintenance.Modify("ephysio_flag.protect=0 ephysio_flag.Background.Color='16777215'")
			indw_provider_maintenance.Modify("hours_of_operation.protect=0 hours_of_operation.Background.Color='16777215'")
			indw_provider_maintenance.Modify("active_start_date.protect=0 active_start_date.Background.Color='16777215'")
			indw_provider_maintenance.Modify("active_end_date.protect=0 active_end_date.Background.Color='16777215'")
			indw_provider_maintenance.Modify("provider_inactive_reason_code.protect=0 provider_inactive_reason_code.Background.Color='16777215'")	
			indw_provider_maintenance.Modify("chiro_early_filing_bonus_flag.protect=0 chiro_early_filing_bonus_flag.Background.Color='16777215'")		
			
		END IF
	ELSE	
		wf_read_only()
	END IF
END IF

wf_configure_tabs() //2014-08-27 David Worboys
end subroutine

public function integer wf_blow_entity_members_away (long al_entity_no);/*
**		Type  		: Function
**		Name 		: wf_blow_entity_members_away
**		Arguments 	: long - al_enity_no
**		Returns 		:
**		Purpose		: Removes entity asociated member access //5.180,5.190 .
**		Date			: 2014/08/22
**		Author		: David Worboys
**	
**		Modifications
**    =========
**     2014-10-01 EH1 David Worboys modified to work with data verification performed in NVO
**	    2014-10-03 EH2 David Worboys added 2 more tables for associated record removal (3.210)
**    2015-01-05  EH3 David Worboys EH2 was not implmented as intended due to miscommunication  (T010946 )
*/
DATETIME ldt_Now
INTEGER li_Loop       = 0
INTEGER li_max       = 0
INTEGER li_return    = -1
STRING  ls_token_id = ""
DATASTORE ln_ds_token



IF (inv_service_provider.ib_blow_entity_members_away) THEN //2014-10-01 EH1 David Worboys, new IF statement

	ln_ds_token = CREATE DATASTORE 
	ln_ds_token.DATAOBJECT = "d_token"
	ln_ds_token.settransobject( SQLCA)
	
	li_return = ln_ds_token.RETRIEVE( al_entity_no)
	
	delete from WIF_GROUP_MEMBERS                                                                                                                                                                                       
	where WIF_GROUP_MEMBERS.wif_group_id in (select wif_group_id from WIF_GROUP where wif_entity_no = :al_Entity_No and wif_entity_type_code = 'P');
	
	li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 1","wf_blow_entity_members_away")
	
	IF (li_return = 0) THEN //Pull out WIF group
		delete from WIF_GROUP where wif_entity_no = :al_Entity_No and wif_entity_type_code = 'P' ;
		li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 2","wf_blow_entity_members_away")
		
		IF (li_return = 0) THEN //Pull out token related stuff -  (2014-03-10 EH2 David Worboys) 	
		
			li_max = ln_ds_token.ROWCOUNT()
			
			FOR li_loop = 1 TO li_max

				ls_token_id = 	ln_ds_token.OBJECT.token_id[li_loop]				
				ldt_Now = DATETIME(TODAY(),NOW())
				
				//2015-01-05 David Worboys start EH3
				UPDATE WORKFLOW_TOKEN SET Used_Datetime			= NULL,
															Cancelled_Comment = 'Provider was made inactive',
				                                                 Cancelled_Datetime = :ldt_Now 
				WHERE Token_Id = :ls_token_id AND  Token_Status_code  = '0';
				
				li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 3","wf_blow_entity_members_away")
				
				IF (li_return < 0) THEN
					EXIT
				END IF
				//2015-01-05 David Worboys end EH3
				
				/* 2015-01-05 David Worboys start EH3 - removed code block			
				delete from WORKFLOW_INVITE_GROUP where token_id = :ls_token_id ;
				li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 3","wf_blow_entity_members_away")
				
				IF (li_return = 0) THEN //Pull out WORKFLOW_INVITE
					delete from WORKFLOW_INVITE where wif_entity_no = :al_Entity_No and wif_entity_type_code = 'P';									
					li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 4","wf_blow_entity_members_away")							
					
					IF (li_return = 0) THEN //Pull out WORKFLOW tokens (2014-03-10 EH2 David Worboys) 	

						delete from WORKFLOW_TOKEN  where token_id = :ls_token_id ;
		
						li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 5","wf_blow_entity_members_away")
													
					END IF
				END IF
				
				2015-01-05 David Worboys End EH3 - removed code block */
				
			NEXT // 2014-03-10 End EH2 David Worboys	
							
		END IF
		
		IF (li_return = 0) THEN //Pull out WIF entities 
			delete from WIF_ENTITY where entity_no = :al_Entity_No and entity_type_code = 'P';			
			li_return = SQLCA.nf_handle_error("w_service_provider","Embedded SQL 4","wf_blow_entity_members_away")
		END IF
		
	END IF
		
	DESTROY ln_ds_token
	
ELSE //Did not do a thing so all good!
	li_return = 1
END IF

RETURN li_return
end function

public function integer wf_configure_tabs ();/*
**		Type  		: Function
**		Name 		: wf_configure_tabs
**		Arguments 	:
**		Returns 		: 1
**		Purpose		: Basically tab control is shotgunned throughout this obect and to prevent some unwanted problems that kept cropping up despite a number of fixes
**                         I wrote this function and called it in a number of strategic places which seems to have got the situation under control - although I have not cleaned out
**                         the original code.
**		Date			: 2014/08/27
**		Author		: David Worboys
**	
**		Modifications
*/
INTEGER li_return = 0

SETPOINTER(HOURGLASS!)

IF (w_service_provider.tag = 'search') THEN
	Tab_Portal.SelectTab ( Tab_Portal.tabpage_provider )
	Tab_Portal.tabpage_provider.Enabled 				= TRUE
	Tab_Portal.tabpage_entity_administrator.Enabled = FALSE	
	Tab_Portal.tabpage_entity_registration.Enabled 	= FALSE
END IF

IF (ib_read_only_mode) THEN //User now permitted to see these tabs but not allowed to edit data - 2014-10-22 David Worboys
	Tab_Portal.tabpage_provider.Enabled 				= TRUE //Only tab that is active but displaying data in readonly mode
	Tab_Portal.tabpage_entity_administrator.Enabled = TRUE	
	Tab_Portal.tabpage_entity_registration.Enabled 	= TRUE
	
	// 2014-10-22 David Worboys Buttons need to be disabled
	Tab_Portal.tabpage_entity_registration.cb_produce_mail_package.Enabled = FALSE
	Tab_Portal.tabpage_entity_registration.cb_reprint.Enabled						= FALSE
	Tab_Portal.tabpage_entity_registration.cb_cancel_invite.Enabled 				= FALSE
	Tab_Portal.tabpage_entity_registration.pb_portal_help.Enabled 				= FALSE
	
ELSE
	 //2014-10-22 David Worboys Buttons need to eb enabled
	Tab_Portal.tabpage_entity_registration.cb_produce_mail_package.Enabled = TRUE
	Tab_Portal.tabpage_entity_registration.cb_reprint.Enabled 						= TRUE
	Tab_Portal.tabpage_entity_registration.cb_cancel_invite.Enabled	 			= TRUE
	Tab_Portal.tabpage_entity_registration.pb_portal_help.Enabled 				= TRUE
	
	IF (Tab_Portal.Tabpage_Provider.idw_provider_maintenance.RowCount() > 0) THEN  //Make sure we have a row
		IF (Tab_Portal.Tabpage_Provider.idw_provider_maintenance.Object.active_flag[1] = 'Y') THEN //Check the active flag
		
			//Must be a medical aid provider for a physio clinic to have e registration
			IF (Trim(Tab_Portal.Tabpage_Provider.idw_provider_maintenance.Object.provider_type_code[1]) = 'M' AND &
				Trim(Tab_Portal.Tabpage_Provider.idw_provider_maintenance.Object.provider_sub_type_code[1]) = '35') THEN//5.10
			
				IF (Tab_Portal.Tabpage_Provider.idw_provider_maintenance.Object.ephysio_flag[1] = 'Y') THEN //enable regisitration only for ephysio stuff
					Tab_Portal.tabpage_entity_registration.Enabled = TRUE
				ELSE
					Tab_Portal.tabpage_entity_registration.Enabled = FALSE
				END IF
			ELSE 
				Tab_Portal.tabpage_entity_registration.Enabled = FALSE
			END IF		
		ELSE 
			Tab_Portal.tabpage_entity_registration.Enabled = FALSE
		END IF
		
		// check to see if entity has entity admin - show tab page if they do.
		 li_return = tab_portal.tabpage_entity_administrator.dw_group_member.retrieve(indw_provider_maintenance.getitemnumber(1,'provider_no'),'P')
		 SQLCA.nf_handle_error('w_service_provider','wf_set_provider_no',"dw_group_member.retrieve()")	
			
		IF (li_return > 0) THEN // Portal Entity Admin - we want to display that tab
				tab_portal.tabpage_entity_administrator.Enabled = TRUE		
		ELSE
				tab_portal.tabpage_entity_administrator.Enabled = FALSE	
		END IF
		
		Tab_Portal.Tabpage_Provider.Enabled = TRUE
		
	ELSE
		Tab_Portal.Tabpage_Provider.Enabled = FALSE
		Tab_Portal.tabpage_entity_administrator.Enabled = FALSE	
		Tab_Portal.tabpage_entity_registration.Enabled = FALSE
	END IF
END IF

RETURN 1
					
end function

event open;call super::open;u_dwa ldw_dw[]
s_window_message lstr_message
LONG ll_find, ll_provider_no
lstr_message = Message.PowerObjectParm

iw_sheet = w_frame.GetActiveSheet()

inv_service_provider = Create n_service_provider

indw_provider_maintenance = tab_portal.tabpage_provider.idw_provider_maintenance
		 
tab_portal.tabpage_entity_registration.dw_token.SetTransObject(SQLCA)		 
tab_portal.tabpage_entity_administrator.dw_group_member.SetTransObject(SQLCA)


ids_token_print = CREATE  u_ds
ids_token_print.dataobject = 'd_token_print'
ids_token_print.settransobject(sqlca)

tab_portal.tabpage_entity_registration.enabled = false  
tab_portal.tabpage_entity_administrator.enabled = false


// initialize the object
ldw_dw[1] =  indw_provider_maintenance
ldw_dw[2] = dw_next_provider_no
ldw_dw[3] = dw_provider_rehab_task
ldw_dw[4] = dw_default_physio_programs

inv_service_provider.nf_init(ldw_dw[], SQLCA, THIS)

uo_search.uf_set_parent(THIS)
IF Upper(lstr_message.as_mode) = 'READ' THEN
	wf_read_only()
	ib_opened_read_only = TRUE
	ib_read_only_mode = TRUE
ELSE
	ib_opened_read_only = FALSE
	ib_read_only_mode = FALSE
END IF

IF UpperBound(lstr_message.as_stringparm[]) > 0 THEN
	IF lstr_message.as_stringparm[1] = 'B' THEN
		ll_provider_no =	lstr_message.al_doubleparm[1]
		is_provider_type_code = lstr_message.as_stringparm[2]
		is_provider_sub_type_code = lstr_message.as_stringparm[3]
		is_name = lstr_message.as_stringparm[4]
		is_city = lstr_message.as_stringparm[5]
		is_type = lstr_message.as_stringparm[6]
		il_search = lstr_message.al_doubleparm[2]
		il_search_provider_no = lstr_message.al_doubleparm[3]
	END IF	
	uo_search.uf_set_provider(il_search_provider_no, is_provider_type_code, is_provider_sub_type_code, is_name, is_city, is_type, il_search)
	ll_find =  uo_search.dw_list.Find('provider_no=' + STRING(ll_provider_no),1, uo_search.dw_list.RowCount())
	uo_search.dw_list.ScrollToRow(ll_find)
	uo_search.dw_list.SetRow(ll_find)
END IF
end event

on close;call w_ancestor::close;
IF IsValid(inv_service_provider) THEN
	Destroy inv_service_provider
END IF
end on

event closequery;call super::closequery;LONG ll_ans

SetPointer(HourGlass!)   

//IF cb_save.enabled THEN
//   ll_ans = MessageBox("Save", 'Data needs saved.  Save?', Question!,YesNo!)
//   IF ll_ans = 1 THEN
//      cb_save.TriggerEvent(Clicked!) 
//      IF idw_provider_maintenance.ModifiedCount() > 0 THEN   
//         Message.ReturnValue = 1
//      END IF
//   END IF
//END IF
//
end event

on w_service_provider.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.tab_portal=create tab_portal
this.dw_default_physio_programs=create dw_default_physio_programs
this.dw_provider_rehab_task=create dw_provider_rehab_task
this.dw_next_provider_no=create dw_next_provider_no
this.uo_search=create uo_search
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_portal
this.Control[iCurrent+2]=this.dw_default_physio_programs
this.Control[iCurrent+3]=this.dw_provider_rehab_task
this.Control[iCurrent+4]=this.dw_next_provider_no
this.Control[iCurrent+5]=this.uo_search
end on

on w_service_provider.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.tab_portal)
destroy(this.dw_default_physio_programs)
destroy(this.dw_provider_rehab_task)
destroy(this.dw_next_provider_no)
destroy(this.uo_search)
end on

type tab_portal from tab within w_service_provider
event create ( )
event destroy ( )
integer x = 32
integer y = 972
integer width = 3209
integer height = 1548
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean powertips = true
integer selectedtab = 1
tabpage_provider tabpage_provider
tabpage_entity_registration tabpage_entity_registration
tabpage_entity_administrator tabpage_entity_administrator
end type

on tab_portal.create
this.tabpage_provider=create tabpage_provider
this.tabpage_entity_registration=create tabpage_entity_registration
this.tabpage_entity_administrator=create tabpage_entity_administrator
this.Control[]={this.tabpage_provider,&
this.tabpage_entity_registration,&
this.tabpage_entity_administrator}
end on

on tab_portal.destroy
destroy(this.tabpage_provider)
destroy(this.tabpage_entity_registration)
destroy(this.tabpage_entity_administrator)
end on

type tabpage_provider from userobject within tab_portal
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 3173
integer height = 1424
long backcolor = 67108864
string text = "Provider"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_add cb_add
cb_cancel cb_cancel
cb_save cb_save
cb_bank cb_bank
idw_provider_maintenance idw_provider_maintenance
end type

on tabpage_provider.create
this.cb_add=create cb_add
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.cb_bank=create cb_bank
this.idw_provider_maintenance=create idw_provider_maintenance
this.Control[]={this.cb_add,&
this.cb_cancel,&
this.cb_save,&
this.cb_bank,&
this.idw_provider_maintenance}
end on

on tabpage_provider.destroy
destroy(this.cb_add)
destroy(this.cb_cancel)
destroy(this.cb_save)
destroy(this.cb_bank)
destroy(this.idw_provider_maintenance)
end on

type cb_add from commandbutton within tabpage_provider
integer x = 37
integer y = 1316
integer width = 302
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;LONG ll_ans

SetPointer(HourGlass!)
inv_service_provider.nf_insert(0)

// Make sure fields are enabled
IF ib_read_only_mode = FALSE THEN
	idw_provider_maintenance.Modify("name.protect=0 name.Background.Color='16777215'")
	idw_provider_maintenance.Modify("sort_name.protect=0 sort_name.Background.Color='16777215'")
	idw_provider_maintenance.Modify("contact_name.protect=0 contact_name.Background.Color='16777215'")
	idw_provider_maintenance.Modify("inactive_reason_desc.protect=0 inactive_reason_desc.Background.Color='16777215'")
	idw_provider_maintenance.Modify("provider_type_code.protect=0 provider_type_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("provider_sub_type_code.protect=0 provider_sub_type_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("address_line1.protect=0 address_line1.Background.Color='16777215'")
	idw_provider_maintenance.Modify("address_line2.protect=0 address_line2.Background.Color='16777215'")
	idw_provider_maintenance.Modify("location_desc2.protect=0 location_desc2.Background.Color='16777215'")
	idw_provider_maintenance.Modify("prov_state_code.protect=0 prov_state_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("country_code.protect=0 country_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("postal_code.protect=0 postal_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("telephone_no.protect=0 telephone_no.Background.Color='16777215'")
	idw_provider_maintenance.Modify("fax_no.protect=0 fax_no.Background.Color='16777215'")
	idw_provider_maintenance.Modify("active_flag.protect=0 active_flag.Background.Color='16777215'")
	idw_provider_maintenance.Modify("nbms_early_filing_bonus_flag.protect=0 nbms_early_filing_bonus_flag.Background.Color='16777215'")
	idw_provider_maintenance.Modify("cellphone_no.protect=0 cellphone_no.Background.Color='16777215'")
	idw_provider_maintenance.Modify("email_address.protect=0 email_address.Background.Color='16777215'")
	idw_provider_maintenance.Modify("contact_name.protect=0 contact_name.Background.Color='16777215'")
	idw_provider_maintenance.Modify("service_offered_language_code.protect=0 service_offered_language_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("preferred_correspond_language_code.protect=0 preferred_correspond_language_code.Background.Color='16777215'")
	idw_provider_maintenance.Modify("physio_contract_flag.protect=0 physio_contract_flag.Background.Color='16777215'")
	idw_provider_maintenance.Modify("ephysio_flag.protect=0 ephysio_flag.Background.Color='16777215'")
	idw_provider_maintenance.Modify("hours_of_operation.protect=0 hours_of_operation.Background.Color='16777215'")
	idw_provider_maintenance.Modify("active_start_date.protect=0 active_start_date.Background.Color='16777215'")
	idw_provider_maintenance.Modify("active_end_date.protect=0 active_end_date.Background.Color='16777215'")
	idw_provider_maintenance.Modify("provider_inactive_reason_code.protect=0 provider_inactive_reason_code.Background.Color='16777215'")	
	idw_provider_maintenance.Modify("chiro_early_filing_bonus_flag.protect=0 chiro_early_filing_bonus_flag.Background.Color='16777215'")		
END IF

// enables sub type column
PARENT.tag = ''
idw_provider_maintenance.SetTabOrder ('provider_type_code', 10)

// Disable tabpages that are not permitted at this time
tab_portal.tabpage_entity_registration.Enabled = FALSE
tab_portal.tabpage_entity_administrator.Enabled = FALSE

cb_add.enabled = FALSE
cb_cancel.enabled = TRUE
cb_save.enabled = TRUE
uo_search.enabled = FALSE
uo_search.uf_clear_highlighting()

wf_configure_tabs() //2014-09-05 David Worboys
  
idw_provider_maintenance.SetColumn('provider_type_code')
idw_provider_maintenance.SetFocus()
end event

type cb_cancel from commandbutton within tabpage_provider
integer x = 398
integer y = 1316
integer width = 302
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;	SetPointer(HourGlass!)

	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_add.enabled = TRUE
	uo_search.enabled = TRUE

//	idw_provider_maintenance.Reset() //2014-10-05 - David Worboys, do not loose current record as requested by Dianna
    idw_provider_maintenance.ResetUpdate() //2014-10-05 - David Worboys, do not loose current record as requested by Dianna, resetting updateflags (most likely not needed)
	
	inv_service_provider.nf_insert(0) //2014-09-05 David Worboys
		
	//uo_search.dw_list.Triggerevent(rowfocuschanged!) //2014-10-05
	
	 wf_set_provider_no(il_provider_no,is_provider_type_code) //2104-10-05 - David Worboys, do not loose current record as requested by Dianna, Reload Data
	
	wf_configure_tabs()  //2014-09-05 David Worboys
	


end event

type cb_save from commandbutton within tabpage_provider
integer x = 759
integer y = 1316
integer width = 302
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Sa&ve"
end type

event clicked;INTEGER		li_rtn, li_trancount
LONG          ll_result_count
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '022' refers to the Service Provider Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('022','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

SetPointer(HourGlass!)

//idw_provider_maintenance.ACCEPTTEXT()

inv_service_provider.nf_set_commit(TRUE)

SQLCA.nf_begin_transaction() 

//2014-10-01 David Worboys - Note order of evaluation is important (Left to Right), nf_save must be before wf_blow_entity_members aways
IF (inv_service_provider.nf_save() >= 0 AND wf_blow_entity_members_away(il_provider_no)	 >= 0) THEN
						
	li_rtn = SQLCA.nf_commit_transaction()
	
	il_provider_no = idw_provider_maintenance.GetItemNumber(1,'provider_no')
	is_provider_type_code = idw_provider_maintenance.GetItemString(1,'provider_type_code')

	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_add.enabled = TRUE
	uo_search.enabled = TRUE
		
	// since the save is complete, protect the provider_type_code column
	idw_provider_maintenance.SetTabOrder ('provider_type_code', 0)
	
	//Basically only do this check if the active flag has been changed - 5.240 David Worboys
	IF (idw_provider_maintenance.Object.active_flag[1] = 'Y' AND idw_provider_maintenance.Object.ephysio_flag[1] = 'Y' ) THEN
		
		select COUNT(*) into :ll_result_count  from WIF_GROUP where wif_entity_no = :il_provider_no and wif_group_type_code = 6 ;
		
		SQLCA.nf_handle_error('w_service_provider','cb_print',"embedded SQL 2")			
	
		IF (ll_result_count = 0) THEN //5.240 Tell user an entity registration should be performed
			MessageBox("Information...","An Entity Registration must now be performed",EXCLAMATION!)
		END IF
						
	END IF
	//End 5.240 David Worbys
	
	/*	
	** Re-retrieve the provider since there would now be a new timestamp and any further 
	**	modifications to this record will cause update collission
	*/
	w_service_provider.Tag = "search" //Bogus
	//uo_search.uf_clear( ) //2014-10-05 David Worboys - Fix Cancel clearing stuff, Request by Diana
	wf_set_provider_no(il_provider_no,is_provider_type_code)
			
	idw_provider_maintenance.ResetUpdate() //2014-08-27 David Worboys 
	
	wf_Configure_Tabs() //2014-08-27 David Worboys 	
	
ELSE
	SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()	
	END IF
END IF
end event

type cb_bank from commandbutton within tabpage_provider
integer x = 2016
integer y = 1316
integer width = 603
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Maintain Bank Info"
end type

event clicked;S_WINDOW_MESSAGE lstr_window_message
W_SHEET	lw_active_sheet
LONG ll_provider_no, ll_row, ll_search, ll_search_provider_no
STRING ls_provider_type_code, ls_provider_sub_type_code, ls_name, ls_city, ls_type

//ll_row = PARENT.uo_search.dw_search.GetRow()


ll_row = w_service_provider.uo_search.dw_search.GetRow()

IF ll_row > 0 THEN	
	ll_provider_no = il_provider_no
	ll_search_provider_no = w_service_provider.uo_search.dw_search.GetItemNumber(ll_row,'provider_no')
	ls_provider_type_code = w_service_provider.uo_search.dw_search.GetItemString(ll_row, 'provider_type')
	ls_provider_sub_type_code = w_service_provider.uo_search.dw_search.GetItemString(ll_row, 'sub_type')
	ls_name = w_service_provider.uo_search.dw_search.GetItemString(ll_row, 'name')
	ls_city = w_service_provider.uo_search.dw_search.GetItemString(ll_row, 'city')
	ls_type = w_service_provider.uo_search.dw_search.GetItemString(ll_row, 'name_type')
	ll_search = w_service_provider.uo_search.dw_search.GetItemNumber(ll_row, 'search_type')
	
	IF IsNull(ll_provider_no) THEN ll_provider_no = 0
	IF IsNull(ll_search_provider_no) THEN ll_search_provider_no = 0
	IF IsNull(ls_provider_type_code) THEN ls_provider_type_code = ''
	IF IsNull(ls_provider_sub_type_code) THEN ls_provider_sub_type_code = ''
	IF IsNull(ls_name) THEN ls_name = ''
	IF IsNull(ls_city) THEN ls_city = ''
	
	IF ll_provider_no > 0 THEN
		lstr_window_message.al_doubleparm[1] =  ll_provider_no
		lstr_window_message.as_stringparm[1] = 'P'
		lstr_window_message.as_stringparm[2] = ls_provider_type_code
		lstr_window_message.as_stringparm[3] = ls_provider_sub_type_code
		lstr_window_message.as_stringparm[4] = ls_name
		lstr_window_message.as_stringparm[5] = ls_city
		lstr_window_message.as_stringparm[6] = ls_type
		lstr_window_message.al_doubleparm[2] = ll_search
		lstr_window_message.al_doubleparm[3] = ll_search_provider_no
		
		Close(w_service_provider)
		
		lw_active_sheet = w_frame.GetActiveSheet()
		
		IF IsValid (lw_active_sheet.iw_maintain_bank_information) THEN
			lw_active_sheet.iw_maintain_bank_information.il_recipient_no = lstr_window_message.al_doubleparm[1]
			lw_active_sheet.iw_maintain_bank_information.is_open = lstr_window_message.as_stringparm[1]
			lw_active_sheet.iw_maintain_bank_information.is_recipient_type_code = lstr_window_message.as_stringparm[2]
			lw_active_sheet.iw_maintain_bank_information.is_recipient_sub_type_code = lstr_window_message.as_stringparm[3]
			lw_active_sheet.iw_maintain_bank_information.is_name = lstr_window_message.as_stringparm[4]
			lw_active_sheet.iw_maintain_bank_information.is_city = lstr_window_message.as_stringparm[5]
			lw_active_sheet.iw_maintain_bank_information.il_search = lstr_window_message.al_doubleparm[2]
			lw_active_sheet.iw_maintain_bank_information.il_search_recipient_no = lstr_window_message.al_doubleparm[3]
			lw_active_sheet.iw_maintain_bank_information.uo_search.uf_search_recipient(ll_provider_no, ls_provider_type_code, ls_provider_sub_type_code, ls_name, ls_city, ll_search)
			lw_active_sheet.iw_maintain_bank_information.Show()	
		ELSE	
			OpenWithParm (lw_active_sheet.iw_maintain_bank_information,lstr_window_message,lw_active_sheet)
		END IF
	ELSE
		MessageBox('Provider Number','There was a problem getting the Provider Number for the current record.', Information!)
		RETURN
	END IF
ELSE
	MessageBox('No Records','A Provider must be selected prior to maintaing the Bank Information.', Information!)
	RETURN
END IF

end event

type idw_provider_maintenance from u_dw_online within tabpage_provider
integer width = 3186
integer height = 1316
integer taborder = 30
string dataobject = "d_provider"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;INT        li_return  	= 0	//2014-08-22 David Worboys set value
LONG		ll_Entity_No  = -1 	//2014-08-22 David Worboys
STRING 	ls_Null 				//2014-08-20 David Worboys

SetNull(ls_Null) //2014-08-20 David Worboys

uf_set_pbmessage(TRUE)
li_return = inv_service_provider.nf_change_item(1)	

IF cb_save.enabled = FALSE THEN
	IF li_return <> 1 THEN
		cb_save.enabled = TRUE
		cb_cancel.enabled = TRUE
		cb_add.enabled = FALSE
		uo_search.enabled = FALSE
	ELSE
		// prevent user from saving
		cb_cancel.enabled = TRUE
		cb_add.enabled = FALSE
		uo_search.enabled = FALSE
	END IF
END IF

//2014-08-20 David Worboys - BR2.100,2.110
CHOOSE CASE dwo.name
	CASE "country_code"
		THIS.Object.location_desc2[1] 		= ls_Null 
		THIS.Object.prov_state_code[1] 	= ls_Null 	
		THIS.Object.prov_state_code[1] 	= ls_Null 		
		THIS.Object.postal_code[1]			= ls_Null	
END CHOOSE

RETURN li_return


end event

event retrieveend;call super::retrieveend;CONSTANT STRING WHITE       = '16777215'
CONSTANT STRING TRANSPARENT = '553648127'
STRING    ls_provider_inactive_reason_code

cb_save.enabled = FALSE
cb_cancel.enabled = FALSE

IF w_service_provider.tag = 'search' THEN //2014-09-14 David Worboys, original code was PARENT.tag which broke as soon as this object was placed in a TAB control
													   //(This method is bad practice for object intercommunications)
//	idw_provider_maintenance.SetTabOrder ('provider_type_code', 0)
	//2014-09-04 David Worboys - better way to do, buth these numbers should be defined elsewhere so not so cryptic (this is white)	
	indw_provider_maintenance.Object.provider_type_code.Protect          = 1
	indw_provider_maintenance.Object.provider_type_code.Background.Color = TRANSPARENT
	
	ls_provider_inactive_reason_code = indw_provider_maintenance.Object.provider_inactive_reason_code[1]
	IF ls_provider_inactive_reason_code = '12' THEN
		ib_read_only_mode = TRUE
		wf_read_only()
	ELSE
		IF ib_opened_read_only = FALSE THEN
			ib_read_only_mode = FALSE
		END IF
	END IF
	
ELSE
//	idw_provider_maintenance.SetTabOrder ('provider_type_code', 10)
	//2014-09-04 David Worboys - better way to do, buth these numbers should be defined elsewhere so not so cryptic
	indw_provider_maintenance.Object.provider_type_code.Protect          = 0
	indw_provider_maintenance.Object.provider_type_code.Background.Color = WHITE
END IF

IF 	ib_read_only_mode = FALSE AND rowcount > 0 THEN
	cb_bank.Enabled = TRUE
END IF
end event

type tabpage_entity_registration from userobject within tab_portal
integer x = 18
integer y = 108
integer width = 3173
integer height = 1424
long backcolor = 67108864
string text = "Entity Registration"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
u_print_mail_pacakge u_print_mail_pacakge
cb_cancel_invite cb_cancel_invite
cb_reprint cb_reprint
cb_produce_mail_package cb_produce_mail_package
dw_token dw_token
st_2 st_2
dw_registration_code dw_registration_code
pb_portal_help pb_portal_help
end type

on tabpage_entity_registration.create
this.u_print_mail_pacakge=create u_print_mail_pacakge
this.cb_cancel_invite=create cb_cancel_invite
this.cb_reprint=create cb_reprint
this.cb_produce_mail_package=create cb_produce_mail_package
this.dw_token=create dw_token
this.st_2=create st_2
this.dw_registration_code=create dw_registration_code
this.pb_portal_help=create pb_portal_help
this.Control[]={this.u_print_mail_pacakge,&
this.cb_cancel_invite,&
this.cb_reprint,&
this.cb_produce_mail_package,&
this.dw_token,&
this.st_2,&
this.dw_registration_code,&
this.pb_portal_help}
end on

on tabpage_entity_registration.destroy
destroy(this.u_print_mail_pacakge)
destroy(this.cb_cancel_invite)
destroy(this.cb_reprint)
destroy(this.cb_produce_mail_package)
destroy(this.dw_token)
destroy(this.st_2)
destroy(this.dw_registration_code)
destroy(this.pb_portal_help)
end on

type u_print_mail_pacakge from u_print_mail_package within tabpage_entity_registration
boolean visible = false
integer x = 2199
integer y = 232
integer taborder = 30
end type

type cb_cancel_invite from commandbutton within tabpage_entity_registration
integer x = 1335
integer y = 1316
integer width = 955
integer height = 104
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel pending Registration"
end type

event clicked;S_WINDOW_MESSAGE lstr_message
integer li_rc

//get the information that will be passed to the cancelled popup window
lstr_message.al_doubleparm[1] =  indw_provider_maintenance.getitemnumber(1,'provider_no')
lstr_message.as_stringparm[1] = dw_token.getitemstring(dw_token.GetSelectedRow(0),"token_id")	
lstr_message.as_stringparm[2] = indw_provider_maintenance.getitemstring(1,'name') 
lstr_message.as_stringparm[3] = string(dw_token.getitemdatetime(dw_token.GetSelectedRow(0),"create_date"))
lstr_message.as_stringparm[4] = dw_token.getitemstring(dw_token.GetSelectedRow(0),"compute_4")	
lstr_message.as_stringparm[5] = string(dw_token.getitemstring(dw_token.GetSelectedRow(0),"compute_1"))	

openwithParm(w_cancel_pending_registration_comment,lstr_message)

//when the token is cancelled , we want to re-retrive the token for the provider to show the newly cancelled token
if (Message.DoubleParm = 1) then
	li_rc = tab_portal.tabpage_entity_registration.dw_token.retrieve(indw_provider_maintenance.getitemnumber(1,'provider_no'))
	SQLCA.nf_handle_error('w_service_provider','tab_portal.tabpage_2.cb_cancel_invite',"dw_token.retrieve()")
	tab_portal.tabpage_entity_registration.cb_cancel_invite.Enabled = false
	tab_portal.tabpage_entity_registration.cb_reprint.Enabled = false
end if
end event

type cb_reprint from commandbutton within tabpage_entity_registration
integer x = 82
integer y = 1304
integer width = 978
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Re-print Registration Letter"
end type

event clicked;long   ll_rc
string ls_token_id



ll_rc = dw_token.GetSelectedRow(0)

if (ll_rc < 1) then
	messagebox('Information', 'You must select an Invite Token to re-print ')
	RETURN -1
end if

//grab the token id to use to retrieve the mail package to reprint
ls_token_id = dw_token.getitemstring(ll_rc,"token_id")

u_print_mail_pacakge.uf_print_mail_package(ls_token_id) //2014-08-27 David Worboys


end event

type cb_produce_mail_package from commandbutton within tabpage_entity_registration
integer x = 791
integer y = 516
integer width = 850
integer height = 148
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Produce Registration Package"
end type

event clicked; integer li_rc
 
S_WINDOW_MESSAGE lstr_message

//These are the parmeters tyhat will be passed to the pop-up window to produce a Paper Mail Package or Registration Letter
lstr_message.al_doubleparm[1] =  indw_provider_maintenance.getitemnumber(1,'provider_no')
lstr_message.as_stringparm[1] = indw_provider_maintenance.getitemstring(1,'provider_sub_type_code') 
lstr_message.as_stringparm[2] = indw_provider_maintenance.getitemstring(1,'name') 
	
openwithParm(w_paper_mail_package,lstr_message)

//get the information passed by structure from pop-up window
lstr_message = Message.PowerObjectParm

IF (isvalid(lstr_message)) THEN //David Worboys 2014-08-18

	//if Registration Letter was successful then show success popup window
	if (lstr_message.al_doubleparm[1] = 1) then
	
	
		openwithParm(w_paper_mail_package_complete,lstr_message)
		
		//Need to retrieve the all tokens for the provider 
		li_rc = tab_portal.tabpage_entity_registration.dw_token.retrieve(indw_provider_maintenance.getitemnumber(1,'provider_no'))		
		SQLCA.nf_handle_error('w_service_provider','tab_portal.tabpage_2.cb_produce_mail_package',"dw_token.retrieve()")	
		
		//if there are tokens - then we show and enable functionality around re-print and cancelling a pending token
		if li_rc > 0 then
			  tab_portal.tabpage_entity_registration.dw_token.selectrow(1,true)
			  tab_portal.tabpage_entity_registration.dw_token.visible = true
			  tab_portal.tabpage_entity_registration.cb_cancel_invite.visible = true
			  tab_portal.tabpage_entity_registration.cb_reprint.visible= true
			  tab_portal.tabpage_entity_registration.cb_cancel_invite.enabled = true
			  tab_portal.tabpage_entity_registration.cb_reprint.enabled= true
		else
			 tab_portal.tabpage_entity_registration.dw_token.visible = false
			 tab_portal.tabpage_entity_registration.cb_cancel_invite.visible =false
			 tab_portal.tabpage_entity_registration.cb_reprint.visible= false
			 tab_portal.tabpage_entity_registration.cb_cancel_invite.enabled = false
			 tab_portal.tabpage_entity_registration.cb_reprint.enabled= false
		end if
	end if
END IF	
end event

type dw_token from datawindow within tabpage_entity_registration
integer x = 27
integer y = 692
integer width = 3077
integer height = 568
integer taborder = 60
string title = "none"
string dataobject = "d_token"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;/*	If the current row is the same as the row the user just clicked,
	a rowfocuschanges will not happen.  Therefore, force one since 
	so the current provider will be re-retrieved
*/

If This.GetRow() = row THEN
	This.TriggerEvent(rowfocuschanged!)
End IF
end event

event rowfocuschanged;LONG  ll_row
long ll_token_status_code

ll_row = THIS.GetRow()
IF ll_row > 0 THEN
	

         ll_token_status_code = this.getitemnumber(ll_row,'token_status_code')
			

		if  ll_token_status_code = 0 then
		      tab_portal.tabpage_entity_registration.cb_cancel_invite.Enabled = true
		      tab_portal.tabpage_entity_registration.cb_reprint.Enabled = true
 		
		else
			  tab_portal.tabpage_entity_registration.cb_cancel_invite.Enabled = false
			    tab_portal.tabpage_entity_registration.cb_reprint.Enabled = false
			 
		end if
	
	
		THIS.SelectRow(0, FALSE)
		THIS.SelectRow(ll_row, TRUE)		
		This.SetFocus()
END IF

end event

type st_2 from statictext within tabpage_entity_registration
integer x = 50
integer y = 1076
integer width = 2917
integer height = 148
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = " "
boolean focusrectangle = false
end type

type dw_registration_code from datawindow within tabpage_entity_registration
integer y = 24
integer width = 3081
integer height = 444
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "d_registration_code"
boolean controlmenu = true
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type pb_portal_help from picturebutton within tabpage_entity_registration
integer x = 1655
integer y = 520
integer width = 178
integer height = 140
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "Help!"
alignment htextalign = left!
string powertiptext = "See more information on Entity Admin Registration"
end type

event clicked;open(w_paper_mail_package_help)
end event

type tabpage_entity_administrator from userobject within tab_portal
integer x = 18
integer y = 108
integer width = 3173
integer height = 1424
long backcolor = 67108864
string text = "Entity Administrator"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_group_member dw_group_member
end type

on tabpage_entity_administrator.create
this.dw_group_member=create dw_group_member
this.Control[]={this.dw_group_member}
end on

on tabpage_entity_administrator.destroy
destroy(this.dw_group_member)
end on

type dw_group_member from datawindow within tabpage_entity_administrator
integer x = 9
integer y = 100
integer width = 3099
integer height = 1196
integer taborder = 30
string title = "none"
string dataobject = "d_group_member"
boolean border = false
boolean livescroll = true
end type

type dw_default_physio_programs from u_dw_online within w_service_provider
boolean visible = false
integer x = 1307
integer y = 2380
integer width = 311
integer height = 276
integer taborder = 80
string dataobject = "d_rehab_program_xref_provider_insert"
end type

type dw_provider_rehab_task from u_dw_online within w_service_provider
boolean visible = false
integer x = 1545
integer y = 2388
integer width = 105
integer height = 76
integer taborder = 70
string dataobject = "d_provider_rehab_task"
end type

type dw_next_provider_no from u_dw_online within w_service_provider
boolean visible = false
integer x = 1403
integer y = 2384
integer width = 105
integer height = 76
integer taborder = 20
string dataobject = "d_provider_no"
end type

type uo_search from u_service_provider_search within w_service_provider
integer x = 9
integer width = 3177
integer height = 956
integer taborder = 10
end type

on uo_search.destroy
call u_service_provider_search::destroy
end on

