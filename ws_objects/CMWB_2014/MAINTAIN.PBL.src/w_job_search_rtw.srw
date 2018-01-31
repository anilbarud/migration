$PBExportHeader$w_job_search_rtw.srw
forward
global type w_job_search_rtw from window
end type
type dw_rtw_eligibility_entry from u_dw_online within w_job_search_rtw
end type
type dw_rtw_qualification_entry from u_dw_online within w_job_search_rtw
end type
type dw_rtw_qualification_list from u_dw_online within w_job_search_rtw
end type
type cb_delete_qualification from commandbutton within w_job_search_rtw
end type
type cb_add_qualification from commandbutton within w_job_search_rtw
end type
type cb_close from commandbutton within w_job_search_rtw
end type
type cb_save from commandbutton within w_job_search_rtw
end type
type cb_delete_eligibility from commandbutton within w_job_search_rtw
end type
type cb_cancel from commandbutton within w_job_search_rtw
end type
type cb_add_eligibility from commandbutton within w_job_search_rtw
end type
type gb_2 from groupbox within w_job_search_rtw
end type
type gb_1 from groupbox within w_job_search_rtw
end type
type dw_rtw_eligibility_list from u_dw_online within w_job_search_rtw
end type
end forward

global type w_job_search_rtw from window
integer width = 2930
integer height = 2628
boolean titlebar = true
string title = "Job Search Return to Work Incentive"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
boolean center = true
dw_rtw_eligibility_entry dw_rtw_eligibility_entry
dw_rtw_qualification_entry dw_rtw_qualification_entry
dw_rtw_qualification_list dw_rtw_qualification_list
cb_delete_qualification cb_delete_qualification
cb_add_qualification cb_add_qualification
cb_close cb_close
cb_save cb_save
cb_delete_eligibility cb_delete_eligibility
cb_cancel cb_cancel
cb_add_eligibility cb_add_eligibility
gb_2 gb_2
gb_1 gb_1
dw_rtw_eligibility_list dw_rtw_eligibility_list
end type
global w_job_search_rtw w_job_search_rtw

type variables
Long    il_claim_no , il_qual_no
Boolean I_Authorized_Access , ib_add_qual, ib_add_eligibility
String  is_open_ended_drug_coverage, is_status,  is_incentive_type = 'JS'
Date	  idt_termination_date
Boolean  ib_read_only = FALSE

Datastore ids_claim



N_RETURN_TO_WORK	 inv_rtw
end variables

forward prototypes
public subroutine wf_read_only ()
public function integer wf_get_rloe ()
public function long wf_get_last_qualification_no ()
public function integer wf_get_tier ()
public function integer wf_add_eligibility ()
public function integer wf_retrieve_datawindow ()
public function boolean wf_set_claim_rtw_status ()
public function boolean wf_get_incentive_status ()
end prototypes

public subroutine wf_read_only ();// wf_read_only

cb_add_qualification.Enabled = FALSE
cb_add_eligibility.enabled = FALSE
cb_delete_qualification.Enabled = FALSE
cb_delete_eligibility.enabled = FALSE
cb_cancel.Enabled = FALSE
cb_save.Enabled = FALSE

//dw_rtw_qualification_list.Enabled = FALSE 
dw_rtw_qualification_entry.Enabled = FALSE
dw_rtw_eligibility_list.Enabled = FALSE
dw_rtw_eligibility_entry.Enabled = FALSE

dw_rtw_qualification_entry.Modify("opening_no.Background.Color='553648127' opening_no.protect=1")
dw_rtw_qualification_entry.Modify("avg_monthly_capable_earning_amt.Background.Color='553648127' avg_monthly_capable_earning_amt.protect=1")
dw_rtw_qualification_entry.Modify("employment_start_date.Background.Color='553648127' employment_start_date.protect=1")

dw_rtw_eligibility_entry.Modify("tier_no.Background.Color='553648127' tier_no.protect=1")
dw_rtw_eligibility_entry.Modify("avg_monthly_employment_income_amt.Background.Color='553648127' avg_monthly_employment_income_amt.protect=1")

end subroutine

public function integer wf_get_rloe ();	DATAWINDOWCHILD	ldwc_child
	
	Long ll_opening_no

	dw_rtw_qualification_entry.GetChild('opening_no',ldwc_child)
	ldwc_child.SetTransObject(SQLCA)
	ldwc_child.Retrieve(il_claim_no)

	IF SQLCA.nf_handle_error("dw_rtw_quailifcation_entry.ldwc_child.Retrieve(il_claim_no)","n_return_to_work","nf_retrieve_rloe() - 'rloe opening'") < 0 THEN
		RETURN -1
	END IF

Return 0
end function

public function long wf_get_last_qualification_no ();
Long ll_max_qual_no

SELECT MAX(rtw_incentive_no)
INTO   :ll_max_qual_no
FROM  RTW_INCENTIVE_QUALIFICATION
WHERE claim_no = :il_claim_no
USING SQLCA;

//SQLCA...

IF IsNull(ll_max_qual_no) THEN ll_max_qual_no = 0

RETURN (ll_max_qual_no +1)


end function

public function integer wf_get_tier ();	DATAWINDOWCHILD	ldwc_tier_child
	
	Long ll_opening_no

	dw_rtw_eligibility_entry.GetChild('tier_no',ldwc_tier_child)
	ldwc_tier_child.SetTransObject(SQLCA)
	ldwc_tier_child.Retrieve(il_claim_no,il_qual_no )

	IF SQLCA.nf_handle_error("dw_rtw_eligibility_entry.ldwc_tier_child.Retrieve(il_claim_no)","n_return_to_work","tier_no'") < 0 THEN
		RETURN -1
	END IF
	

end function

public function integer wf_add_eligibility ();Long ll_row, ll_qual_row
Decimal ldec_qual_amount, ldec_eligibility_amount


ll_row = dw_rtw_eligibility_entry.getrow()
ll_qual_row = dw_rtw_qualification_list.GetRow()

ldec_qual_amount = dw_rtw_qualification_list.GetItemNumber(ll_qual_row,"avg_monthly_capable_earning_amt")
ldec_eligibility_amount = dw_rtw_eligibility_entry.GetItemNumber(ll_row,"avg_monthly_employment_income_amt")



IF ldec_qual_amount > ldec_eligibility_amount THEN
	dw_rtw_eligibility_entry.SetItem(ll_row,"payment_eligibility_flag","N")
ELSE
	dw_rtw_eligibility_entry.SetItem(ll_row,"payment_eligibility_flag","Y")
END IF

Return 0
end function

public function integer wf_retrieve_datawindow ();// wf_retrieve_datawindows
//
Long     ll_rowcount, ll_row, ll_rtn, ll_count, ll_count_tiers, ll_tier_no
Datetime ldtm_employment_start
String ls_qual_flag

cb_cancel.Enabled = FALSE
cb_save.Enabled = FALSE

wf_get_rloe()
ll_rowcount = dw_rtw_qualification_list.Retrieve(il_claim_no)
SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve")

cb_add_qualification.enabled = True

IF ll_rowcount = 0 THEN 
	dw_rtw_qualification_list.Reset()
	dw_rtw_qualification_entry.Reset()
	dw_rtw_eligibility_list.Reset()
	dw_rtw_eligibility_entry.Reset()
	il_qual_no = 1
	cb_delete_qualification.enabled = False
	cb_add_eligibility.enabled = False
	cb_delete_eligibility.enabled = False	

ELSE
	dw_rtw_qualification_list.GetSelectedRow(0)
	ll_row = dw_rtw_qualification_list.GetRow()
	dw_rtw_qualification_list.SelectRow(ll_row,True)
	
	il_qual_no = dw_rtw_qualification_list.getitemnumber(ll_row,"rtw_incentive_no")
	ldtm_employment_start = dw_rtw_qualification_list.getitemdatetime(1,"employment_start_date") //Get the very first start date (earliest)
	ls_qual_flag = dw_rtw_qualification_list.getitemstring(ll_row,"qualification_flag")

    ll_rtn = dw_rtw_qualification_entry.Retrieve(il_claim_no,il_qual_no)
	SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve")
	
	//Check to see if all 3 tiers are eligible
	Select  Count(*)
	Into     :ll_count
	From    RTW_INCENTIVE_PAYMENT_ELIGIBILITY
	Where  claim_no = :il_claim_no
	And      payment_eligibility_flag = 'Y'
	Using    SQLCA;
	SQLCA.nf_handle_error("w_job_search_rtw", "wf_retrieve_datawindow", "Count")
	
	//Get the number of active tiers
	Select   Count(*)
	Into      :ll_count_tiers
	From    Rtw_Incentive_Tier
	Where  effective_date <= :ldtm_employment_start
	And      active_flag = 'Y'
	Using    SQLCA;
	SQLCA.nf_handle_error("w_job_search_rtw", "wf_retrieve_datawindow", "Count Rtw_Incentive_Tier")
	
	IF ll_count >= ll_count_tiers OR ls_qual_flag = "N" THEN
		//disable the add eligibility buttons there are no more tiers to add OR he has not qualified don't allow a tier to be entered.
		cb_add_eligibility.enabled = False
	ELSE
		cb_add_eligibility.enabled = True
	END IF
	
	IF ls_qual_flag = "N" THEN cb_delete_qualification.enabled = True	

	wf_get_tier()
	ll_rowcount = dw_rtw_eligibility_list.Retrieve(il_claim_no,il_qual_no)
	SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve")
	
	IF ll_rowcount > 0 THEN

		cb_delete_qualification.enabled = False
		ll_row = dw_rtw_eligibility_list.GetRow()
		dw_rtw_eligibility_list.SelectRow(ll_row,True)
		ll_tier_no =  dw_rtw_eligibility_list.getitemnumber(ll_row,"tier_no")
  	    
	    ll_rtn = dw_rtw_eligibility_entry.Retrieve(il_claim_no,il_qual_no,ll_tier_no)
        SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve")

		Select count(*)
		Into    :ll_count
		From   RTW_INCENTIVE_PAYMENT_XREF
		Where claim_no = :il_claim_no
		And     rtw_incentive_type_code = 'JS'
		And     rtw_incentive_no = :il_qual_no
		And     tier_no = :ll_tier_no
		And     payment_no IS NOT NULL
		Using SQLCA;

		SQLCA.nf_handle_error("w_job_search_rtw", "dw_rtw_eligibility_entry- itemchanged", "Select From RTW_INCENTIVE_PAYMENT_XREF")
		
		IF ll_count > 0 THEN
			cb_delete_eligibility.enabled = False
		ELSE
			cb_delete_eligibility.enabled = True
		END IF	
		  
	ELSE
		dw_rtw_eligibility_list.Reset()
		dw_rtw_eligibility_entry.Reset()
		cb_delete_eligibility.enabled = False
		IF  ls_qual_flag = "N" THEN 
			cb_add_eligibility.enabled = False
		ELSE
			cb_add_eligibility.enabled = True
		END IF	
		cb_delete_qualification.enabled = True
	END IF
	
END IF

IF ib_read_only = TRUE THEN wf_read_only()

RETURN 0

end function

public function boolean wf_set_claim_rtw_status ();	Decimal ldec_employment_income
	Date      ldt_employment_start_date
	Boolean ib_status = False
	String    ls_rtw_claim_status, ls_rtw_incentive_status_code
	Long     ll_count
	
	
	ls_rtw_incentive_status_code = ids_claim.GetitemString(ids_claim.getrow(),'rtw_incentive_status_code')
	
	// See if there already is a payment eligibility, if so then don't touch status unless all three tiers are eligibile for payment.
	Select count(*)
	Into    :ll_count
	From  RTW_INCENTIVE_PAYMENT_ELIGIBILITY
	Where claim_no = :il_claim_no
	And     payment_eligibility_flag = 'Y'
	USING SQLCA;
	
	IF ll_count > 2 AND ls_rtw_incentive_status_code = 'Q' THEN
		ls_rtw_claim_status = 'C' // All three tiers are eligibile for payment the claim RTW status is Complete
		ib_status = True
	ELSE	
		ldec_employment_income = dw_rtw_qualification_entry.GetItemDecimal(dw_rtw_qualification_entry.getrow(), 'avg_monthly_capable_earning_amt')
   		ldt_employment_start_date = DATE(dw_rtw_qualification_entry.GetItemDateTime(dw_rtw_qualification_entry.getrow(),'employment_start_date'))
			
		IF ldec_employment_income > 0 AND NOT IsNull(ldt_employment_start_date) AND (ls_rtw_incentive_status_code <> 'Q' OR ls_rtw_incentive_status_code <> 'C')THEN
			ls_rtw_claim_status = 'Q' //Qualified
			ib_status = True
			Messagebox('Reminder','The Job Search Return to Work Incentive Qualification Notification should be sent to the injured worker.')
		ELSEIF ldec_employment_income > 0 AND IsNull(ldt_employment_start_date)  AND ls_rtw_incentive_status_code = ' ' THEN
			ls_rtw_claim_status = 'P' //Potential
			ib_status = True
			Messagebox('Reminder','The Job Search Qualification Notification should be sent to the injured worker.')
		END IF	
	END IF
	
	IF ib_status = True THEN
		ids_claim.SetItem(ids_claim.getrow(),'rtw_incentive_status_code',ls_rtw_claim_status)
	END IF

    Return ib_status
end function

public function boolean wf_get_incentive_status ();	Decimal ldec_employment_income
	Date      ldt_employment_start_date
	Boolean ib_status = False
	String    ls_rtw_claim_status, ls_rtw_incentive_status_code
	Long     ll_count, ll_qual_flag, ll_qual_count
	
	
	ls_rtw_incentive_status_code = ids_claim.GetitemString(ids_claim.getrow(),'rtw_incentive_status_code')
	
	//See if there is a qualification record.
	
	Select count(*)
	Into    :ll_qual_count
	From  RTW_INCENTIVE_QUALIFICATION
	Where claim_no = :il_claim_no
	USING SQLCA;
		
	SQLCA.nf_handle_error("wf_get_incentive_status","","RTW_INCENTIVE_QUALIFICATION")
	 
	IF ll_qual_count = 0 THEN
		ls_rtw_claim_status = " "
		ib_status = True
		ids_claim.SetItem(ids_claim.getrow(),'rtw_incentive_status_code',ls_rtw_claim_status)
  	    Return ib_status
	END IF		
		
	// See if there already is a payment eligibility, if so then don't touch status unless all three tiers are eligibile for payment.
	Select count(*)
	Into    :ll_count
	From  RTW_INCENTIVE_PAYMENT_ELIGIBILITY
	Where claim_no = :il_claim_no
	And     payment_eligibility_flag = 'Y'
	USING SQLCA;
	
	IF ll_count > 2 AND ls_rtw_incentive_status_code = 'Q' THEN
		ls_rtw_claim_status = 'C' // All three tiers are eligibile for payment the claim RTW status is Complete
		ib_status = True
	ELSE	
		
		Select  count(*)
		Into     :ll_qual_flag
		From    RTW_INCENTIVE_QUALIFICATION
		Where  claim_no = :il_claim_no
		And      qualification_flag = 'Y'
		USING  SQLCA;
		
		SQLCA.nf_handle_error("wf_get_incentive_status","","RTW_INCENTIVE_QUALIFICATION")
		 
		IF ll_qual_flag > 0 THEN
			IF ls_rtw_incentive_status_code <> "Q"  THEN
				ls_rtw_claim_status = 'Q' //Qualified
				ib_status = True
				Messagebox('Reminder','The Job Search Return to Work Incentive Qualification Notification should be sent to the injured worker.')
			END IF
		ELSE
			Select  count(*)
			Into     :ll_qual_flag
			From    RTW_INCENTIVE_QUALIFICATION
			Where  claim_no = :il_claim_no
			And      qualification_flag = 'N'
			And      avg_monthly_capable_earning_amt > 0 
			USING  SQLCA;
		
			SQLCA.nf_handle_error("wf_get_incentive_status","","RTW_INCENTIVE_QUALIFICATION")

			IF ll_qual_flag > 0 THEN
				IF ls_rtw_incentive_status_code <> "P"  THEN
					ls_rtw_claim_status = 'P' //Potential
					ib_status = True
					Messagebox('Reminder','The Job Search Qualification Notification should be sent to the injured worker.')
				END IF
			END IF		
		END IF
	END IF	
	
	IF ib_status = True THEN
		ids_claim.SetItem(ids_claim.getrow(),'rtw_incentive_status_code',ls_rtw_claim_status)
	END IF

    Return ib_status

end function

on w_job_search_rtw.create
this.dw_rtw_eligibility_entry=create dw_rtw_eligibility_entry
this.dw_rtw_qualification_entry=create dw_rtw_qualification_entry
this.dw_rtw_qualification_list=create dw_rtw_qualification_list
this.cb_delete_qualification=create cb_delete_qualification
this.cb_add_qualification=create cb_add_qualification
this.cb_close=create cb_close
this.cb_save=create cb_save
this.cb_delete_eligibility=create cb_delete_eligibility
this.cb_cancel=create cb_cancel
this.cb_add_eligibility=create cb_add_eligibility
this.gb_2=create gb_2
this.gb_1=create gb_1
this.dw_rtw_eligibility_list=create dw_rtw_eligibility_list
this.Control[]={this.dw_rtw_eligibility_entry,&
this.dw_rtw_qualification_entry,&
this.dw_rtw_qualification_list,&
this.cb_delete_qualification,&
this.cb_add_qualification,&
this.cb_close,&
this.cb_save,&
this.cb_delete_eligibility,&
this.cb_cancel,&
this.cb_add_eligibility,&
this.gb_2,&
this.gb_1,&
this.dw_rtw_eligibility_list}
end on

on w_job_search_rtw.destroy
destroy(this.dw_rtw_eligibility_entry)
destroy(this.dw_rtw_qualification_entry)
destroy(this.dw_rtw_qualification_list)
destroy(this.cb_delete_qualification)
destroy(this.cb_add_qualification)
destroy(this.cb_close)
destroy(this.cb_save)
destroy(this.cb_delete_eligibility)
destroy(this.cb_cancel)
destroy(this.cb_add_eligibility)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.dw_rtw_eligibility_list)
end on

event open;Long     ll_num_rows, ll_count, ll_row, ll_rowcount, ll_qual_no, ll_rtn, ll_tier_no
Integer  li_rtn
String   ls_claim_status_code, ls_term_date
Datetime ldt_death_date

s_window_message lstr_message
U_DWA ldw_dw[]

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

SetPointer(Hourglass!)

G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True   // declared as an instance variable

il_claim_no = lstr_message.al_doubleparm[1]
IF lstr_message.as_mode = "READ" THEN
	ib_read_only = TRUE
END IF

dw_rtw_qualification_list.Visible = TRUE
dw_rtw_eligibility_list.Visible = TRUE
dw_rtw_qualification_entry.Visible = TRUE
dw_rtw_eligibility_entry.Visible = TRUE

// Set the transobjects and do the retrieves
inv_rtw = Create n_return_to_work

li_rtn = dw_rtw_qualification_list.SetTransObject(SQLCA)
li_rtn = dw_rtw_qualification_entry.SetTransObject(SQLCA)
li_rtn = dw_rtw_eligibility_list.SetTransObject(SQLCA)
li_rtn = dw_rtw_eligibility_entry.SetTransObject(SQLCA)

ldw_dw[1] = dw_rtw_qualification_entry
ldw_dw[2] = dw_rtw_eligibility_entry
ldw_dw[3] = dw_rtw_qualification_list

ids_claim = create datastore
ids_claim.dataobject = 'd_claim_rtw_status'
ids_claim.settransobject(sqlca)
ids_claim.retrieve(il_claim_no)
SQLCA.nf_handle_error("w_job_search_rtw", "", "ids_claim - Retrieve")

inv_rtw.nf_set_datawindow(ldw_dw[],SQLCA)

dw_rtw_qualification_list.uf_setselect(1)
dw_rtw_eligibility_list.uf_setselect(1)

wf_retrieve_datawindow()

end event

type dw_rtw_eligibility_entry from u_dw_online within w_job_search_rtw
integer x = 96
integer y = 1872
integer width = 2674
integer height = 324
integer taborder = 70
string dataobject = "d_rtw_eligibility_entry"
boolean border = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;Long ll_count, ll_incentive_no, ll_tier_no
Decimal ldec_tier_income

cb_cancel.enabled = True
cb_delete_eligibility.enabled = False


ll_incentive_no = dw_rtw_qualification_entry.GetItemNumber(dw_rtw_qualification_entry.getrow(),"rtw_incentive_no")
ll_tier_no         = dw_rtw_eligibility_entry.GetItemNumber(dw_rtw_eligibility_entry.getrow(),"tier_no")

// A payment eligibility tier must not be changed if there is an associated payment.

Select count(*)
Into    :ll_count
From   RTW_INCENTIVE_PAYMENT_XREF
Where claim_no = :il_claim_no
And     rtw_incentive_type_code = 'JS'
And     rtw_incentive_no = :ll_incentive_no
And     tier_no = :ll_tier_no
Using SQLCA;

SQLCA.nf_handle_error("w_job_search_rtw", "dw_rtw_eligibility_entry- itemchanged", "Select From RTW_INCENTIVE_PAYMENT_XREF")

IF ll_count > 0 THEN
	MessageBox("Error","The payment eligibility can not be changed when there is an associated payment.",Exclamation!)
	Return 1
END IF

//If an eligibility is modified make sure it doesn't already exist as eligibile under a different qualification

Select count(*)
Into    :ll_count
From   RTW_INCENTIVE_PAYMENT_ELIGIBILITY
Where claim_no = :il_claim_no
And     rtw_incentive_type_code = 'JS'
And     tier_no = :ll_tier_no
And     payment_eligibility_flag = 'Y'
And     rtw_incentive_no <> :ll_incentive_no
Using SQLCA;

SQLCA.nf_handle_error("w_job_search_rtw", "dw_rtw_eligibility_entry- itemchanged", "Select From RTW_INCENTIVE_PAYMENT_XREF")

IF ll_count > 0 THEN
	MessageBox("Error","The tier eligibility can not be changed when the same tier is already eligible under a different qualification.",Exclamation!)
	Return 1
END IF





IF dwo.name = 'avg_monthly_employment_income_amt' THEN
	ldec_tier_income = Dec(data)
	IF ldec_tier_income <= 0 THEN
		MessageBox("Error","The Average Employment Income must be more than $0.00.",Exclamation!)
		Return 1
	END IF	
END IF


ib_add_eligibility = True
cb_save.enabled = True

dw_rtw_qualification_list.enabled = False
dw_rtw_qualification_entry.enabled = False
cb_add_qualification.enabled = False
cb_delete_qualification.enabled = False
cb_add_eligibility.enabled = False
cb_delete_eligibility.enabled = False






end event

event editchanged;call super::editchanged;dw_rtw_qualification_list.enabled = False
dw_rtw_qualification_entry.enabled = False

cb_add_qualification.enabled = False
cb_delete_qualification.enabled = False

cb_delete_eligibility.enabled = False
cb_cancel.enabled = True
end event

type dw_rtw_qualification_entry from u_dw_online within w_job_search_rtw
integer x = 96
integer y = 684
integer width = 2638
integer height = 324
integer taborder = 70
string dataobject = "d_rtw_qualification_entry"
boolean border = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;Long ll_row
String  ls_date
Date ldt_current_date,ldt_employment_start_date
Datetime ldtm_comparison


ldt_current_date = DATE(f_server_datetime())

// A qualification must not be changed if there is payment eligibility 

cb_cancel.enabled = True

dw_rtw_eligibility_list.enabled = False
dw_rtw_eligibility_entry.enabled = False

cb_add_eligibility.enabled = False
cb_delete_eligibility.enabled = False

cb_add_qualification.enabled = False
cb_delete_qualification.enabled = False

IF dw_rtw_eligibility_list.RowCount() > 0 THEN
	Messagebox("Error","This qualification record can not be changed when there is payment eligibility, please cancel.",Exclamation!)
	Return 1
END IF

IF dwo.name = "opening_no" THEN
	wf_get_rloe()	
END IF	

IF dwo.name = "employment_start_date"  THEN
	ls_date = LEFT(data,10)
	IF NOT IsDate(ls_date) THEN
		MessageBox("Error","Invalid employment start date.",Exclamation!)
		Return 1
	END IF
	ldt_employment_start_date = DATE(ls_date)
	//Warning employment start date more than one month
	SELECT Distinct DATEADD(MONTH,1,GetDate())
	INTO   :ldtm_comparison
	FROM   sysobjects
	USING  SQLCA;

	SQLCA.nf_handle_error("w_job_search_rtw","dw_rtw_qualification_entry","select from sysobjects")	
	
	IF ldt_employment_start_date > DATE(ldtm_comparison) THEN
		Messagebox("Warning","The Employment Start Date should not be more than one month in the future.",Exclamation!)
	END IF	
	
	//The employment start date must be on or after the benefit start date of the associated opening.
	
END IF	


cb_save.enabled = True
ib_add_qual = True

end event

event editchanged;call super::editchanged;dw_rtw_eligibility_list.enabled = False
dw_rtw_eligibility_entry.enabled = False

cb_add_eligibility.enabled = False
cb_delete_eligibility.enabled = False

cb_cancel.enabled = True
end event

type dw_rtw_qualification_list from u_dw_online within w_job_search_rtw
integer x = 64
integer y = 28
integer width = 2743
integer height = 564
integer taborder = 70
string dataobject = "d_rtw_qualification_list"
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long ll_qual_no, ll_rows, ll_rowcount, ll_row, ll_tier_no, ll_rtn, ll_count
String ls_qual_flag
	
	
IF dw_rtw_qualification_list.rowcount() > 0 THEN

	il_qual_no = dw_rtw_qualification_list.getitemnumber(currentrow,"rtw_incentive_no")
	ls_qual_flag = dw_rtw_qualification_list.getitemstring(currentrow,"qualification_flag")

    ll_rows =  dw_rtw_qualification_entry.Retrieve(il_claim_no,il_qual_no)
	IF	SQLCA.nf_handle_error("dw_rtw_qualification_list", "", "rowfocuschanged") < 0 THEN
		RETURN -1
	END IF
	IF ll_rows < 1 THEN
		MessageBox('Retrieval Error','Unable to retrieve requested qualification.')
		RETURN -1
	END IF
	
	ll_rowcount = dw_rtw_eligibility_list.Retrieve(il_claim_no,il_qual_no)
	IF SQLCA.nf_handle_error("dw_rtw_qualification_list", "", "rowfocuschanged") < 0 THEN
		RETURN -1
	END IF	
	
	IF ll_rowcount > 0 THEN
		dw_rtw_eligibility_list.GetSelectedRow(0)
		ll_row = dw_rtw_eligibility_list.GetRow()
		dw_rtw_eligibility_list.SelectRow(ll_row,True)
	
	    ll_tier_no = dw_rtw_eligibility_list.getitemnumber(ll_row,"tier_no")	
	
	    ll_rtn = dw_rtw_eligibility_entry.Retrieve(il_claim_no,il_qual_no,ll_tier_no)
        IF SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve") < 0 THEN
			RETURN -1
		END IF
		cb_delete_qualification.enabled = False
		
		Select count(*)
		Into    :ll_count
		From  RTW_INCENTIVE_PAYMENT_ELIGIBILITY
		Where claim_no = :il_claim_no
		And     payment_eligibility_flag = 'Y'
		Using SQLCA;
		
		SQLCA.nf_handle_error("dw_rtw_qualification_list", "", "rowfocuschanged")
		
		IF ll_count < 3 THEN 
			cb_add_eligibility.enabled = True 
		ELSE
			cb_add_eligibility.enabled = False
		END IF
		
		// Check to see if the eligibility record has been paid if so then you can't delete it.
		
		Select Count(*)
		Into    :ll_count
		From   RTW_INCENTIVE_PAYMENT_XREF
		Where claim_no = :il_claim_no
		And     rtw_incentive_no = :il_qual_no
		And     tier_no = :ll_tier_no
		Using SQLCA;
		
		SQLCA.nf_handle_error("dw_rtw_qualification_list", "", "rowfocuschanged")
		
		IF ll_count > 0 THEN
			cb_delete_eligibility.enabled = False
		ELSE
			cb_delete_eligibility.enabled = True
		END IF
				
	ELSE
		// There are no rows so reset dw_rtw_eligibility_entry
		
		dw_rtw_eligibility_list.Reset()
		dw_rtw_eligibility_entry.Reset()
		cb_delete_qualification.enabled = True
		cb_delete_eligibility.enabled = False
		
		IF ls_qual_flag = 'Y' THEN
			cb_add_eligibility.enabled = True
		ELSE
			cb_add_eligibility.enabled = False
		END IF	
	
	END IF
ELSE
		dw_rtw_qualification_list.Reset()
		dw_rtw_qualification_entry.Reset()
		cb_delete_qualification.enabled = False
		cb_delete_eligibility.enabled = False
		cb_add_eligibility.enabled = False
END IF

IF ib_read_only = TRUE THEN wf_read_only()
end event

event clicked;call super::clicked;setrow(row)
end event

event rowfocuschanging;call super::rowfocuschanging;IF currentrow > 0 THEN
   THIS.SelectRow(currentrow,FALSE)
END IF 
end event

event buttonclicked;call super::buttonclicked;string		ls_filter
LONG			ll_begin_rowcount
LONG			ll_end_rowcount

ll_begin_rowcount = this.rowcount()

ll_end_rowcount = this.RowCount()

IF ll_end_rowcount <> ll_begin_rowcount THen
	this.SelectRow(0,False)
	this.SelectRow(1,True)
End if
end event

type cb_delete_qualification from commandbutton within w_job_search_rtw
integer x = 686
integer y = 1064
integer width = 585
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete Qualification"
end type

event clicked;Long ll_rows, ll_rowcount, ll_row, ll_rtn
Boolean ib_status
String ls_rtw_incentive_status_code



ll_rows = dw_rtw_eligibility_list.RowCount()

IF ll_rows > 0 THEN
	MessageBox('Error','The qualification record can not be removed until the corresponding eligibility records are removed.',Exclamation!)
	Return -1
END IF

ll_rtn = Messagebox("Continue Delete?", "The qualification record will be removed.  Do you wish to continue?", Question!, YesNo!, 2)
IF ll_rtn = 2 THEN
	RETURN
END IF

SetPointer(Hourglass!)

cb_cancel.Enabled = False
cb_save.Enabled = False


dw_rtw_qualification_entry.DeleteRow(0)


SQLCA.nf_begin_transaction()

dw_rtw_qualification_entry.Update()

ib_status = wf_get_incentive_status()
IF ib_status = TRUE THEN
	ids_claim.Update()
END IF

SQLCA.nf_commit_transaction()


//get the row the user has selected
ll_rowcount = dw_rtw_qualification_list.Retrieve(il_claim_no)
IF SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve") < 0 THEN
	Return -1
END IF

IF ll_rowcount = 0 THEN 
	il_qual_no = 1
ELSE
	dw_rtw_qualification_list.GetSelectedRow(0)
	ll_row = dw_rtw_qualification_list.GetRow()
	dw_rtw_qualification_list.SelectRow(ll_row,True)
END IF	

ls_rtw_incentive_status_code = ids_claim.GetitemString(ids_claim.getrow(),'rtw_incentive_status_code')
IF ls_rtw_incentive_status_code = " "  OR ls_rtw_incentive_status_code = "P" THEN
	cb_add_eligibility.enabled = False
	cb_delete_qualification.enabled = False
END IF

wf_retrieve_datawindow()
	



end event

type cb_add_qualification from commandbutton within w_job_search_rtw
integer x = 78
integer y = 1064
integer width = 585
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Qualification"
end type

event clicked;
Long ll_row, ll_row_no, ll_opening_no, ll_rtn, ll_qual_no

ib_add_qual = True

cb_add_qualification.Enabled = FALSE
cb_delete_qualification.Enabled = FALSE
cb_add_eligibility.Enabled = FALSE
cb_delete_eligibility.Enabled = FALSE
cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE

dw_rtw_qualification_entry.Enabled = TRUE

wf_get_rloe()
dw_rtw_qualification_entry.Reset() // Reset the datawindow so that there aren't any rows; prevents scrolling
ll_row = dw_rtw_qualification_entry.InsertRow(0)
dw_rtw_qualification_entry.ScrollToRow(ll_row)

il_qual_no = wf_get_last_qualification_no()

dw_rtw_qualification_entry.SetItem(ll_row, "claim_no", il_claim_no)
dw_rtw_qualification_entry.SetItem(ll_row, "rtw_incentive_no", il_qual_no)
dw_rtw_qualification_entry.SetItem(ll_row,"rtw_incentive_type_code",is_incentive_type)
dw_rtw_qualification_entry.SetItem(ll_row,"opening_no",0)

dw_rtw_qualification_entry.SetColumn("opening_no")
dw_rtw_qualification_entry.SetFocus()

IF dw_rtw_eligibility_list.RowCount() > 0 THEN
	dw_rtw_eligibility_list.Reset()
	dw_rtw_eligibility_entry.Reset()
END IF	
end event

type cb_close from commandbutton within w_job_search_rtw
integer x = 2455
integer y = 2248
integer width = 329
integer height = 92
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Boolean lb_status

Integer 	li_rtn

IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "A change has been made and not saved yet," +&
							  " Do you want to save it?",Question!, YesNoCancel!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
//			dw_override_term_date.SetFocus()
			RETURN 1
		END IF
	ELSEIF li_rtn = 3 THEN
		RETURN 1
	ELSEIF li_rtn = 2 THEN
//		wf_retrieve_man_term_date_dw()
	END IF
END IF



CloseWithReturn(Parent,is_status)

end event

type cb_save from commandbutton within w_job_search_rtw
integer x = 2089
integer y = 2248
integer width = 329
integer height = 92
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;Integer  li_rtn, li_trancount
Long     ll_row
Date	   ldt_server_date
String   ls_rtw_claim_status
Boolean  ib_set_status

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Claim Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('008','044','save of RTW incentive',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


SetPointer(HourGlass!)


IF ib_add_qual = True THEN
	
	li_rtn = dw_rtw_qualification_entry.AcceptText()
	IF li_rtn = -1 THEN
		RETURN
	END IF
	
	IF inv_rtw.nf_check_bus_rule() < 0 THEN
		RETURN
	END IF	
	
	SQLCA.nf_begin_transaction()
	
	// Update
	li_rtn = dw_rtw_qualification_entry.Update()
	SQLCA.nf_handle_error("w_job_search_rtw", "", "cb_save - dw_list_qualification_entry.Update()")
	
END IF	

IF ib_add_eligibility = True THEN

	li_rtn = dw_rtw_eligibility_entry.AcceptText()
	IF li_rtn = -1 THEN
		RETURN
	END IF

	wf_add_eligibility()	
	
	IF inv_rtw.nf_check_bus_rule_eligibility() < 0 THEN
		RETURN
	END IF
	
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount = 0 THEN
		SQLCA.nf_begin_transaction()
	END IF
	
	// Update
	li_rtn = dw_rtw_eligibility_entry.Update()
	li_rtn = SQLCA.nf_handle_error("w_job_search_rtw", "", "cb_save - dw_list_eligiblity_entry.Update()")

	Messagebox('Reminder','The Job Search Return to Work Incentive Review Result should be sent to the injured worker.')

END IF

SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	SQLCA.nf_begin_transaction()
END IF

//ib_set_status = wf_set_claim_rtw_status()
ib_set_status = wf_get_incentive_status()
IF ib_set_status = True THEN
	ids_claim.Update()
END IF

SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount > 0 THEN
	SQLCA.nf_commit_transaction()
END IF


cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE

// Refresh DW
li_rtn = wf_retrieve_datawindow()

ib_add_eligibility = false
ib_add_qual = false
dw_rtw_qualification_list.enabled = TRUE
dw_rtw_qualification_entry.enabled = TRUE

Return li_rtn

end event

type cb_delete_eligibility from commandbutton within w_job_search_rtw
integer x = 686
integer y = 2248
integer width = 585
integer height = 92
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Delete Eligibility"
end type

event clicked;Long ll_rows, ll_rowcount, ll_row, ll_rtn
Long ll_count, ll_incentive_no, ll_tier_no
String ls_eligibility_flag
Boolean ib_status

cb_cancel.enabled = True

ll_incentive_no = dw_rtw_qualification_entry.GetItemNumber(dw_rtw_qualification_entry.getrow(),"rtw_incentive_no")
ll_tier_no         = dw_rtw_eligibility_entry.GetItemNumber(dw_rtw_eligibility_entry.getrow(),"tier_no")

// A payment eligibility tier must not be changed if there is an associated payment.

Select count(*)
Into    :ll_count
From   RTW_INCENTIVE_PAYMENT_XREF
Where claim_no = :il_claim_no
And     rtw_incentive_type_code = "JS"
And     rtw_incentive_no = :ll_incentive_no
And     tier_no = :ll_tier_no
Using SQLCA;

SQLCA.nf_handle_error("w_job_search_rtw", "dw_rtw_eligibility_entry- itemchanged", "Select From RTW_INCENTIVE_PAYMENT_XREF")

IF ll_count > 0 THEN
	MessageBox("Error","The payment eligibility can not be removed when there is an associated payment.",Exclamation!)
	Return -1
END IF

ll_rtn = Messagebox("Continue Delete?", "The eligibility record will be removed.  Do you wish to continue?", Question!, YesNo!, 2)
IF ll_rtn = 2 THEN
	RETURN
END IF

SetPointer(Hourglass!)

cb_cancel.Enabled = False
cb_save.Enabled = False

dw_rtw_eligibility_entry.DeleteRow(0)


SQLCA.nf_begin_transaction()

dw_rtw_eligibility_entry.Update()

ib_status = wf_get_incentive_status()
IF ib_status = TRUE THEN
	ids_claim.Update()
END IF

SQLCA.nf_commit_transaction()


wf_retrieve_datawindow()

end event

type cb_cancel from commandbutton within w_job_search_rtw
integer x = 1733
integer y = 2248
integer width = 329
integer height = 92
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Cancel"
end type

event clicked;
ib_add_qual = False
ib_add_eligibility = False

dw_rtw_qualification_list.enabled = True
dw_rtw_qualification_entry.enabled = True


IF wf_retrieve_datawindow() < 0 THEN
	Return -1
END IF 	
end event

type cb_add_eligibility from commandbutton within w_job_search_rtw
integer x = 78
integer y = 2248
integer width = 585
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add &Eligibility"
end type

event clicked;Long ll_row, ll_row_no, ll_opening_no, ll_rtn, ll_qual_no

ib_add_eligibility = True

cb_add_eligibility.Enabled = FALSE
cb_delete_eligibility.Enabled = FALSE
cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE

dw_rtw_eligibility_entry.Enabled = TRUE

wf_get_tier()

dw_rtw_eligibility_entry.Reset() // Reset the datawindow so that there aren't any rows; prevents scrolling
ll_row = dw_rtw_eligibility_entry.InsertRow(0)
dw_rtw_eligibility_entry.ScrollToRow(ll_row)

dw_rtw_eligibility_entry.SetItem(ll_row, "claim_no", il_claim_no)
dw_rtw_eligibility_entry.SetItem(ll_row, "rtw_incentive_no", il_qual_no)
dw_rtw_eligibility_entry.SetItem(ll_row, "rtw_incentive_type_code",is_incentive_type)

dw_rtw_eligibility_entry.SetColumn("tier_no")
dw_rtw_eligibility_entry.SetFocus()
end event

type gb_2 from groupbox within w_job_search_rtw
integer x = 64
integer y = 1804
integer width = 2743
integer height = 416
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Payment Eligibility"
end type

type gb_1 from groupbox within w_job_search_rtw
integer x = 69
integer y = 616
integer width = 2743
integer height = 416
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Qualification"
end type

type dw_rtw_eligibility_list from u_dw_online within w_job_search_rtw
integer x = 64
integer y = 1216
integer width = 2743
integer height = 564
integer taborder = 70
string dataobject = "d_rtw_eligibility_list"
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;setrow(row)
end event

event rowfocuschanged;call super::rowfocuschanged;	Long ll_qual_no, ll_rows, ll_rowcount, ll_row, ll_tier_no, ll_rtn
	Long ll_claim_no, ll_count

	
	ll_rowcount = dw_rtw_eligibility_list.RowCount()
	
	
	IF ll_rowcount > 0 THEN
	    ll_tier_no = dw_rtw_eligibility_list.getitemnumber(currentrow,"tier_no")
	   
		ll_rtn = dw_rtw_eligibility_entry.Retrieve(il_claim_no,il_qual_no,ll_tier_no)
       
		IF SQLCA.nf_handle_error("w_job_search_rtw", "", "open - Retrieve") < 0 THEN
			RETURN -1
		END IF
		
		Select count(*)
		Into    :ll_count
		From   RTW_INCENTIVE_PAYMENT_XREF
		Where claim_no = :il_claim_no
		And     rtw_incentive_type_code = 'JS'
		And     rtw_incentive_no = :il_qual_no
		And     tier_no = :ll_tier_no
		And     payment_no IS NOT NULL
		Using SQLCA;

		SQLCA.nf_handle_error("w_job_search_rtw", "dw_rtw_eligibility_entry- itemchanged", "Select From RTW_INCENTIVE_PAYMENT_XREF")
		
		IF ll_count > 0 THEN
			cb_delete_eligibility.enabled = False
		ELSE
			cb_delete_eligibility.enabled = True
		END IF	
		
	ELSE
		// There are no rows so reset dw_rtw_eligibility_entry
		dw_rtw_eligibility_list.Reset()
		dw_rtw_eligibility_entry.Reset()

	END IF

	IF ib_read_only = TRUE THEN wf_read_only()
end event

event rowfocuschanging;call super::rowfocuschanging;IF currentrow > 0 THEN
   THIS.SelectRow(currentrow,FALSE)
END IF
end event

event buttonclicked;call super::buttonclicked;string		ls_filter
LONG			ll_begin_rowcount
LONG			ll_end_rowcount

ll_begin_rowcount = this.rowcount()

ll_end_rowcount = this.RowCount()

IF ll_end_rowcount <> ll_begin_rowcount THen
	this.SelectRow(0,False)
	this.SelectRow(1,True)
End if
end event

