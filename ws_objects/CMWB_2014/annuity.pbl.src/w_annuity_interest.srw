$PBExportHeader$w_annuity_interest.srw
$PBExportComments$Main processing window for the quarterly intertest processing
forward
global type w_annuity_interest from w_ancestor
end type
type dw_error from u_dw_online within w_annuity_interest
end type
type cb_cancel from commandbutton within w_annuity_interest
end type
type cb_close from commandbutton within w_annuity_interest
end type
type cb_save from commandbutton within w_annuity_interest
end type
type cb_add from commandbutton within w_annuity_interest
end type
type gb_interestdetails from groupbox within w_annuity_interest
end type
type dw_annuity_interest from u_dw_online within w_annuity_interest
end type
type dw_annuity_interest_details from u_dw_online within w_annuity_interest
end type
type st_1 from statictext within w_annuity_interest
end type
type st_2 from statictext within w_annuity_interest
end type
type st_3 from statictext within w_annuity_interest
end type
type st_4 from statictext within w_annuity_interest
end type
type cb_calculate_interest from commandbutton within w_annuity_interest
end type
type st_message from statictext within w_annuity_interest
end type
type dw_annuity_interest_results from u_dw_online within w_annuity_interest
end type
end forward

global type w_annuity_interest from w_ancestor
string title = "Quarterly Interest Process"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
event ue_print ( )
dw_error dw_error
cb_cancel cb_cancel
cb_close cb_close
cb_save cb_save
cb_add cb_add
gb_interestdetails gb_interestdetails
dw_annuity_interest dw_annuity_interest
dw_annuity_interest_details dw_annuity_interest_details
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
cb_calculate_interest cb_calculate_interest
st_message st_message
dw_annuity_interest_results dw_annuity_interest_results
end type
global w_annuity_interest w_annuity_interest

type variables
LONG  il_payment_no
LONG  il_txn_no

LONG          il_quarter
INTEGER    ii_year

DECIMAL   idcm_interest_rate

n_annuity_interest  inv_annuity_interest


end variables

forward prototypes
public function integer wf_drop_temp_tables ()
public function integer wf_create_temp_tables ()
public function integer wf_populate_temp_tables (ref date as_date_from, ref date as_date_to, ref decimal as_interest_rate, ref datetime as_current_datetime)
public function long wf_last_annuity_interest_rate ()
public function integer wf_check_dates (long al_year, integer al_quarter)
public function integer wf_return_quarter ()
end prototypes

event ue_print();dw_annuity_interest_details.Print()
end event

public function integer wf_drop_temp_tables ();string ls_sql

	// TEMP TABLES
	
	ls_sql ='DROP TABLE #CALC_INTEREST'
	EXECUTE IMMEDIATE :ls_sql using SQLCA;
	IF SQLCA.nf_handle_error('DROP table #CALC_INTEREST', 'w_annuity_interest', 'cb_calculate_interest') < 0 THEN
		Return -1
	END IF
		
	ls_sql = 'DROP TABLE #CALC_INTEREST_SUMM'
	EXECUTE IMMEDIATE :ls_sql;
	IF SQLCA.nf_handle_error('DROP table #CALC_INTEREST_SUMM ', 'w_annuity_interest', 'cb_calculate_interest') < 0 THEN
		Return -1
	END IF

	ls_sql = 'DROP TABLE #SECOND_INTEREST'
	EXECUTE IMMEDIATE :ls_sql;
	IF SQLCA.nf_handle_error('DROP table #SECOND_INTEREST ', 'w_annuity_interest', 'cb_calculate_interest') < 0 THEN
		Return -1
	END IF			 
			 
			 
	RETURN 1
end function

public function integer wf_create_temp_tables ();string ls_sql

ls_sql ='CREATE TABLE #CALC_INTEREST ( claim_no int  not null,recipient_no  int    not null,interest_amount  money  not null)'
EXECUTE IMMEDIATE :ls_sql using SQLCA;
IF SQLCA.nf_handle_error('create table #CALC_INTEREST', 'w_annuity_interest', 'cb_calculate_interest') < 0 THEN
	Return -1
END IF
	
ls_sql = 'CREATE TABLE #CALC_INTEREST_SUMM ( claim_no 	int  not null,  recipient_no int   not null,  interest_amount  money  not null,  txn_no int  not null identity(1,1))'
EXECUTE IMMEDIATE :ls_sql;
IF SQLCA.nf_handle_error('create table #CALC_INTEREST_SUMM ', 'w_annuity_interest', 'cb_calculate_interest') < 0 THEN
	Return -1
END IF

ls_sql = 'CREATE TABLE #SECOND_INTEREST ( claim_no int  not null, recipient_no  int    not null,  txn_amount  money  not null,  processed_date smalldatetime not null, interest_amount  money  not null)'
EXECUTE IMMEDIATE :ls_sql;
IF SQLCA.nf_handle_error('create table #CALC_INTEREST_SUMM ', 'w_annuity_interest', 'cb_calculate_interest') < 0 THEN
	Return -1
END IF

RETURN 1
end function

public function integer wf_populate_temp_tables (ref date as_date_from, ref date as_date_to, ref decimal as_interest_rate, ref datetime as_current_datetime);
//Part 1
INSERT INTO #CALC_INTEREST(claim_no, recipient_no, interest_amount)
SELECT b.claim_no,a.recipient_no,interest=round(CONVERT(money,sum(convert(money, ( convert(money,datediff(day,b.processed_date,:as_date_to)) / convert(money,datediff(day,dateadd(day,-1,:as_date_from),:as_date_to))) * (:as_interest_rate/100.00) * a.txn_amount))),2)
  FROM PAYMENT b , APPLIED_CLAIM_TXN a, CLAIM_PARTICIPANT c, ANNUITY_ACCOUNT d,	 ANNUITY_ELIGIBILITY e
 WHERE ( a.payment_no 				 = b.payment_no) 
   AND ( b.payment_type_code 		 = '97' ) 
	AND ( b.processed_date 			>= :as_date_from) 
	AND ( b.processed_date 			<= :as_date_to) 
	AND ( b.payment_sub_type_code <> 'IN')
	AND EXISTS (SELECT c.claim_no
    				  FROM PAYMENT c 
   				 WHERE ( c.payment_type_code = '97' ) 
						AND ( c.processed_date 	 <= :as_current_datetime) 
					   AND ( b.claim_no 			  = c.claim_no)
					 GROUP BY c.claim_no
 					HAVING sum(c.total_payment_amount +  c.adjustment_payment_amount) > 0 )
	AND c.claim_no = a.claim_no
	AND c.individual_no = a.recipient_no
	AND d.individual_no = c.individual_no
	AND (d.claim_role_code = c.claim_role_code and e.annuity_account_no = d.annuity_account_no AND b.processed_date <= e.annuity_end_date AND e.annuity_eligibility_status_code = 'A')
GROUP BY b.claim_no,a.recipient_no
UNION ALL
SELECT b.claim_no,a.recipient_no, interest=round(convert(money,sum(a.txn_amount * (:as_interest_rate/100.00))),2) 
    FROM PAYMENT b ,APPLIED_CLAIM_TXN a, ANNUITY_ACCOUNT d, ANNUITY_ELIGIBILITY c, CLAIM_PARTICIPANT e
   WHERE ( a.payment_no 	 = b.payment_no) 
   AND ( b.payment_type_code = '97' ) 
   AND ((b.processed_date < :as_date_from) OR ((b.payment_type_code  = '97' AND b.payment_sub_type_code = 'IN') AND (b.processed_date >= :as_date_from) AND (b.processed_date <= :as_date_to)))
   AND EXISTS ( SELECT c.claim_no
    		           FROM PAYMENT c 
   		          WHERE ( c.payment_type_code = '97' ) 
							AND ( c.processed_date   <= :as_current_datetime) 
							AND ( c.claim_no          = b.claim_no) 
		             GROUP BY c.claim_no
 		            HAVING sum(c.total_payment_amount + c.adjustment_payment_amount) > 0 )
AND e.individual_no = a.recipient_no
AND e.claim_no = a.claim_no
AND d.individual_no = e.individual_no
AND d.claim_role_code = e.claim_role_code						 
AND   d.annuity_account_no =c.annuity_account_no
AND   c.annuity_end_date >= a.processed_date 
AND   c.annuity_end_date >= :as_date_from  
AND   c.annuity_eligibility_status_code = 'A'	
GROUP BY b.claim_no,a.recipient_no Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest', 'cb_calculate_interest','embeded sql - retrieving interest data 3') 


INSERT INTO #CALC_INTEREST_SUMM (claim_no, recipient_no, interest_amount)
     SELECT claim_no,recipient_no,SUM(interest_amount)
       FROM #CALC_INTEREST
      group by claim_no,recipient_no 
		USING SQLCA;

SQLCA.nf_handle_error('embeded sql - retrieving interest data', 'w_annuity_interest', 'cb_calculate_interest')
		
RETURN 1
end function

public function long wf_last_annuity_interest_rate ();long	ll_last_annuity_interest_rate_no



Update Last_Annuity_Interest_Rate_No
Set last_annuity_interest_rate_no = last_annuity_interest_rate_no + 1
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_last_interest_rate','Update Last_Annuity_Interest_Rate_No')


Select last_annuity_interest_rate_no
Into    :ll_last_annuity_interest_rate_no
From Last_Annuity_Interest_Rate_No
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_last_interest_rate','Select from Last_Annuity_Interest_Rate_No')

Return ll_last_annuity_interest_rate_no

end function

public function integer wf_check_dates (long al_year, integer al_quarter);Datetime ldtm_interest_applied_date
Long ll_count, ll_year, ll_quarter_no, ll_previous_quarter, ll_previous_year, ll_current_year
int li_quarter_no

Select count(*)
Into   :ll_count
From Annuity_Interest_Rate
Where year = :al_year
And    quarter_no = :al_quarter
And   active_flag = 'Y'
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_check_dates','Select count(*) 1')

IF ll_count > 0 THEN
	MessageBox('Applied Interest Error','An active annuity interest rate has already been applied for that year and quarter.',Exclamation!)
	Return -1
END IF

//Can't enter an interest rate for future quarter 

li_quarter_no = wf_return_quarter()
ll_current_year = Year(Date(f_server_datetime()))

IF al_year > ll_current_year THEN
	MessageBox('Applied Interest Error','An annuity interest rate can not be entered for a future quarter.',Exclamation!)
	Return -1
END IF

IF al_year =  ll_current_year AND al_quarter >= li_quarter_no THEN
	MessageBox('Applied Interest Error','An annuity interest rate can not be entered for a future quarter.',Exclamation!)
	Return -1
END IF	

Select MAX(interest_applied_date)
Into   :ldtm_interest_applied_date
From Annuity_Interest_Rate
Where annuity_interest_rate_no <> 0
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_check_dates','Select MAX(interest_applied_date) 2')

Select Count(*)
Into :ll_count
From Annuity_Interest_Rate
Where interest_applied_date IS NULL
And annuity_interest_rate_no <> 0
And active_flag = 'Y'
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_check_dates','Select Count(*) 3')

IF ll_count > 0 Then 
	MessageBox('Applied Interest Error','An annuity interest has not been applied. Please apply before adding another quarter.',Exclamation!)
	Return -1
END IF

Select Max(year)
Into :ll_year
From Annuity_Interest_Rate
Where active_flag = 'Y'
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_check_dates','')

Select Max(quarter_no)
Into    :ll_quarter_no
From Annuity_Interest_Rate
Where year = :ll_year
And active_flag = 'Y'
Using SQLCA;

SQLCA.nf_handle_error('w_annuity_interest','wf_check_dates','Select Max(quarter_no)')


IF al_quarter = 1 then 
	ll_previous_quarter = 4
	ll_previous_year = al_year - 1
ELSE
	ll_previous_quarter = al_quarter - 1
	ll_previous_year = al_year
END IF

IF ll_year <> ll_previous_year THEN
	MessageBox('Interest Rate Error',"The previous quarter's interest rate does not exist, please enter it first.",Exclamation!)
	Return -1
ELSEIF ll_quarter_no <> ll_previous_quarter THEN
	MessageBox('Interest Rate Error',"The previous quarter's interest rate does not exist, please enter it first.",Exclamation!)
	Return -1
END IF

	


Return 1
end function

public function integer wf_return_quarter ();
int li_quarter_no,li_month
date ldt_current_date

ldt_current_date = Date(f_server_datetime())

li_month = Month(ldt_current_date)

IF li_month >= 1 and li_month <=3 THEN li_quarter_no = 1

IF li_month >= 4 and li_month <=6 THEN li_quarter_no = 2

IF li_month >= 7 and li_month <=9 THEN li_quarter_no = 3

IF li_month >= 9 and li_month <=12 THEN li_quarter_no = 4


Return li_quarter_no
end function

event open;call super::open;LONG		  ll_rows, ll_row
U_DWA   	  ldw_dw[]
INTEGER    li_year, li_quarter_no
datetime   ldtm_interest_applied_date


 st_message.text = ' '
		
inv_annuity_interest = Create n_annuity_interest
inv_annuity_interest.nf_set_window_parent(THIS)

dw_annuity_interest_details.settransobject(sqlca)
dw_annuity_interest_results.settransobject(sqlca)


ldw_dw[1] = dw_annuity_interest

inv_annuity_interest.nf_init(ldw_dw[],SQLCA,THIS)

ll_rows = dw_annuity_interest_details.retrieve()


if ll_rows >= 1 then
	li_year       = dw_annuity_interest_details.getitemnumber(1,'year')
	li_quarter_no = dw_annuity_interest_details.getitemnumber(1,'quarter_no')
   ll_row        = dw_annuity_interest.retrieve(li_year,li_quarter_no)
	if ll_row > 0 then
		dw_annuity_interest_details.SelectRow(1,True)
		ldtm_interest_applied_date = dw_annuity_interest.getitemdatetime(dw_annuity_interest.getrow(),'interest_applied_date')
		
		if isnull(ldtm_interest_applied_date) then
			dw_annuity_interest.enabled = TRUE
			cb_calculate_interest.enabled = TRUE
		else
			dw_annuity_interest.enabled = FALSE
			cb_calculate_interest.enabled = FALSE
		end if
	end if
end if





	
end event

on w_annuity_interest.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.dw_error=create dw_error
this.cb_cancel=create cb_cancel
this.cb_close=create cb_close
this.cb_save=create cb_save
this.cb_add=create cb_add
this.gb_interestdetails=create gb_interestdetails
this.dw_annuity_interest=create dw_annuity_interest
this.dw_annuity_interest_details=create dw_annuity_interest_details
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.cb_calculate_interest=create cb_calculate_interest
this.st_message=create st_message
this.dw_annuity_interest_results=create dw_annuity_interest_results
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_error
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.cb_close
this.Control[iCurrent+4]=this.cb_save
this.Control[iCurrent+5]=this.cb_add
this.Control[iCurrent+6]=this.gb_interestdetails
this.Control[iCurrent+7]=this.dw_annuity_interest
this.Control[iCurrent+8]=this.dw_annuity_interest_details
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.cb_calculate_interest
this.Control[iCurrent+14]=this.st_message
this.Control[iCurrent+15]=this.dw_annuity_interest_results
end on

on w_annuity_interest.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_error)
destroy(this.cb_cancel)
destroy(this.cb_close)
destroy(this.cb_save)
destroy(this.cb_add)
destroy(this.gb_interestdetails)
destroy(this.dw_annuity_interest)
destroy(this.dw_annuity_interest_details)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.cb_calculate_interest)
destroy(this.st_message)
destroy(this.dw_annuity_interest_results)
end on

event close;call super::close;
If IsValid(inv_annuity_interest) THEN
	DESTROY inv_annuity_interest
END IF

end event

type dw_error from u_dw_online within w_annuity_interest
boolean visible = false
integer x = 59
integer y = 2496
integer width = 1147
integer height = 360
integer taborder = 110
string dataobject = "d_error_report_positive"
end type

type cb_cancel from commandbutton within w_annuity_interest
integer x = 704
integer y = 2332
integer width = 274
integer height = 100
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;LONG      ll_rows
INTEGER   li_year, li_quarter_no, ll_row
DATETIME  ldtm_interest_applied_date

ll_rows = dw_annuity_interest_details.retrieve()

/* defult the buttons just in case */
cb_add.enabled  = TRUE
cb_save.enabled = FALSE

/* reset the bottom datawindow */
dw_annuity_interest.reset()

if ll_rows >= 1 then
	li_year       = dw_annuity_interest_details.getitemnumber(1,'year')
	li_quarter_no = dw_annuity_interest_details.getitemnumber(1,'quarter_no')
   ll_row        = dw_annuity_interest.retrieve(li_year,li_quarter_no)
	if ll_row > 0 then
		dw_annuity_interest_details.SelectRow(1,True)
		ldtm_interest_applied_date = dw_annuity_interest.getitemdatetime(dw_annuity_interest.getrow(),'interest_applied_date')
		
		if isnull(ldtm_interest_applied_date) then
			dw_annuity_interest.enabled   = TRUE
			cb_calculate_interest.enabled = TRUE
			this.enabled                  = TRUE
		else
			dw_annuity_interest.enabled   = FALSE
			cb_calculate_interest.enabled = FALSE
		end if
	end if
end if





	
end event

type cb_close from commandbutton within w_annuity_interest
event clicked pbm_bnclicked
integer x = 2208
integer y = 2332
integer width = 274
integer height = 100
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;/*	Reset the dw's, then call the function to reset the dw's and drop downs.
*/
	Close(w_annuity_interest)
	
end event

type cb_save from commandbutton within w_annuity_interest
integer x = 411
integer y = 2332
integer width = 274
integer height = 100
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;long     ll_row, ll_item, ll_sort_row,ll_year, ll_last_annuity_rate_no
decimal  ldcm_interest_rate
integer   li_quarter_no, li_rtn
string   ls_criteria

SetPointer(HourGlass!)

LONG				ll_count, ll_current_row
dwItemStatus 	l_status

/*	Check the business rules for the Annuity Interest. Perform consistency and mandatory validations
*/
IF dw_annuity_interest.accepttext() = -1 THEN RETURN
ll_current_row = dw_annuity_interest.GetRow()


//Check for an existing interest record that has not be applied.
//Select interest_applied_date 
//From Annuity_Interest_Rate
//Where year =
//And quarter_no = 1

ll_year = dw_annuity_interest.GetItemNumber(ll_current_row, "year")

if isnull(ll_year) OR trim(string(ll_year)) = "" then
	MessageBox('Invalid Year','You must enter a valid year')	
	dw_annuity_interest.SetFocus()
	dw_annuity_interest.SetColumn('year')
	Return 
end if
	

if ll_year < 1982 then
	MessageBox('Invalid Year','Year must be greater than 1981')	
	dw_annuity_interest.SetFocus()
	dw_annuity_interest.SetColumn('year')
	Return 
end if
		
if ll_year > 2050 then
	MessageBox('Invalid Year','Year must be less than 2050')	
	dw_annuity_interest.SetFocus()
	dw_annuity_interest.SetColumn('year')
	Return 
end if
	
li_quarter_no = dw_annuity_interest.GetItemNumber(ll_current_row, "quarter_no")

if isnull(li_quarter_no) OR trim(string(li_quarter_no)) = "" then
	MessageBox('Invalid quarter','You must enter a quarter')	
	dw_annuity_interest.SetFocus()
	dw_annuity_interest.SetColumn('quarter_no')
	Return 
end if

if li_quarter_no < 1 or li_quarter_no > 4 then
	MessageBox('Invalid Quarter No','Quarter must be 1,2,3, or 4')	
	dw_annuity_interest.SetFocus()
	dw_annuity_interest.SetColumn('quarter_no')
	Return 
end if
		
li_rtn = wf_check_dates(ll_year, li_quarter_no)
IF li_rtn < 0 THEN
	Return 
END IF
		
ldcm_interest_rate = dw_annuity_interest.GetItemNumber(ll_current_row, "annuity_interest_rate")

if ldcm_interest_rate < -25 or ldcm_interest_rate > 100 or IsNull(ldcm_interest_rate) then
	MessageBox('Invalid interest rate','Interest Rate must be between -25% and 100%')	
	dw_annuity_interest.SetFocus()
	dw_annuity_interest.SetColumn('annuity_interest_rate')
	Return 
end if
		
l_status = dw_annuity_interest.GetItemStatus(ll_current_row, 0, Primary!)
if l_status = New! or l_status = NewModified! then
  SELECT count(*) 
    INTO :ll_count
	 FROM Annuity_Interest_Rate  
   WHERE ( Annuity_Interest_Rate.year       = :ll_year ) 
	  AND	( Annuity_Interest_Rate.quarter_no = :li_quarter_no)   
   using SQLCA;
	
   SQLCA.nf_handle_error('Select count','w_annuity_interest', 'cb_save')
		
	if ll_count > 0 then
		MessageBox('Duplicate','Year/Quarter already exist' )	
		dw_annuity_interest.SetFocus()
		dw_annuity_interest.SetColumn('year')
		Return 
	end if		
end if


SQLCA.nf_begin_transaction()

//Get the last rate no
ll_last_annuity_rate_no = wf_last_annuity_interest_rate()

dw_annuity_interest.SetItem(ll_current_row,'annuity_interest_rate_no',ll_last_annuity_rate_no)

dw_annuity_interest.update()
SQLCA.nf_handle_error('update','w_annuity_interest', 'cb_save')

SQLCA.nf_commit_transaction()


cb_save.enabled = FALSE
cb_add.enabled  = TRUE

dw_annuity_interest_details.Retrieve()
dw_annuity_interest.Retrieve(ll_year,li_quarter_no)
SQLCA.nf_handle_error('retrieve','w_annuity_interest', 'cb_save')

ls_criteria =  "year = " + string(ll_year) + " and quarter_no = " + string(li_quarter_no)
ll_row = dw_annuity_interest_details.Find( ls_criteria,1, dw_annuity_interest_details.RowCount() )

IF ll_row = 0 THEN ll_row = 1

dw_annuity_interest_details.ScrollToRow( ll_row )
dw_annuity_interest_details.SelectRow( ll_row, True )

cb_calculate_interest.enabled = True

/*
ll_year            = dw_annuity_interest.getitemnumber(1,'year')
li_quarter_no      = dw_annuity_interest.getitemnumber(1,'quarter_no')
ldcm_interest_rate = dw_annuity_interest.getitemDECIMAL(1,'interest_rate')

ls_criteria =  "year = " + string(ll_year) + " and quarter_no = " + string(li_quarter_no)
		
ll_row = dw_annuity_interest_details.Find( ls_criteria,ll_row, dw_annuity_interest_details.RowCount() )
	
IF ll_row > 0 THEN
		// Row found, scroll to it and make it current
		dw_annuity_interest_details.setitem(ll_row,'interest_rate',ldcm_interest_rate)
		dw_annuity_interest_details.ScrollToRow( ll_row )	
	else
		dw_annuity_interest_details.visible = false
		ll_item = dw_annuity_interest_details.insertrow(0)
		dw_annuity_interest_details.setitem(ll_item,'year',ll_year)
		dw_annuity_interest_details.setitem(ll_item,'quarter_no',li_quarter_no)
		dw_annuity_interest_details.setitem(ll_item,'interest_rate',ldcm_interest_rate)
		ls_criteria =  "year = " + string(ll_year) + " and quarter_no = " + string(li_quarter_no)
		dw_annuity_interest_details.sort()
		ll_sort_row = dw_annuity_interest_details.Find( ls_criteria,ll_row, dw_annuity_interest_details.RowCount() )
		
		if ll_sort_row > 0 then dw_annuity_interest_details.ScrollToRow(ll_sort_row)
	
		dw_annuity_interest_details.visible = true
	END IF

*/

end event

type cb_add from commandbutton within w_annuity_interest
integer x = 128
integer y = 2332
integer width = 274
integer height = 100
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;LONG		ll_row

dw_annuity_interest.enabled = TRUE
dw_annuity_interest.reset()

//ll_row = inv_annuity_interest.nf_insert(1) 
dw_annuity_interest.insertrow(0)
dw_annuity_interest.scrolltorow(ll_row)
dw_annuity_interest.setfocus()
dw_annuity_interest.settaborder('year',10)
dw_annuity_interest.settaborder('quarter_no',20)
dw_annuity_interest.settaborder('annuity_interest_rate',30)

dw_annuity_interest.setcolumn('year')

cb_save.enabled               = TRUE
cb_cancel.enabled             = TRUE
cb_add.enabled                = FALSE
cb_calculate_interest.enabled = FALSE

end event

type gb_interestdetails from groupbox within w_annuity_interest
integer x = 160
integer y = 1540
integer width = 2363
integer height = 760
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Interest Details"
end type

type dw_annuity_interest from u_dw_online within w_annuity_interest
integer x = 183
integer y = 1632
integer width = 2304
integer height = 604
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_annuity_interest_rate"
borderstyle borderstyle = styleraised!
end type

event itemfocuschanged;/* Override ancestor script to have edits on edit mask fields work correctly.
*/
end event

event itemerror;return 0
end event

event editchanged;call super::editchanged;cb_save.enabled   = TRUE
cb_cancel.enabled = true
cb_add.enabled    = FALSE
end event

type dw_annuity_interest_details from u_dw_online within w_annuity_interest
integer x = 192
integer y = 428
integer width = 2290
integer height = 1068
integer taborder = 30
string dataobject = "d_annuity_interest_rate_details"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;integer  li_year, li_quarter_no
long     ll_row
datetime ldtm_interest_applied_date

this.SelectRow (0,False)

dw_annuity_interest.RESET()

if currentrow >= 1 then
	st_message.text = ' '
	li_year         = this.getitemnumber(currentrow,'year')
	li_quarter_no   = this.getitemnumber(currentrow,'quarter_no')
   ll_row          = dw_annuity_interest.retrieve(li_year,li_quarter_no)
	if ll_row > 0 then
		dw_annuity_interest_details.SelectRow(currentrow,True)
		ldtm_interest_applied_date = dw_annuity_interest.getitemdatetime(dw_annuity_interest.getrow(),'interest_applied_date')
		
		dw_annuity_interest.settaborder('year',0)
		dw_annuity_interest.settaborder('quarter_no',0)
			
		if isnull(ldtm_interest_applied_date) then
			dw_annuity_interest.enabled   = TRUE
			cb_calculate_interest.enabled = TRUE
			
			dw_annuity_interest.settaborder('annuity_interest_rate',10)
			dw_annuity_interest.setcolumn('annuity_interest_rate')
		else
			dw_annuity_interest.enabled   = FALSE
			cb_calculate_interest.enabled = FALSE
			dw_annuity_interest.settaborder('annuity_interest_rate',0)
		end if
	end if
end if

cb_add.enabled = TRUE








end event

type st_1 from statictext within w_annuity_interest
integer x = 315
integer y = 232
integer width = 951
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Quarter 1:  Jan. 1  thru Mar. 31"
boolean focusrectangle = false
end type

type st_2 from statictext within w_annuity_interest
integer x = 315
integer y = 320
integer width = 965
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Quarter 2:  Apr. 1 thru Jun. 30"
boolean focusrectangle = false
end type

type st_3 from statictext within w_annuity_interest
integer x = 1440
integer y = 232
integer width = 933
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Quarter 3:  Jul.1  thru Sept. 30"
boolean focusrectangle = false
end type

type st_4 from statictext within w_annuity_interest
integer x = 1440
integer y = 320
integer width = 942
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Quarter 4:  Oct.1  thru Dec. 31"
boolean focusrectangle = false
end type

type cb_calculate_interest from commandbutton within w_annuity_interest
integer x = 1143
integer y = 2332
integer width = 558
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Interest Calculation"
end type

event clicked;date 	   ldt_quarter_start_date,	ldt_quarter_end_date,ld_current_date
string   ls_sql, ls_criteria
integer  li_response, li_year, li_quarter_no, li_rc,li_previous_quarter_no,li_previous_year, li_rtn
decimal  ldcm_interest_rate,ldcm_transaction_sum
long 		ll_rows, ll_row, ll_payment_no_to,ll_payment_no_from,ll_count,ll_batch_no,ll_payment_count
LONG     ll_unapplied_count,ll_check1,ll_check2
datetime ldt_scheduled_processing_date,ldt_null_date
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '024' refers to the Quarterly Annuity Interest module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('024','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


setpointer(HourGlass!)
setnull(ldt_null_date)
ld_current_date = date(f_server_datetime())

li_response = MessageBox("Warning", "process may be time & resource consuming and other CMWB user should be considered", Exclamation!, OKCancel!, 2)
ldt_scheduled_processing_date = f_server_datetime()

IF li_response <> 1 THEN 
	st_message.text            = ' '
	cb_close.enabled           = true
//   w_annuity_interest.enabled = true
	Messagebox("abort","You have cancelled the process")
	Return
END IF

IF cb_save.enabled = TRUE THEN
	MessageBox("Save/Cancel", "Please save or cancel the current action before processing this quarter")
	RETURN
END IF 

li_year            = dw_annuity_interest.getitemnumber(dw_annuity_interest.getrow(),'year')
li_quarter_no      = dw_annuity_interest.getitemnumber(dw_annuity_interest.getrow(),'quarter_no')
ldcm_interest_rate = dw_annuity_interest.getitemdecimal(dw_annuity_interest.getrow(),'annuity_interest_rate')

li_response = Messagebox("Attention","You are about to calculate interest for the year: " + string(li_year) + " quarter: " + string(li_quarter_no) + " rate: " + string(ldcm_interest_rate),Exclamation!, OKCancel!, 2) 

IF li_response <> 1 THEN 
	st_message.text            = ' '
	cb_close.enabled           = true
//   w_annuity_interest.enabled = true
	Messagebox("abort","You have cancelled the process")
	return
END IF

st_message.text = 'BEGIN PROCESS'
		 	
/* check to make sure that previous quarter has been applied */
if li_quarter_no = 1 then 
	li_previous_year       = li_year - 1 
	li_previous_quarter_no = 4
else
	li_previous_year       = li_year
	li_previous_quarter_no = li_quarter_no - 1
end if
	
if li_quarter_no = 1 and li_year = 1993 then
else
		
	SELECT count(*)  
	  INTO :ll_count
	  FROM Annuity_Interest_Rate  
	 WHERE year                  = :li_previous_year  
	   AND quarter_no            = :li_previous_quarter_no  
	   AND interest_applied_date is not null    
	 USING SQLCA;
			
	SQLCA.nf_handle_error("w_annuity_interest","cb_calculate_intestest"," dw_annuity_summarize_payments") 
				
	if ll_count <= 0 then
		Messagebox("ERROR","Previous Quartely Interest has not been applied - Process Aborted",StopSign!)
		RETURN -1
	end if
end if 


	/* setup the date range for the quarter being processed */
	CHOOSE CASE li_quarter_no
		CASE 1
			ldt_quarter_start_date = date(li_year,1,1)
			ldt_quarter_end_date = date(li_year,3,31)
		CASE 2
			ldt_quarter_start_date = date(li_year,4,1)
			ldt_quarter_end_date = date(li_year,6,30)
		CASE 3
			ldt_quarter_start_date = date(li_year,7,1)
			ldt_quarter_end_date = date(li_year,9,30)	
	   CASE 4
			ldt_quarter_start_date = date(li_year,10,1)
			ldt_quarter_end_date = date(li_year,12,31)
	END CHOOSE
	
	/* check to make sure that the quarter selected is not the current or in the future*/
	IF ldt_quarter_end_date > Date(ldt_scheduled_processing_date) THEN
		Messagebox("ERROR","The Quarter you have selected is either the current quarter, or in the future.~r~nPlease select a Quarter that is in the past.",StopSign!)
		RETURN -1
	END IF
		
	// End powerbuilder's initial transaction
	st_message.text = 'PROCESSING'

	
SQLCA.nf_begin_transaction()
			
	/* disable the window */
	//w_annuity_interest.enabled = false 
		
	// set transaction level Read Uncommited
	ls_sql = 'set transaction isolation level read uncommitted'
	EXECUTE IMMEDIATE :ls_sql USING SQLCA;
	SQLCA.nf_handle_error('set transaction isolation level read uncommitted', 'w_annuity_interest', 'cb_calculate_interest') 
			
	st_message.text = 'CREATING TEMP TABLES'
		
	/* can only return a 1 or the application will crash with no return */	
	wf_create_temp_tables()

	st_message.text = 'POPULATING TEMP TABLES'
	wf_populate_temp_tables(ldt_quarter_start_date,ldt_quarter_end_date,ldcm_interest_rate,ldt_scheduled_processing_date)
	
     // start the transactions
     st_message.text = 'UPDATING TABLES - CLAIM'

			
	ls_sql = 'select last_payment_no from Last_Payment_No (TABLOCKX)'
	EXECUTE IMMEDIATE  :ls_sql USING SQLCA;
	SQLCA.nf_handle_error('sselect last_payment_no from Last_Payment_No (TABLOCKX)', 'w_annuity_interest', 'cb_calculate_interest') 
					
	ls_sql = 'SELECT last_txn_no  FROM Last_Claim_Txn_No  (TABLOCKX)'
			
	EXECUTE IMMEDIATE  :ls_sql USING SQLCA;
	SQLCA.nf_handle_error('SELECT last_txn_no  FROM Last_Claim_Txn_No  (TABLOCKX)', 'w_annuity_interest', 'cb_calculate_interest') 
				
   /* temporary change until we decide where the batch_no will come from */
	ls_sql = 'SELECT last_batch_no  FROM Last_Batch_No  (TABLOCKX)'
			
	EXECUTE IMMEDIATE  :ls_sql USING SQLCA;
	SQLCA.nf_handle_error('SELECT last_batch_no  FROM Last_Batch_No  (TABLOCKX)', 'w_annuity_interest', 'cb_calculate_interest') 
	
// get the last payment no & last txn no

	Select last_payment_no into :ll_payment_no_from 
	  from Last_Payment_No using SQLCA;
	 SQLCA.nf_handle_error('SELECT last_txn_no  FROM Last_PAYMENT_No ', 'w_annuity_interest', 'cb_calculate_interest') 
				
	ll_payment_no_from  = ll_payment_no_from + 1
	
// grab the last_batch_no 	
	Select last_batch_no into :ll_batch_no 
	  from Last_Batch_No using SQLCA;
	 SQLCA.nf_handle_error('SELECT last_batch_no  FROM Last_Batch_No ', 'w_annuity_interest', 'cb_calculate_interest') 
				
	ll_batch_no  = ll_batch_no + 1
	
// create the interest payments (applied claim txn)

	st_message.text = 'CREATING INTEREST PAYMENT - CLAIM '
	INSERT  INTO CLAIM..PAYMENT  ( payment_no,   
			claim_no,             opening_no,                benefit_calculation_no, 
			award_no,             payment_type_code,         payment_sub_type_code,   
			final_payment_flag,   paid_days_lost,            paid_hours_lost,
		   paid_quantity,        paid_from_date,				 paid_to_date,
			total_award_amount,   total_deductions,          tax_amount,          
			                      adjustment_days_lost,      adjustment_hours_lost,     
			adjustment_quantity,  adjustment_tax_amount,     adjustment_payment_amount, 
			processed_date,       loe_explanation,           payment_adjustment_flag,   
			authorized_by_code,   authorized_date,           authorization_no,          
			submitted_amount,tax_rate)  
			
	select a.txn_no + b.last_payment_no, 
			a.claim_no, 			 0, 								 0,
			0,							 '97',							 'IN',
			'N',						 0,								 0,
		   0, 	                :ldt_quarter_start_date,   :ldt_quarter_end_date,
			a.interest_amount,	 0,                         0,
			                      0,                         0,							 
			0,                    0,							    0,								 
			NULL,                 ' ',						       'N',								 
			:vgst_user_profile.user_id, :ld_current_date,	0,								 
			0,                     0  
 	  FROM #CALC_INTEREST_SUMM a,CLAIM..Last_Payment_No b USING SQLCA;
			
		IF SQLCA.SQLCode = -1 THEN
			SQLCA.nf_handle_error('	ERROR INSERTING INTO PAYMENT', 'w_annuity_interest', 'cb_calculate_interest') 
			SQLCA.nf_rollback_transaction()
			Return -1
		END IF
		
		/* put the record count into the first check */
		ll_check1 = SQLCA.SQLNRows
		
		st_message.text = 'CREATING INTEREST TXNS - CLAIM'
					
		INSERT INTO CLAIM..UNAPPLIED_CLAIM_TXN  ( txn_no,   
			claim_no,   			    payment_no,  					 txn_type_code, 
			txn_sub_type_code,	    batch_no,   					 recipient_no,   
			recipient_type_code,     recipient_sub_type_code,   coc_period,      
			manual_cheque_req_no,    cheque_no,   					 direct_deposit_xmit_no,  
			payment_method_code,     tax_amount,					 txn_amount,   
			admin_region_code,       scheduled_processing_date, explanation,     
			related_txn_no,          recipient_name,   			 address_line1,   
			address_line2,   		    city,   						 prov_state_code,   
			country,   				    postal_code,   				 use_default_address_flag,   
			cheque_print_group_code, txn_unit_of_work_no,       maintain_allowed_flag)          
		select   a.txn_no + c.last_txn_no ,
			a.claim_no,              a.txn_no + b.last_payment_no, '1',
			' ',                     :ll_batch_no,                 a.recipient_no,
			'I',                     ' ',                          0,
			0,                       0,                            0,
			"I",                     0,                            a.interest_amount,
			d.admin_region_code ,    :ld_current_date,       ' ',
			0,                       e.given_names + " " + e.last_name,e.address_line1 ,
			e.address_line2,         e.city,                      e.prov_state_code,
			f.location_desc1,        e.postal_code,                "Y",
			' ',                     0,                            'N'
		FROM #CALC_INTEREST_SUMM a,Last_Payment_No b, Last_Claim_Txn_No c ,CLAIM d ,INDIVIDUAL e , Location f
		WHERE a.claim_no           = d.claim_no 
		  AND a.recipient_no      = e.individual_no
		  AND e.country_code       = f.location_code
        AND f.location_type_code = 'C'  
		  AND f.location_code      <> '' 
		USING SQLCA;
		
		IF SQLCA.SQLCode = -1 THEN
			SQLCA.nf_handle_error('	ERROR INSERTING INTO UNAPPLIED_CLAIM_TXN', 'w_annuity_interest', 'cb_calculate_interest') 
			SQLCA.nf_rollback_transaction()
			Return -1
		END IF
		
	/* put the record count into the second check */
	ll_check2 = SQLCA.SQLNRows
	
	/* check the record counts */
	IF ll_check1 <> ll_check2 THEN
		SQLCA.SQLDBCode = -1 
		SQLCA.nf_handle_error('Transaction counts do not match - UNAPPLIED_CLAIM_TXN and PAYMENT - CHECK 1', 'w_annuity_interest', 'cb_calculate_interest') 
	END IF 
		
	//grab a count of the number of transactions that we are going to batch
	SELECT count(*), sum(interest_amount)  INTO :ll_unapplied_count, :ldcm_transaction_sum 
	FROM #CALC_INTEREST_SUMM USING SQLCA;
	
	SQLCA.nf_handle_error('	ERROR SELECT count(*) INTO :ll_unapplied_count', 'w_annuity_interest', 'cb_calculate_interest') 

	/* here we want to ensure that the record counts all equal each other */
	IF ll_unapplied_count <> ll_check1 THEN
		SQLCA.SQLDBCode = -1 
		SQLCA.nf_handle_error('Transaction counts do not match - UNAPPLIED_CLAIM_TXN and PAYMENT - CHECK 2', 'w_annuity_interest', 'cb_calculate_interest') 
	END IF 
		
	//insert a record into the TXN_BATCH_CONTROL table.
	INSERT INTO CLAIM..TXN_BATCH_CONTROL  
		(batch_no,                 batch_type_code,                 admin_region_code,
	   user_batch_flag,           number_txns_batched,             number_txns_processed, 	 
		number_txns_rejected,      number_txns_del_after_processed, txn_amount_batched,      
		txn_amount_processed,      txn_amount_rejected,             txn_amount_del_after_processed,
		scheduled_processing_date, processed_date 
		)          
	SELECT  :ll_batch_no,     "F"  ,    							  " ",   
	   "N",                   :ll_unapplied_count,             0,     
		0,                     0,                               :ldcm_transaction_sum,
		0,                     0,	                             0,
		:ld_current_date,             :ldt_null_date 
	 USING SQLCA;
	
	SQLCA.nf_handle_error('	ERROR INSERTING INTO CLAIM..TXN_BATCH_CONTROL', 'w_annuity_interest', 'cb_calculate_interest') 
						
	// update the last payment_no and last_txn_no tables
		st_message.text = 'UPDATING last txn and payment no - CLAIM'
		
		update CLAIM..Last_Payment_No
		   set last_payment_no = last_payment_no  + (select count(*) from #CALC_INTEREST_SUMM) using SQLCA; 
			
			SQLCA.nf_handle_error('ERROR UPDATING Last Payment No','w_annuity_interest','cb_calculate_interest') 
				
		update CLAIM..Last_Claim_Txn_No  
			set last_txn_no = last_txn_no + (select count(*) from #CALC_INTEREST_SUMM) USING SQLCA;
			
			SQLCA.nf_handle_error('ERROR UPDATING Last Claim Txn No','w_annuity_interest','cb_calculate_interest') 
		
		update CLAIM..Last_Batch_No  
			set last_batch_no = :ll_batch_no USING SQLCA;
			
			SQLCA.nf_handle_error('ERROR UPDATING Last_Batch_No','w_annuity_interest','cb_calculate_interest') 
			
		st_message.text = 'Updating Annuity Interest - CLAIM'

		UPDATE Annuity_Interest_Rate 
     		SET interest_applied_date = :ldt_scheduled_processing_date
   	 WHERE Annuity_Interest_Rate.year = :li_year 
			AND Annuity_Interest_Rate.quarter_no = :li_quarter_no 
			AND Annuity_Interest_Rate.active_flag = 'Y' 
		 USING SQLCA ; 

		SQLCA.nf_handle_error('w_annuity_interest', '', 'cb_calculate_interest - UPDATE Annuity_Interest_Rate SET interest_applied_date') 
			
		/* grab the maximum PAYMENT  number */
		SELECT last_payment_no INTO :ll_payment_no_to FROM Last_Payment_No USING SQLCA;
			
		SQLCA.nf_handle_error('counting payment transactions','w_annuity_interest','cb_calculate_interest') 
			
		st_message.text = 'Committing Work'

		  
		IF SQLCA.SQLCode = -1 THEN
			SQLCA.nf_handle_error('	ERROR Committing to SQLCA - ', 'w_annuity_interest', 'cb_calculate_interest') 
			SQLCA.nf_rollback_transaction()
			Return -1
		END IF
			
		st_message.text = 'DROPING TEMP TABLES'
				
		li_rc = wf_drop_temp_tables()

		if li_rc <> -1 then	
					
				
			ll_rows = dw_annuity_interest_results.retrieve( ll_payment_no_from, ll_payment_no_to,li_year, li_quarter_no,ldcm_interest_rate)
			SQLCA.nf_handle_error('ERROR ', 'w_annuity_interest', 'dw_annuity_interest_result') 
							
			if ll_rows > 0 then
				st_message.text = 'CREATING REPORT'
				dw_annuity_interest_results.print()
			end if
			
			ll_rows = dw_annuity_interest_details.retrieve()	
			SQLCA.nf_handle_error('s ', 'w_annuity_interest', 'dw_annuity_interest_details') 
					
			ll_rows = dw_annuity_interest.retrieve(li_year, li_quarter_no)
			SQLCA.nf_handle_error('s ', 'w_annuity_interest', 'dw_annuity_interest')
					
				
			/*If the user enters a Negative Interest Rate and calculates the Interest.
			  If the Interest Calculated is a Positive Txn Amount, the transaction is 
			  created and  the Error given to the user in Print Form

           If the user enters a Positive Interest Rate and calculates the Interest
           If the Interest Calculated is a Negative Txn Amount, Create the Txn and Report the Error
			*/
			IF ldcm_interest_rate > 0 THEN 
				dw_error.DataObject = 'd_error_report_positive'	
			ELSEIF ldcm_interest_rate < 0 THEN
				dw_error.DataObject = 'd_error_report_negative'
			ELSE//= 0 should have transactions <> 0
				dw_error.DataObject = 'd_error_report_zero'
			END IF
				
			dw_error.settransobject(sqlca)
			dw_error.retrieve(li_year, li_quarter_no,ldcm_interest_rate,ldt_quarter_start_date,ldt_quarter_end_date)
			SQLCA.nf_handle_error('dw_error_report.retrieve()','w_annuity_interest', 'cb_calculate_interest')
					
			/* if there are rows then print the report */
			IF dw_error.rowcount() > 0 THEN 	dw_error.print()
					
			if ll_rows > 0 then
					
				ls_criteria =  "year = " + string(li_year) + " and quarter_no = " + string(li_quarter_no)
					
				ll_row = dw_annuity_interest_details.Find( ls_criteria,ll_rows, dw_annuity_interest_details.RowCount() )
				
				IF ll_row > 0 THEN dw_annuity_interest_details.scrolltorow(ll_row)
			end if
			
			EXECUTE IMMEDIATE 'set transaction isolation level read committed' USING SQLCA;
			SQLCA.nf_handle_error('set transaction isolation level read uncommitted', 'w_annuity_interest', 'cb_calculate_interest') 
				
		end if

SQLCA.nf_commit_transaction()


	st_message.text = 'PROCESS FINISHED'
cb_close.enabled = true 
cb_calculate_interest.Enabled = FALSE 

end event

type st_message from statictext within w_annuity_interest
integer x = 206
integer y = 44
integer width = 2286
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean enabled = false
long bordercolor = 255
boolean focusrectangle = false
end type

type dw_annuity_interest_results from u_dw_online within w_annuity_interest
boolean visible = false
integer x = 1367
integer y = 1724
integer width = 1120
integer height = 360
integer taborder = 10
string dataobject = "d_annuity_interest_results"
end type

event itemchanged;call super::itemchanged;STRING	ls_column_name
INTEGER	li_return_code

ls_column_name = THIS.GetColumnName()
This.AcceptText()
li_return_code = inv_annuity_interest.nf_change_item(1)
cb_save.enabled = true
IF li_return_code = 1 THEN
	RETURN 1
END IF


end event

event itemfocuschanged;/* Override ancestor script to have edits on edit mask fields work correctly.
*/
end event

