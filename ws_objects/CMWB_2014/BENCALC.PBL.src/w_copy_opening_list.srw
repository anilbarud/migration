$PBExportHeader$w_copy_opening_list.srw
$PBExportComments$Pop-Up Window that lists the openings that a Benefit Calculation can be copied to
forward
global type w_copy_opening_list from w_a_tool
end type
type cb_copy from commandbutton within w_copy_opening_list
end type
type cb_cancel from commandbutton within w_copy_opening_list
end type
type dw_copy_opening_list from u_dw_online within w_copy_opening_list
end type
type gb_1 from groupbox within w_copy_opening_list
end type
end forward

global type w_copy_opening_list from w_a_tool
int X=1586
int Y=1596
int Width=2725
int Height=880
WindowType WindowType=response!
boolean Resizable=false
cb_copy cb_copy
cb_cancel cb_cancel
dw_copy_opening_list dw_copy_opening_list
gb_1 gb_1
end type
global w_copy_opening_list w_copy_opening_list

type variables
//
// Variables required by this module
//

Date			idt_accident_recurrence_date

s_window_message 	istr_bencalc_parameters
end variables

on open;call w_a_tool::open;/* ---------------------------------------------------------------------------------------------------- */
/* Declare and Initialize Variables                                                                     */
/* ---------------------------------------------------------------------------------------------------- */

	DATETIME						ldtm_accident_recurrence_date

	STRING						ls_opening_type_code, ls_copy_cost_dw, ls_copy_bencalc_dw


	LONG							ll_rownum,  ll_claim_no, ll_copy_opening_no

/* Position the response window
*/
	this.x = 1900
	this.y = 600


	ls_copy_cost_dw 		= "d_copy_cost_analysis_list"
	ls_copy_bencalc_dw	= "d_copy_opening_list"


/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]
	Copy BenCalc Opening No			al_doubleparm[5]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]

	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]

	Read/Update Mode					as_mode 

	**********************************************************
*/

	istr_bencalc_parameters 	=	Message.PowerObjectParm
//
///* ---------------------------------------------------------------------------------------------------- */
///* Database Connections                                                                                 */
///* ---------------------------------------------------------------------------------------------------- */
//
//	dw_copy_opening_list.SetTransObject		(sqlca)



	ll_copy_opening_no =  istr_bencalc_parameters.al_doubleparm[5]
	IF ll_copy_opening_no = 0 THEN
		dw_copy_opening_list.DataObject = ls_copy_cost_dw
		dw_copy_opening_list.SetTransObject(SQLCA)	 
		dw_copy_opening_list.Reset()
		/* Retrieve the list of openings that a user can select from based on the claim no and opening type code 
		*/
		dw_copy_opening_list.Retrieve(istr_bencalc_parameters.al_doubleparm[1], istr_bencalc_parameters.as_stringparm[3])
	ELSE
 		dw_copy_opening_list.DataObject = ls_copy_bencalc_dw
		dw_copy_opening_list.SetTransObject(SQLCA)	 
		dw_copy_opening_list.Reset()
		/* Retrieve the list of openings that a user can select from based on the claim no, opening type code and the
			accident/recurrence date
		*/
		dw_copy_opening_list.Retrieve(istr_bencalc_parameters.al_doubleparm[1], istr_bencalc_parameters.as_stringparm[3], istr_bencalc_parameters.adtm_datetimeparm[1])
	END IF

	dw_copy_opening_list.uf_setselect(1)

/*	Highlight the last Opening in the list as the default opening to be copied to
*/
	ll_rownum = dw_copy_opening_list.RowCount()
	dw_copy_opening_list.ScrollToRow(ll_rownum)



end on

on w_copy_opening_list.create
int iCurrent
call super::create
this.cb_copy=create cb_copy
this.cb_cancel=create cb_cancel
this.dw_copy_opening_list=create dw_copy_opening_list
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_copy
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_copy_opening_list
this.Control[iCurrent+4]=this.gb_1
end on

on w_copy_opening_list.destroy
call super::destroy
destroy(this.cb_copy)
destroy(this.cb_cancel)
destroy(this.dw_copy_opening_list)
destroy(this.gb_1)
end on

type st_title from w_a_tool`st_title within w_copy_opening_list
string Text="Copy a Benefit Calculation"
end type

type cb_copy from commandbutton within w_copy_opening_list
int X=663
int Y=732
int Width=425
int Height=88
int TabOrder=30
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;// Declare Variables

W_SHEET					lwi_active_sheet

LONG						ll_opening_to_copy
DATETIME					ldtm_benefit_start
INT						li_error_code

/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]
	Copy BenCalc Opening No			al_doubleparm[5]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]

	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]

	Read/Update Mode					as_mode 

	**********************************************************
*/

/* Set up the Opening Number and the Benefit Start Date parameters from the Selected Opening.
*/

	ll_opening_to_copy = dw_copy_opening_list.GetRow()

	istr_bencalc_parameters.al_doubleparm[2]			= dw_copy_opening_list.GetItemNumber(ll_opening_to_copy,"opening_no")
	istr_bencalc_parameters.adtm_datetimeparm[1]		= dw_copy_opening_list.GetItemDateTime(ll_opening_to_copy,"accident_recurrence_date")		

   IF dw_copy_opening_list.GetItemString(ll_opening_to_copy,"recurrence_type_code") = 'R' THEN
		istr_bencalc_parameters.adtm_datetimeparm[2] = dw_copy_opening_list.GetItemDateTime(ll_opening_to_copy,"benefit_start_date")
   ELSE
// Must find the matching recurrence in order to get the benefit start date PR1890 SManzer
		Select Max(benefit_start_date)
		INTO :ldtm_benefit_start
		FROM OPENING 
		WHERE claim_no = :istr_bencalc_parameters.al_doubleparm[1] AND
				opening_type_code = :istr_bencalc_parameters.as_stringparm[3] AND
				recurrence_type_code = 'R' AND
				accident_recurrence_date = :istr_bencalc_parameters.adtm_datetimeparm[1]
		USING SQLCA;
		
		li_error_code = SQLCA.nf_handle_error("w_copy_opening_list","Embedded SQL: read OPENING","cb_copy")
		IF li_error_code < 0 THEN
			Close(Parent)
			Return
		ELSEIF li_error_code = 100 THEN
			istr_bencalc_parameters.adtm_datetimeparm[2] = dw_copy_opening_list.GetItemDateTime(ll_opening_to_copy,"benefit_start_date")
		END IF
		istr_bencalc_parameters.adtm_datetimeparm[2] = ldtm_benefit_start
	END IF

	lwi_active_sheet = w_frame.GetActiveSheet()

	OpenWithParm (lwi_active_sheet.iw_calculation_details,istr_bencalc_parameters,lwi_active_sheet)

 	Close(parent)




end event

type cb_cancel from commandbutton within w_copy_opening_list
int X=1435
int Y=732
int Width=443
int Height=88
int TabOrder=40
string Text="Cance&l"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;/* Cancel Copying Benefit Calculation
*/

close (w_copy_opening_list)
 
 


 

end on

type dw_copy_opening_list from u_dw_online within w_copy_opening_list
int X=123
int Y=184
int Width=2455
int Height=436
int TabOrder=20
string DataObject="d_copy_opening_list"
boolean Border=false
BorderStyle BorderStyle=StyleBox!
boolean VScrollBar=true
end type

on rowfocuschanged;call u_dw_online::rowfocuschanged;//

LONG	ll_rownum 

/* Highlight the row when a new row has been selected
*/

ll_rownum = GetRow()
uf_ProcessSelect(ll_rownum,"Keyboard")


 

end on

type gb_1 from groupbox within w_copy_opening_list
int X=91
int Y=124
int Width=2514
int Height=548
int TabOrder=10
string Text="Select Opening to Copy to:"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

