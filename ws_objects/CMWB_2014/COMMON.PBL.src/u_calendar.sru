$PBExportHeader$u_calendar.sru
forward
global type u_calendar from userobject
end type
type dw_daily from datawindow within u_calendar
end type
type dw_42 from u_dw_cal_day within u_calendar
end type
type dw_41 from u_dw_cal_day within u_calendar
end type
type dw_40 from u_dw_cal_day within u_calendar
end type
type dw_39 from u_dw_cal_day within u_calendar
end type
type dw_38 from u_dw_cal_day within u_calendar
end type
type dw_37 from u_dw_cal_day within u_calendar
end type
type dw_36 from u_dw_cal_day within u_calendar
end type
type st_7 from statictext within u_calendar
end type
type r_1 from rectangle within u_calendar
end type
type st_6 from statictext within u_calendar
end type
type st_5 from statictext within u_calendar
end type
type st_4 from statictext within u_calendar
end type
type st_3 from statictext within u_calendar
end type
type st_2 from statictext within u_calendar
end type
type st_1 from statictext within u_calendar
end type
type dw_35 from u_dw_cal_day within u_calendar
end type
type dw_34 from u_dw_cal_day within u_calendar
end type
type dw_33 from u_dw_cal_day within u_calendar
end type
type dw_32 from u_dw_cal_day within u_calendar
end type
type dw_31 from u_dw_cal_day within u_calendar
end type
type dw_30 from u_dw_cal_day within u_calendar
end type
type dw_29 from u_dw_cal_day within u_calendar
end type
type dw_28 from u_dw_cal_day within u_calendar
end type
type dw_27 from u_dw_cal_day within u_calendar
end type
type dw_26 from u_dw_cal_day within u_calendar
end type
type dw_25 from u_dw_cal_day within u_calendar
end type
type dw_24 from u_dw_cal_day within u_calendar
end type
type dw_23 from u_dw_cal_day within u_calendar
end type
type dw_22 from u_dw_cal_day within u_calendar
end type
type dw_21 from u_dw_cal_day within u_calendar
end type
type dw_20 from u_dw_cal_day within u_calendar
end type
type dw_19 from u_dw_cal_day within u_calendar
end type
type dw_18 from u_dw_cal_day within u_calendar
end type
type dw_17 from u_dw_cal_day within u_calendar
end type
type dw_16 from u_dw_cal_day within u_calendar
end type
type dw_15 from u_dw_cal_day within u_calendar
end type
type dw_14 from u_dw_cal_day within u_calendar
end type
type dw_13 from u_dw_cal_day within u_calendar
end type
type dw_12 from u_dw_cal_day within u_calendar
end type
type dw_11 from u_dw_cal_day within u_calendar
end type
type dw_10 from u_dw_cal_day within u_calendar
end type
type dw_9 from u_dw_cal_day within u_calendar
end type
type dw_8 from u_dw_cal_day within u_calendar
end type
type dw_7 from u_dw_cal_day within u_calendar
end type
type dw_6 from u_dw_cal_day within u_calendar
end type
type dw_5 from u_dw_cal_day within u_calendar
end type
type dw_4 from u_dw_cal_day within u_calendar
end type
type dw_3 from u_dw_cal_day within u_calendar
end type
type dw_2 from u_dw_cal_day within u_calendar
end type
type dw_1 from u_dw_cal_day within u_calendar
end type
end forward

global type u_calendar from userobject
integer width = 4169
integer height = 3140
boolean border = true
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event type integer ue_date_changing ( date ad_new_date )
event ue_resize ( )
dw_daily dw_daily
dw_42 dw_42
dw_41 dw_41
dw_40 dw_40
dw_39 dw_39
dw_38 dw_38
dw_37 dw_37
dw_36 dw_36
st_7 st_7
r_1 r_1
st_6 st_6
st_5 st_5
st_4 st_4
st_3 st_3
st_2 st_2
st_1 st_1
dw_35 dw_35
dw_34 dw_34
dw_33 dw_33
dw_32 dw_32
dw_31 dw_31
dw_30 dw_30
dw_29 dw_29
dw_28 dw_28
dw_27 dw_27
dw_26 dw_26
dw_25 dw_25
dw_24 dw_24
dw_23 dw_23
dw_22 dw_22
dw_21 dw_21
dw_20 dw_20
dw_19 dw_19
dw_18 dw_18
dw_17 dw_17
dw_16 dw_16
dw_15 dw_15
dw_14 dw_14
dw_13 dw_13
dw_12 dw_12
dw_11 dw_11
dw_10 dw_10
dw_9 dw_9
dw_8 dw_8
dw_7 dw_7
dw_6 dw_6
dw_5 dw_5
dw_4 dw_4
dw_3 dw_3
dw_2 dw_2
dw_1 dw_1
end type
global u_calendar u_calendar

type variables
u_dw_cal_day		idw_position[]
statictext			ist_header[]
DATASTORE        ids_dw[]

DataStore		ids_datasource


Date      		id_display_date
LONG				il_display_year
INTEGER			ii_display_month
INTEGER			ii_display_day

DataStore      ids_company_calendar

BOOLEAN			ib_display_company_calendar

INTEGER		ii_WEEKEND_MODE = 2

//CONSTANT	INTEGER		WEEKEND_NONE = 0
CONSTANT INTEGER		WEEKEND_ONE = 1
CONSTANT INTEGER		WEEKEND_TWO = 2

CONSTANT LONG		COLOR_LIGHTPURPLE = 33521792
LONG il_individual_no
end variables

forward prototypes
public function integer nf_set_datasource (datastore ads_datasource)
public function integer nf_set_display_month (integer ai_month)
public function integer nf_load_datasource ()
public function integer nf_set_display_date (date ad_display_date)
public function integer nf_load_company_calendar (integer ai_year)
public function integer nf_display_company_calendar (boolean ab_display_cal)
public function integer nf_filter_for_month ()
public function integer nf_increment_date (string as_element, integer ai_increment)
public function integer nf_set_weekend_mode (integer ai_weekend_mode)
public function integer nf_populate_daily (date adt_date)
end prototypes

event type integer ue_date_changing(date ad_new_date);RETURN 1
end event

event ue_resize();//The resized width and height of the calendar control
LONG		ll_calendar_w
LONG		ll_calendar_h
LONG     ll_upper

BOOLEAN		lb_DayVisible[] ={True,True,True,True,True,True,True}
BOOLEAN		lb_HeaderVisible[] ={True,True,True,True,True,True,True}

//Th
LONG		ll_columns
LONG		ll_rows =8
LONG		ll_column_gap[] = {0,4,4,4,4,4,4}
LONG		ll_row_gap[] = {0,4,4,4,4,4}
Decimal	ldec_avg_column_w
Decimal	ldec_avg_row_h
LONG		ll_new_column_w
LONG		ll_header_height 


LONG		ll_current_dw_w
LONG		ll_current_dw_w_pixels
LONG		ll_new_dw_w_pixels
LONG		ll_new_dw_h_pixels
INTEGER	li_x
INTEGER	li_week_day
INTEGER	li_row
INTEGER	li_previous_row
LONG		ll_previous_x
LONG		ll_previous_w
LONG		ll_current_dw_h
LONG		ll_current_dw_h_pixels
LONG		ll_y


//IF ii_WEEKEND_MODE = WEEKEND_NONE THEN
//	ll_columns = 5
//	lb_DayVisible[6] = False
//	lb_DayVisible[7] = False
//	
//	lb_HeaderVisible[6] = False
//	lb_HeaderVisible[7] = False
////ELSE
//IF ii_WEEKEND_MODE = WEEKEND_ONE THEN
//	ll_columns = 6
//	lb_DayVisible[6] = True
//	lb_DayVisible[7] = True
//	
//	lb_HeaderVisible[6] = True
//	lb_HeaderVisible[7] = False
////ELSE
IF 	ii_WEEKEND_MODE = WEEKEND_TWO THEN
	ll_columns = 7
	lb_DayVisible[6] = True
	lb_DayVisible[7] = True
	
	lb_HeaderVisible[6] = True
	lb_HeaderVisible[7] = True
ELSE
	SignalError(-666,'Invalid WEEKEND_MODE')
END IF 

IF UpperBound(idw_position) > 0 Then
	SetRedraw(False)
	
	//Get the current width and height of the day datawindows. They are all the same so just us the first one.
	ll_current_dw_w = idw_position[1].width
	ll_current_dw_h = idw_position[1].height
	
	//Get the current datawindows size in pixels. This is done so we only change the size if it will result in a visible change. Atleast one pixel
	ll_current_dw_w_pixels = UnitsToPixels(ll_current_dw_w,XUnitsToPixels!)
	ll_current_dw_h_pixels = UnitsToPixels(ll_current_dw_h,yUnitsToPixels!)

	//Get the Y position of the first datawindow since it is not zero.
	ll_y = idw_position[1].y
	ll_header_height = ll_y	
	
	//Get the calendars height and width
	ll_calendar_w = width
	ll_calendar_h = height
	
	//Calculate the potential new width and height of the day datawindows
	ldec_avg_column_w = (ll_calendar_w - 18) / ll_columns
	ldec_avg_row_h = (ll_calendar_h - 18 - ll_header_height) / ll_rows
	
	//Convert the new potential size to pixels
	ll_new_dw_w_pixels = UnitsToPixels(Integer(ldec_avg_column_w),XUnitsToPixels!)
	ll_new_dw_h_pixels = UnitsToPixels(Integer(ldec_avg_row_h),yUnitsToPixels!)
	
	ll_upper = UpperBound(idw_position)
	//Only go into the resize routine if the Height or Width will change
	IF ll_new_dw_w_pixels <> ll_current_dw_w_pixels or ll_current_dw_h_pixels <> ll_new_dw_h_Pixels Then
		//This is here for the open of the window.  The uo_calendar is created at that point.
		For li_x = 1 to ll_upper
			//Calculate the row
			li_row = Ceiling(li_x/7)
			//Calculate the day of the week
			li_week_day = li_x - ((li_row - 1) * 7)	

			
			IF ii_WEEKEND_MODE  = WEEKEND_ONE and li_week_day = 7 Then
				//Day 6
				idw_position[li_x - 1].height = Integer(ldec_avg_row_h - ll_row_gap[li_row]) / 2
				
				//Day 7
				idw_position[li_x].x = ll_previous_x 
				idw_position[li_x].width = Integer(ldec_avg_column_w) - ll_column_gap	[li_week_day]
				
				idw_position[li_x].y = ll_y + idw_position[li_x - 1].height + ll_row_gap[li_row]
//				idw_position[li_x].height = (Integer(ldec_avg_row_h - ll_row_gap[li_row]) / 2) - ll_row_gap[li_row]
				idw_position[li_x].height = (idw_position[li_x -2].Y + idw_position[li_x - 2].height) - idw_position[li_x].Y
				
				idw_position[li_x].visible = lb_DayVisible[li_week_day]				
			Else
			
				idw_position[li_x].x = (ll_previous_x + ll_previous_w) + ll_column_gap[li_week_day]
				idw_position[li_x].width = Integer(ldec_avg_column_w) - ll_column_gap	[li_week_day]
				idw_position[li_x].y = ll_y
				idw_position[li_x].height = Integer(ldec_avg_row_h - ll_row_gap[li_row])
				idw_position[li_x].visible = lb_DayVisible[li_week_day]
				
			End if
			
			IF li_row = 1 Then
				ist_header[li_x].x = idw_position[li_x].x
				ist_header[li_x].width = idw_position[li_x].width
				ist_header[li_x].visible = lb_HeaderVisible[li_week_day]
			End if
			
			IF Ceiling((li_x + 1) /7) <> li_row Then
				//Figure out the new y

				ll_y = idw_position[li_x - 3].y + idw_position[li_x - 3].height + 4
				
								
				ll_previous_x = 0
				ll_previous_w = 0
			Else
				ll_previous_x = idw_position[li_x].x
				ll_previous_w = idw_position[li_x].width
			End if

		Next
		dw_daily.x = idw_position[1].x
		dw_daily.y = idw_position[ll_upper].y + idw_position[ll_upper].height + 5
		dw_daily.width = ll_calendar_w
	End if

End if

SetRedraw(True)
end event

public function integer nf_set_datasource (datastore ads_datasource);
ids_datasource = ads_datasource

return 1
end function

public function integer nf_set_display_month (integer ai_month);
IF ai_month < 1 or ai_month > 12 Then
	SignalError(-666,'Invalid month number')
End if

ii_display_month = ai_month

return 1
end function

public function integer nf_load_datasource ();INTEGER			li_x
LONG				ll_source_rows
INTEGER			li_rtn

ll_source_rows = ids_datasource.RowCount()

If ll_source_rows <> 0 Then	
	For li_x = 1 to UpperBound(idw_position)
		idw_position[li_x].Reset()
		idw_position[li_x].object.data = ids_datasource.object.data
	Next
End if


return 1
end function

public function integer nf_set_display_date (date ad_display_date);
If this.event ue_date_changing(ad_display_date) = 1 Then
	
	id_display_date = ad_display_date
	il_display_year = Year(id_display_date)
	ii_display_month = Month(id_display_date)
	ii_display_day = Day(id_display_date)
End if



return 1
end function

public function integer nf_load_company_calendar (integer ai_year);

IF ai_year < 1900 or ai_year > Year(Today()) + 30 Then
	SignalError(-666,'Error retrieving company calendar for rediculous date')
end if

If ids_company_calendar.Retrieve(ai_year) = -1 Then SignalError(-666,'Error retrieving company calendar information')

SQLCA.nf_handle_error('u_mark_calendar_tester','nf_load_company_calendar','retrieve')

RETURN 1
end function

public function integer nf_display_company_calendar (boolean ab_display_cal);

ib_display_company_calendar = ab_display_cal


return 1
end function

public function integer nf_filter_for_month ();//STRING		ls_filter
LONG			ll_first_day_number
LONG			ll_found_row
INTEGER		li_first_day_position
Date			ld_first_day_of_month
Date			ld_dw_position_date
INTEGER		li_x
INTEGER		li_days_to_add
INTEGER		li_rtn
INTEGER		li_cal_day_number[7] = {1,2,3,4,5,6,7} //Converts DayNumber to what I need it to be
INTEGER		li_focus_dw
STRING		ls_day_text
BOOLEAN		lb_holiday


SetPointer(hourglass!)
SetRedraw(False)

//Calculate the number of days to subtract from the display date
//to get the first day of the month
li_days_to_add = (-1 * Day(id_display_date)) + 1

//Calculate the date for the first day of the month
ld_first_day_of_month = RelativeDate(id_display_date, li_days_to_add)

//Determine which u_dw_cal_day in the first week (row) represents the first
//day of the display month
//If the first day of the month is Thursday then dw_4 is the u_dw_cal_day 
long li_day, li_day_num
li_day = Day(ld_first_day_of_month)
li_day_num = daynumber(ld_first_day_of_month)
li_first_day_position = li_cal_day_number[DayNumber(ld_first_day_of_month)]

//Loop throught all the u_dw_cal_days setting the daynumber for each and 
// setting the filter so they only show information for the date they have
// been assigned.
FOR li_x = 1 to UpperBound(idw_position)
	//Get each u_dw_cal_days date to display.
	ld_dw_position_date = RelativeDate(ld_first_day_of_month, (-1 * (li_first_day_position - li_x)))
	
	//Load the company calendar holiday descritpion for the current position date
	ll_found_row = ids_company_calendar.Find("calendar_date = Date('" + String(ld_dw_position_date,'yyyy-mm-dd') + "')",1,ids_company_calendar.RowCount())
	IF ll_found_row  = -1 Then SignalError(-666,'Error finding company calendar info.')
	
	IF ll_found_row > 0 THen
		ls_day_text = ids_company_calendar.GetItemString(ll_found_row,"day_desc")
		idw_position[li_x].nf_set_display_date(ld_dw_position_date,ls_day_text)
		lb_holiday = True
	Else
		idw_position[li_x].nf_set_display_date(ld_dw_position_date)
		lb_holiday = False
	End if
	
	//Set the display color for days outside the display month
	iF Month(ld_dw_position_date) <> Month(id_display_date) Then
		idw_position[li_x].object.datawindow.color = RGB(235,240,250)
	Else
		IF lb_holiday = True Then
			idw_position[li_x].object.datawindow.color = RGB(154,181,228)
			idw_position[li_x].object.event_description.color = RGB(255,255,255)
		Else
			idw_position[li_x].object.datawindow.color = RGB(250,242,235)
		End if
	end if
	If ld_dw_position_date = id_display_date Then
		li_focus_dw = li_x
	End if
Next

idw_position[li_focus_dw].SetFocus()

SetRedraw(True)

return 1
end function

public function integer nf_increment_date (string as_element, integer ai_increment);INTEGER		li_new_month
INTEGER		li_new_year



CHOOSE CASE as_element
	CASE 'm'	
		li_new_month = ii_display_month + ai_increment
		li_new_year = il_display_year
		
		if li_new_month > 12 then
			li_new_month = 1
			li_new_year = il_display_year + 1
		
		elseIF li_new_month < 1 Then
			li_new_month = 12
			li_new_year = il_display_year - 1
		end if
		
		Do until isDate(String(li_new_year) + '-' + String(li_new_month) + '-' + String(ii_display_day))
			ii_display_day = ii_display_day - 1
		Loop
		
		nf_set_display_date( Date(li_new_year,li_new_month,ii_display_day))
END CHOOSE

nf_filter_for_month()

return 1
end function

public function integer nf_set_weekend_mode (integer ai_weekend_mode);ii_Weekend_mode = ai_weekend_mode

IF ii_weekend_mode = WEEKEND_ONE THEN
	ist_header[7].text = 'Sat/Sun'
Else
	ist_header[7].text = 'Saturday'
END IF

This.event ue_resize()

return 1
end function

public function integer nf_populate_daily (date adt_date);LONG ll_rows

ll_rows = dw_daily.Retrieve(il_individual_no, adt_date)

IF ll_rows < 0 THEN
	MessageBox('Daily Calendar','The Daily Calendar is not available for the date selected.  Please try another date.', Information!)
END IF

RETURN ll_rows
end function

on u_calendar.create
this.dw_daily=create dw_daily
this.dw_42=create dw_42
this.dw_41=create dw_41
this.dw_40=create dw_40
this.dw_39=create dw_39
this.dw_38=create dw_38
this.dw_37=create dw_37
this.dw_36=create dw_36
this.st_7=create st_7
this.r_1=create r_1
this.st_6=create st_6
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.dw_35=create dw_35
this.dw_34=create dw_34
this.dw_33=create dw_33
this.dw_32=create dw_32
this.dw_31=create dw_31
this.dw_30=create dw_30
this.dw_29=create dw_29
this.dw_28=create dw_28
this.dw_27=create dw_27
this.dw_26=create dw_26
this.dw_25=create dw_25
this.dw_24=create dw_24
this.dw_23=create dw_23
this.dw_22=create dw_22
this.dw_21=create dw_21
this.dw_20=create dw_20
this.dw_19=create dw_19
this.dw_18=create dw_18
this.dw_17=create dw_17
this.dw_16=create dw_16
this.dw_15=create dw_15
this.dw_14=create dw_14
this.dw_13=create dw_13
this.dw_12=create dw_12
this.dw_11=create dw_11
this.dw_10=create dw_10
this.dw_9=create dw_9
this.dw_8=create dw_8
this.dw_7=create dw_7
this.dw_6=create dw_6
this.dw_5=create dw_5
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_2=create dw_2
this.dw_1=create dw_1
this.Control[]={this.dw_daily,&
this.dw_42,&
this.dw_41,&
this.dw_40,&
this.dw_39,&
this.dw_38,&
this.dw_37,&
this.dw_36,&
this.st_7,&
this.r_1,&
this.st_6,&
this.st_5,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_1,&
this.dw_35,&
this.dw_34,&
this.dw_33,&
this.dw_32,&
this.dw_31,&
this.dw_30,&
this.dw_29,&
this.dw_28,&
this.dw_27,&
this.dw_26,&
this.dw_25,&
this.dw_24,&
this.dw_23,&
this.dw_22,&
this.dw_21,&
this.dw_20,&
this.dw_19,&
this.dw_18,&
this.dw_17,&
this.dw_16,&
this.dw_15,&
this.dw_14,&
this.dw_13,&
this.dw_12,&
this.dw_11,&
this.dw_10,&
this.dw_9,&
this.dw_8,&
this.dw_7,&
this.dw_6,&
this.dw_5,&
this.dw_4,&
this.dw_3,&
this.dw_2,&
this.dw_1}
end on

on u_calendar.destroy
destroy(this.dw_daily)
destroy(this.dw_42)
destroy(this.dw_41)
destroy(this.dw_40)
destroy(this.dw_39)
destroy(this.dw_38)
destroy(this.dw_37)
destroy(this.dw_36)
destroy(this.st_7)
destroy(this.r_1)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_35)
destroy(this.dw_34)
destroy(this.dw_33)
destroy(this.dw_32)
destroy(this.dw_31)
destroy(this.dw_30)
destroy(this.dw_29)
destroy(this.dw_28)
destroy(this.dw_27)
destroy(this.dw_26)
destroy(this.dw_25)
destroy(this.dw_24)
destroy(this.dw_23)
destroy(this.dw_22)
destroy(this.dw_21)
destroy(this.dw_20)
destroy(this.dw_19)
destroy(this.dw_18)
destroy(this.dw_17)
destroy(this.dw_16)
destroy(this.dw_15)
destroy(this.dw_14)
destroy(this.dw_13)
destroy(this.dw_12)
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.dw_9)
destroy(this.dw_8)
destroy(this.dw_7)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_2)
destroy(this.dw_1)
end on

event constructor;
//WEEK 1
idw_position[1] = dw_7
idw_position[2] = dw_1
idw_position[3] = dw_2
idw_position[4] = dw_3
idw_position[5] = dw_4
idw_position[6] = dw_5
idw_position[7] = dw_6

//WEEK 2
idw_position[8] = dw_8
idw_position[9] = dw_14
idw_position[10] = dw_13
idw_position[11] = dw_12
idw_position[12] = dw_11
idw_position[13] = dw_10
idw_position[14] = dw_9


//WEEK 3
idw_position[15] = dw_15
idw_position[16] = dw_21
idw_position[17] = dw_20
idw_position[18] = dw_19
idw_position[19] = dw_18
idw_position[20] = dw_17
idw_position[21] = dw_16

//WEEK 4
idw_position[22] = dw_22
idw_position[23] = dw_28
idw_position[24] = dw_27
idw_position[25] = dw_26
idw_position[26] = dw_25
idw_position[27] = dw_24
idw_position[28] = dw_23

//WEEK 5
idw_position[29] = dw_29
idw_position[30] = dw_35
idw_position[31] = dw_34
idw_position[32] = dw_33
idw_position[33] = dw_32
idw_position[34] = dw_31
idw_position[35] = dw_30

//WEEK 6
idw_position[36] = dw_37
idw_position[37] = dw_36
idw_position[38] = dw_38
idw_position[39] = dw_39
idw_position[40] = dw_40
idw_position[41] = dw_41
idw_position[42] = dw_42


//HEADERS
ist_header[1] = st_7
ist_header[2] = st_1
ist_header[3] = st_2
ist_header[4] = st_3
ist_header[5] = st_4
ist_header[6] = st_5
ist_header[7] = st_6


ids_company_calendar = CREATE u_ds

ids_company_calendar.DataObject = 'd_company_calendar'
ids_company_calendar.SetTransObject(SQLCA)

dw_daily.SetTransObject(SQLCA)



end event

type dw_daily from datawindow within u_calendar
integer x = 14
integer y = 2352
integer width = 4133
integer height = 744
integer taborder = 120
string title = "none"
string dataobject = "d_daily"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_42 from u_dw_cal_day within u_calendar
integer x = 3552
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 110
boolean border = false
end type

type dw_41 from u_dw_cal_day within u_calendar
integer x = 2962
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 110
boolean border = false
end type

type dw_40 from u_dw_cal_day within u_calendar
integer x = 2373
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 110
boolean border = false
end type

event getfocus;call super::getfocus;LONG ll_rows

THIS.RowCount()

end event

type dw_39 from u_dw_cal_day within u_calendar
integer x = 1783
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 100
boolean border = false
end type

type dw_38 from u_dw_cal_day within u_calendar
integer x = 1193
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 90
boolean border = false
end type

type dw_37 from u_dw_cal_day within u_calendar
integer x = 14
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 80
boolean border = false
end type

type dw_36 from u_dw_cal_day within u_calendar
integer x = 603
integer y = 1952
integer width = 585
integer height = 368
integer taborder = 70
boolean border = false
end type

type st_7 from statictext within u_calendar
integer x = 14
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Sunday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type r_1 from rectangle within u_calendar
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 22188753
integer x = 9
integer y = 84
integer width = 4137
integer height = 2248
end type

type st_6 from statictext within u_calendar
integer x = 3552
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Saturday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within u_calendar
integer x = 2962
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Friday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within u_calendar
integer x = 2373
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Thursday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within u_calendar
integer x = 1783
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Wednesday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within u_calendar
integer x = 1193
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Tuesday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within u_calendar
integer x = 603
integer y = 4
integer width = 585
integer height = 76
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 16777215
long backcolor = 22188753
string text = "Monday"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_35 from u_dw_cal_day within u_calendar
integer x = 603
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 60
boolean border = false
end type

type dw_34 from u_dw_cal_day within u_calendar
integer x = 1193
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_33 from u_dw_cal_day within u_calendar
integer x = 1783
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 70
boolean border = false
end type

type dw_32 from u_dw_cal_day within u_calendar
integer x = 2373
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_31 from u_dw_cal_day within u_calendar
integer x = 2962
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 50
boolean border = false
end type

type dw_30 from u_dw_cal_day within u_calendar
integer x = 3552
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 50
boolean border = false
end type

type dw_29 from u_dw_cal_day within u_calendar
integer x = 14
integer y = 1580
integer width = 585
integer height = 368
integer taborder = 60
boolean border = false
end type

type dw_28 from u_dw_cal_day within u_calendar
integer x = 603
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 20
boolean border = false
end type

type dw_27 from u_dw_cal_day within u_calendar
integer x = 1193
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 60
boolean border = false
end type

type dw_26 from u_dw_cal_day within u_calendar
integer x = 1783
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_25 from u_dw_cal_day within u_calendar
integer x = 2373
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_24 from u_dw_cal_day within u_calendar
integer x = 2962
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_23 from u_dw_cal_day within u_calendar
integer x = 3552
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 50
boolean border = false
end type

type dw_22 from u_dw_cal_day within u_calendar
integer x = 14
integer y = 1208
integer width = 585
integer height = 368
integer taborder = 50
boolean border = false
end type

type dw_21 from u_dw_cal_day within u_calendar
integer x = 603
integer y = 836
integer width = 585
integer height = 368
integer taborder = 50
boolean border = false
end type

type dw_20 from u_dw_cal_day within u_calendar
integer x = 1193
integer y = 836
integer width = 585
integer height = 368
integer taborder = 20
boolean border = false
end type

type dw_19 from u_dw_cal_day within u_calendar
integer x = 1783
integer y = 836
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_18 from u_dw_cal_day within u_calendar
integer x = 2373
integer y = 836
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_17 from u_dw_cal_day within u_calendar
integer x = 2962
integer y = 836
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_16 from u_dw_cal_day within u_calendar
integer x = 3552
integer y = 836
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_15 from u_dw_cal_day within u_calendar
integer x = 14
integer y = 836
integer width = 585
integer height = 368
integer taborder = 50
boolean border = false
end type

type dw_14 from u_dw_cal_day within u_calendar
integer x = 603
integer y = 464
integer width = 585
integer height = 368
integer taborder = 10
boolean border = false
end type

type dw_13 from u_dw_cal_day within u_calendar
integer x = 1193
integer y = 464
integer width = 585
integer height = 368
integer taborder = 20
boolean border = false
end type

type dw_12 from u_dw_cal_day within u_calendar
integer x = 1783
integer y = 464
integer width = 585
integer height = 368
integer taborder = 20
boolean border = false
end type

type dw_11 from u_dw_cal_day within u_calendar
integer x = 2373
integer y = 464
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_10 from u_dw_cal_day within u_calendar
integer x = 2962
integer y = 464
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_9 from u_dw_cal_day within u_calendar
integer x = 3552
integer y = 464
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_8 from u_dw_cal_day within u_calendar
integer x = 14
integer y = 464
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_7 from u_dw_cal_day within u_calendar
integer x = 14
integer y = 92
integer width = 585
integer height = 368
integer taborder = 40
boolean border = false
end type

type dw_6 from u_dw_cal_day within u_calendar
integer x = 3552
integer y = 92
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_5 from u_dw_cal_day within u_calendar
integer x = 2962
integer y = 92
integer width = 585
integer height = 368
integer taborder = 30
boolean border = false
end type

type dw_4 from u_dw_cal_day within u_calendar
integer x = 2373
integer y = 92
integer width = 585
integer height = 368
integer taborder = 20
boolean border = false
end type

type dw_3 from u_dw_cal_day within u_calendar
integer x = 1783
integer y = 92
integer width = 585
integer height = 368
integer taborder = 20
boolean border = false
end type

type dw_2 from u_dw_cal_day within u_calendar
integer x = 1193
integer y = 92
integer width = 585
integer height = 368
integer taborder = 10
boolean border = false
end type

type dw_1 from u_dw_cal_day within u_calendar
event ue_lbutton_down pbm_dwnlbuttonup
integer x = 603
integer y = 92
integer width = 585
integer height = 368
integer taborder = 10
boolean bringtotop = true
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

event itemchanged;call super::itemchanged;this.SetPosition(ToTop!)
end event

