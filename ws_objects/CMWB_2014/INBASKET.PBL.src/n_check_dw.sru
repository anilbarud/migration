$PBExportHeader$n_check_dw.sru
$PBExportComments$Prevents app. error when there's concurrency prob between 2 different DWs. It only works for up to 12 args and it's not coded for array args. Also - the DW can only have 100 obj.
forward
global type n_check_dw from nonvisualobject
end type
end forward

global type n_check_dw from nonvisualobject
end type
global n_check_dw n_check_dw

type variables
ANY ia_any[12]
U_DS ids_ds
STRING is_datawindow, is_window
end variables

forward prototypes
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, any aa_any10, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, any aa_any10, any aa_any11, string as_datawindow, string as_window)
public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, any aa_any10, any aa_any11, any aa_any12, string as_datawindow, string as_window)
public function string nf_compare (ref datawindow adw_dw)
public function string nf_updateable (integer ai_col_number)
end prototypes

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, string as_datawindow, string as_window);
STRING ls_temp, ls_sql

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ia_any[7] = aa_any07
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ia_any[7] = aa_any07
ia_any[8] = aa_any08
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, string as_datawindow, string as_window);
STRING ls_temp

ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

is_datawindow = as_datawindow; is_window = as_window
ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ia_any[7] = aa_any07
ia_any[8] = aa_any08
ia_any[9] = aa_any09
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, any aa_any10, string as_datawindow, string as_window);
STRING ls_temp

ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

is_datawindow = as_datawindow; is_window = as_window
ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ia_any[7] = aa_any07
ia_any[8] = aa_any08
ia_any[9] = aa_any09
ia_any[10] = aa_any10
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, any aa_any10, any aa_any11, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ia_any[7] = aa_any07
ia_any[8] = aa_any08
ia_any[9] = aa_any09
ia_any[10] = aa_any10
ia_any[11] = aa_any11
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_check_dw (ref datawindow adw_dw, ref n_transaction atr_trans, any aa_any01, any aa_any02, any aa_any03, any aa_any04, any aa_any05, any aa_any06, any aa_any07, any aa_any08, any aa_any09, any aa_any10, any aa_any11, any aa_any12, string as_datawindow, string as_window);
STRING ls_temp

is_datawindow = as_datawindow; is_window = as_window
ids_ds.DataObject = adw_dw.DataObject
ids_ds.SetTransObject(atr_trans)

ia_any[1] = aa_any01
ia_any[2] = aa_any02
ia_any[3] = aa_any03
ia_any[4] = aa_any04
ia_any[5] = aa_any05
ia_any[6] = aa_any06
ia_any[7] = aa_any07
ia_any[8] = aa_any08
ia_any[9] = aa_any09
ia_any[10] = aa_any10
ia_any[11] = aa_any11
ia_any[12] = aa_any12
ls_temp = nf_compare(adw_dw)

RETURN ls_temp
end function

public function string nf_compare (ref datawindow adw_dw);
STRING			ls_temp,ls_sort,ls_table,ls_col,ls_cols,ls_filter, ls_name, ls_updateable
ANY				la_col_value

/*Variable names that start with lower case d are database values 
Variable names that start with lower case w are window values from w_inbasket
*/
long ll_dCount,ll_wCount,ll_ColCount,ll_RowCntr,ll_ColCntr
String ls_dwsyntax,ls_errors,ls_oldSyntax

ids_ds.retrieve(ia_any[1], ia_any[2], ia_any[3], ia_any[4], ia_any[5], ia_any[6], ia_any[7], ia_any[8], ia_any[9], ia_any[10], ia_any[11], ia_any[12])

IF ids_ds.getRow() > 0 THEN
	ls_sort = adw_dw.Describe("DataWindow.Table.Sort")
	ids_ds.setSort(ls_sort)
	ids_ds.sort()
	
	adw_dw.setSort(ls_sort)
	adw_dw.sort()
	
	//Compare rowcount
	ll_wCount = adw_dw.rowcount() 
	ll_dCount = ids_ds.rowcount()
	if ll_wCount = ll_dCount then
		ll_ColCount = long(ids_ds.Object.DataWindow.Column.Count)
		FOR ll_RowCntr = 1 to ll_wCount
			FOR ll_ColCntr = 1 to ll_ColCount
				IF ids_ds.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr] = adw_dw.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr] THEN
					//Records match do nothing
				ELSE
					ls_updateable = nf_updateable(ll_ColCntr)
					IF isNull(ids_ds.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr]) and isNull(adw_dw.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr]) THEN
					ELSEIF Lower(ls_updateable) = 'no' THEN
					ELSEIF ls_updateable = '-1' THEN
						MessageBox("Error","This window has too many object to check. Call the help desk.")
						return  ls_updateable
					ELSE
						//Records do not match & the column is updateable; return error
						ls_temp = "Another user has changed this " + is_datawindow + ". Please refresh this " + is_window + " window."
						return  ls_temp
					END IF
				END IF
			NEXT
		NEXT
	else
		if ll_wCount < ll_dCount then
			ls_temp = "Another user has added to this " + is_datawindow + ". Please refresh this " + is_window + " window."
			return ls_temp
		else
			ls_temp =  "Another user has deleted from this " + is_datawindow + ". Please refresh this " + is_window + " window."
			return  ls_temp
		end if	
	end if
ELSE
	ls_temp = "Another user has deleted all the " + is_datawindow + "s in this " + is_window + " window."
END IF

return  ls_temp
end function

public function string nf_updateable (integer ai_col_number);String ls_update

CHOOSE CASE ai_col_number
	CASE 1
		ls_update = ids_ds.Object.#1.Update
	CASE 2
		ls_update = ids_ds.Object.#2.Update
	CASE 3
		ls_update = ids_ds.Object.#3.Update
	CASE 4
		ls_update = ids_ds.Object.#4.Update
	CASE 5
		ls_update = ids_ds.Object.#5.Update
	CASE 6
		ls_update = ids_ds.Object.#6.Update
	CASE 7
		ls_update = ids_ds.Object.#7.Update
	CASE 8
		ls_update = ids_ds.Object.#8.Update
	CASE 9
		ls_update = ids_ds.Object.#9.Update
	CASE 10
		ls_update = ids_ds.Object.#10.Update
	CASE 11
		ls_update = ids_ds.Object.#11.Update
	CASE 12
		ls_update = ids_ds.Object.#12.Update
	CASE 13
		ls_update = ids_ds.Object.#13.Update
	CASE 14
		ls_update = ids_ds.Object.#14.Update
	CASE 15
		ls_update = ids_ds.Object.#15.Update
	CASE 16
		ls_update = ids_ds.Object.#16.Update
	CASE 17
		ls_update = ids_ds.Object.#17.Update
	CASE 18
		ls_update = ids_ds.Object.#18.Update
	CASE 19
		ls_update = ids_ds.Object.#19.Update
	CASE 20
		ls_update = ids_ds.Object.#20.Update
	CASE 21
		ls_update = ids_ds.Object.#21.Update
	CASE 22
		ls_update = ids_ds.Object.#22.Update
	CASE 23
		ls_update = ids_ds.Object.#23.Update
	CASE 24
		ls_update = ids_ds.Object.#24.Update
	CASE 25
		ls_update = ids_ds.Object.#25.Update
	CASE 26
		ls_update = ids_ds.Object.#26.Update
	CASE 27
		ls_update = ids_ds.Object.#27.Update
	CASE 28
		ls_update = ids_ds.Object.#28.Update
	CASE 29
		ls_update = ids_ds.Object.#29.Update
	CASE 30
		ls_update = ids_ds.Object.#30.Update
	CASE 31
		ls_update = ids_ds.Object.#31.Update
	CASE 32
		ls_update = ids_ds.Object.#32.Update
	CASE 33
		ls_update = ids_ds.Object.#33.Update
	CASE 34
		ls_update = ids_ds.Object.#34.Update
	CASE 35
		ls_update = ids_ds.Object.#35.Update
	CASE 36
		ls_update = ids_ds.Object.#36.Update
	CASE 37
		ls_update = ids_ds.Object.#37.Update
	CASE 38
		ls_update = ids_ds.Object.#38.Update
	CASE 39
		ls_update = ids_ds.Object.#39.Update
	CASE 40
		ls_update = ids_ds.Object.#40.Update
	CASE 41
		ls_update = ids_ds.Object.#41.Update
	CASE 42
		ls_update = ids_ds.Object.#42.Update
	CASE 43
		ls_update = ids_ds.Object.#43.Update
	CASE 44
		ls_update = ids_ds.Object.#44.Update
	CASE 45
		ls_update = ids_ds.Object.#45.Update
	CASE 46
		ls_update = ids_ds.Object.#46.Update
	CASE 47
		ls_update = ids_ds.Object.#47.Update
	CASE 48
		ls_update = ids_ds.Object.#48.Update
	CASE 49
		ls_update = ids_ds.Object.#49.Update
	CASE 50
		ls_update = ids_ds.Object.#50.Update
	CASE 51
		ls_update = ids_ds.Object.#51.Update
	CASE 52
		ls_update = ids_ds.Object.#52.Update
	CASE 53
		ls_update = ids_ds.Object.#53.Update
	CASE 54
		ls_update = ids_ds.Object.#54.Update
	CASE 55
		ls_update = ids_ds.Object.#55.Update
	CASE 56
		ls_update = ids_ds.Object.#56.Update
	CASE 57
		ls_update = ids_ds.Object.#57.Update
	CASE 58
		ls_update = ids_ds.Object.#58.Update
	CASE 59
		ls_update = ids_ds.Object.#59.Update
	CASE 60
		ls_update = ids_ds.Object.#60.Update
	CASE 61
		ls_update = ids_ds.Object.#61.Update
	CASE 62
		ls_update = ids_ds.Object.#62.Update
	CASE 63
		ls_update = ids_ds.Object.#63.Update
	CASE 64
		ls_update = ids_ds.Object.#64.Update
	CASE 65
		ls_update = ids_ds.Object.#65.Update
	CASE 66
		ls_update = ids_ds.Object.#66.Update
	CASE 67
		ls_update = ids_ds.Object.#67.Update
	CASE 68
		ls_update = ids_ds.Object.#68.Update
	CASE 69
		ls_update = ids_ds.Object.#69.Update
	CASE 70
		ls_update = ids_ds.Object.#70.Update
	CASE 71
		ls_update = ids_ds.Object.#71.Update
	CASE 72
		ls_update = ids_ds.Object.#72.Update
	CASE 73
		ls_update = ids_ds.Object.#73.Update
	CASE 74
		ls_update = ids_ds.Object.#74.Update
	CASE 75
		ls_update = ids_ds.Object.#75.Update
	CASE 76
		ls_update = ids_ds.Object.#76.Update
	CASE 77
		ls_update = ids_ds.Object.#77.Update
	CASE 78
		ls_update = ids_ds.Object.#78.Update
	CASE 79
		ls_update = ids_ds.Object.#79.Update
	CASE 80
		ls_update = ids_ds.Object.#80.Update
	CASE 81
		ls_update = ids_ds.Object.#81.Update
	CASE 82
		ls_update = ids_ds.Object.#82.Update
	CASE 83
		ls_update = ids_ds.Object.#83.Update
	CASE 84
		ls_update = ids_ds.Object.#84.Update
	CASE 85
		ls_update = ids_ds.Object.#85.Update
	CASE 86
		ls_update = ids_ds.Object.#86.Update
	CASE 87
		ls_update = ids_ds.Object.#87.Update
	CASE 88
		ls_update = ids_ds.Object.#88.Update
	CASE 89
		ls_update = ids_ds.Object.#89.Update
	CASE 90
		ls_update = ids_ds.Object.#90.Update
	CASE 91
		ls_update = ids_ds.Object.#91.Update
	CASE 92
		ls_update = ids_ds.Object.#92.Update
	CASE 93
		ls_update = ids_ds.Object.#93.Update
	CASE 94
		ls_update = ids_ds.Object.#94.Update
	CASE 95
		ls_update = ids_ds.Object.#95.Update
	CASE 96
		ls_update = ids_ds.Object.#96.Update
	CASE 97
		ls_update = ids_ds.Object.#97.Update
	CASE 98
		ls_update = ids_ds.Object.#98.Update
	CASE 99
		ls_update = ids_ds.Object.#99.Update
	CASE 100
		ls_update = ids_ds.Object.#100.Update
	CASE ELSE
		return "-1" // only can handle 100 datawindow objects!
END CHOOSE
return ls_update
end function

on n_check_dw.create
TriggerEvent( this, "constructor" )
end on

on n_check_dw.destroy
TriggerEvent( this, "destructor" )
end on

event constructor;ids_ds = Create U_DS
end event

