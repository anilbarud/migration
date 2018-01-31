$PBExportHeader$uo_tab_insert_physio.sru
forward
global type uo_tab_insert_physio from userobject
end type
end forward

global type uo_tab_insert_physio from userobject
integer width = 1595
integer height = 84
long backcolor = 67108864
long picturemaskcolor = 536870912
end type
global uo_tab_insert_physio uo_tab_insert_physio

type variables
STRING  	is_provider_type_code
LONG	 	il_task_no, il_provider_no, il_claim_no
DATE		idt_planned_start_date

end variables

forward prototypes
public function long of_return_task_no ()
public function str_info_to_tab of_return_tab_info ()
end prototypes

public function long of_return_task_no ();IF isnull(il_task_no) OR il_task_no < 1 THEN RETURN 0

RETURN il_task_no
end function

public function str_info_to_tab of_return_tab_info ();str_info_to_tab str_info_to_tab

str_info_to_tab.provider_no 					= il_provider_no
str_info_to_tab.task_no 						= il_task_no
str_info_to_tab.provider_type 				= is_provider_type_code
str_info_to_tab.tab_text 						=""
str_info_to_tab.planned_start_date 		= idt_planned_start_date
str_info_to_tab.claim_no						= il_claim_no


RETURN str_info_to_tab
end function

on uo_tab_insert_physio.create
end on

on uo_tab_insert_physio.destroy
end on

event constructor;str_info_to_tab str_info_to_tab

//populate the structure
str_info_to_tab = message.powerobjectparm

//set the variables
il_task_no 						= str_info_to_tab.task_no
il_provider_no 				= str_info_to_tab.provider_no
is_provider_type_code 	= str_info_to_tab.provider_type
idt_planned_start_date	= str_info_to_tab.planned_start_date
il_claim_no						= str_info_to_tab.claim_no
THIS.text 						= str_info_to_tab.tab_text


end event

