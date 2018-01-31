$PBExportHeader$w_service_provider_index.srw
$PBExportComments$inherited window from w_service_provider_search
forward
global type w_service_provider_index from w_service_provider_search
end type
end forward

global type w_service_provider_index from w_service_provider_search
integer x = 1335
integer y = 688
end type
global w_service_provider_index w_service_provider_index

on w_service_provider_index.create
int iCurrent
call super::create
end on

on w_service_provider_index.destroy
call super::destroy
end on

event open;call super::open;//due to the multiple viewer configuarations we want to set this close to the frame location
this.x = w_frame.x + 200
end event

type uo_search from w_service_provider_search`uo_search within w_service_provider_index
end type

type cb_ok from w_service_provider_search`cb_ok within w_service_provider_index
end type

type cb_cancel from w_service_provider_search`cb_cancel within w_service_provider_index
end type

