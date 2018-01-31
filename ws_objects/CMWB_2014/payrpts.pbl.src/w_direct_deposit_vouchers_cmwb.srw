$PBExportHeader$w_direct_deposit_vouchers_cmwb.srw
$PBExportComments$Medical Aid Vouchers
forward
global type w_direct_deposit_vouchers_cmwb from w_direct_deposit_vouchers_ext
end type
end forward

global type w_direct_deposit_vouchers_cmwb from w_direct_deposit_vouchers_ext
integer height = 2220
string menuname = "m_cmwb_notools"
end type
global w_direct_deposit_vouchers_cmwb w_direct_deposit_vouchers_cmwb

on w_direct_deposit_vouchers_cmwb.create
call super::create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
end on

on w_direct_deposit_vouchers_cmwb.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on

event open;call super::open;/*
Sven Oborn
April 7/2011

The only changes to this window was to hide some controls from the user.  No script changes had been made.

*/

end event

type cb_search from w_direct_deposit_vouchers_ext`cb_search within w_direct_deposit_vouchers_cmwb
end type

type rb_deposit_number from w_direct_deposit_vouchers_ext`rb_deposit_number within w_direct_deposit_vouchers_cmwb
end type

type rb_last_printed from w_direct_deposit_vouchers_ext`rb_last_printed within w_direct_deposit_vouchers_cmwb
boolean visible = false
end type

type cb_print_batch from w_direct_deposit_vouchers_ext`cb_print_batch within w_direct_deposit_vouchers_cmwb
boolean visible = false
end type

type cb_preview from w_direct_deposit_vouchers_ext`cb_preview within w_direct_deposit_vouchers_cmwb
end type

type cb_next from w_direct_deposit_vouchers_ext`cb_next within w_direct_deposit_vouchers_cmwb
end type

type cb_prior from w_direct_deposit_vouchers_ext`cb_prior within w_direct_deposit_vouchers_cmwb
end type

type dw_direct_deposit from w_direct_deposit_vouchers_ext`dw_direct_deposit within w_direct_deposit_vouchers_cmwb
end type

type cb_print_voucher from w_direct_deposit_vouchers_ext`cb_print_voucher within w_direct_deposit_vouchers_cmwb
integer x = 2203
end type

type dw_direct_deposit_voucher from w_direct_deposit_vouchers_ext`dw_direct_deposit_voucher within w_direct_deposit_vouchers_cmwb
end type

type dw_dd_history from w_direct_deposit_vouchers_ext`dw_dd_history within w_direct_deposit_vouchers_cmwb
end type

type cb_close from w_direct_deposit_vouchers_ext`cb_close within w_direct_deposit_vouchers_cmwb
end type

type gb_search_by from w_direct_deposit_vouchers_ext`gb_search_by within w_direct_deposit_vouchers_cmwb
end type

type rb_service_provider from w_direct_deposit_vouchers_ext`rb_service_provider within w_direct_deposit_vouchers_cmwb
end type

