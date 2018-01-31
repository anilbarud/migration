$PBExportHeader$w_direct_deposit_vouchers_ext.srw
$PBExportComments$Medical Aid Vouchers
forward
global type w_direct_deposit_vouchers_ext from w_direct_deposit_vouchers
end type
end forward

global type w_direct_deposit_vouchers_ext from w_direct_deposit_vouchers
end type
global w_direct_deposit_vouchers_ext w_direct_deposit_vouchers_ext

on w_direct_deposit_vouchers_ext.create
call super::create
end on

on w_direct_deposit_vouchers_ext.destroy
call super::destroy
end on

type cb_search from w_direct_deposit_vouchers`cb_search within w_direct_deposit_vouchers_ext
end type

type rb_deposit_number from w_direct_deposit_vouchers`rb_deposit_number within w_direct_deposit_vouchers_ext
end type

type rb_last_printed from w_direct_deposit_vouchers`rb_last_printed within w_direct_deposit_vouchers_ext
end type

type cb_print_batch from w_direct_deposit_vouchers`cb_print_batch within w_direct_deposit_vouchers_ext
end type

type cb_preview from w_direct_deposit_vouchers`cb_preview within w_direct_deposit_vouchers_ext
end type

type cb_next from w_direct_deposit_vouchers`cb_next within w_direct_deposit_vouchers_ext
end type

type cb_prior from w_direct_deposit_vouchers`cb_prior within w_direct_deposit_vouchers_ext
end type

type dw_direct_deposit from w_direct_deposit_vouchers`dw_direct_deposit within w_direct_deposit_vouchers_ext
end type

type cb_print_voucher from w_direct_deposit_vouchers`cb_print_voucher within w_direct_deposit_vouchers_ext
end type

type dw_direct_deposit_voucher from w_direct_deposit_vouchers`dw_direct_deposit_voucher within w_direct_deposit_vouchers_ext
end type

type dw_dd_history from w_direct_deposit_vouchers`dw_dd_history within w_direct_deposit_vouchers_ext
end type

type cb_close from w_direct_deposit_vouchers`cb_close within w_direct_deposit_vouchers_ext
end type

type gb_search_by from w_direct_deposit_vouchers`gb_search_by within w_direct_deposit_vouchers_ext
end type

type rb_service_provider from w_direct_deposit_vouchers`rb_service_provider within w_direct_deposit_vouchers_ext
end type

