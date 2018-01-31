$PBExportHeader$docktest.sra
$PBExportComments$Generated Application Object
forward
global type docktest from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global type docktest from application
string appname = "docktest"
end type
global docktest docktest

on docktest.create
appname="docktest"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on docktest.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;open(w_mdiwindow)
end event

