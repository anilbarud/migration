$PBExportHeader$uo_nvo_base.sru
$PBExportComments$Base Object to inherit from for non visual custom objects
forward
global type uo_nvo_base from nonvisualobject
end type
end forward

global type uo_nvo_base from nonvisualobject
end type
global uo_nvo_base uo_nvo_base

on uo_nvo_base.create
TriggerEvent( this, "constructor" )
end on

on uo_nvo_base.destroy
TriggerEvent( this, "destructor" )
end on

