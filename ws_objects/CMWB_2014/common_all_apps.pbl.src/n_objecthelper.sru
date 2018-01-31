$PBExportHeader$n_objecthelper.sru
forward
global type n_objecthelper from nonvisualobject
end type
end forward

global type n_objecthelper from nonvisualobject autoinstantiate
end type

forward prototypes
public function long get_app_component_type (powerobject currentobject)
public function string getpath (powerobject currentobject)
end prototypes

public function long get_app_component_type (powerobject currentobject);
// the returned value should correspond to 
// SELECT component_category_code FROM AREA51..Component_Category for the appropriate enumerated type

CHOOSE CASE currentobject.TypeOf()
	CASE Window!
		RETURN 100 
	CASE CheckBox!
		RETURN 200
	CASE CommandButton!
		RETURN 300
	CASE DataWindow!
		RETURN 400
	CASE DropDownListBox!
		RETURN 500
	CASE FunctionObject!
		RETURN 600
	CASE ListBox!
		RETURN 700
	CASE Menu!
		RETURN 800
	CASE NonVisualObject!
		RETURN 900
	CASE RadioButton!
		RETURN 1000
	CASE UserObject!
		RETURN 1100
		
	CASE ELSE
		SIGNALERROR()

END CHOOSE
end function

public function string getpath (powerobject currentobject);
try
	powerobject parentObject
	parentObject = currentObject.GetParent()

	window currentWindow
	boolean isMdi
	string className
		
	className = currentObject.ClassName()

	// Windows don't have Parents they have ParenWindows
	if currentObject.TypeOf() = Window! Then
		currentWindow = currentObject
		parentObject = currentWindow.ParentWindow()
		
		// If the current window is an MDI the append the name of the ActiveSheet to it.
		// This is necessary because other type of windows won't repor a "Sheet"
		// as there ParentWindow. They will say it's the MDI, so we will fill in the blank for them.
		if currentWindow.WindowType = MDI! Then
			isMdi = true
			window activeSheet
			activeSheet = currentWindow.GetActiveSheet()
			if IsValid(activeSheet) Then
				className = className + '\' + activeSheet.Classname()
			End if
		End if
	End if
	
	string path
	// If the currentObject has a parentObject then recursively
	// call this function to keep walking up the parents path.
	if IsValid(parentObject) Then
		path = GetPath(parentObject) 
	ELSE
		// If the parentObject is still not valid then
		// the top most object is the Application object
		PATH = GetApplication().AppName
	END IF 
	
	//In the case of mdi sheets, they are not reported as the parentwindow of popups and other types of windows.
	//That is why the code above, appends the activesheets classname to the name of the MDI frame
	//This is great except when it's the mdi sheet that is the top most window. In that case the sheets classname gets
	//duplicated in the path. So, we look for it and if it's found we don't append it again.
	if LastPos(path,classname) > 0 then
		return path
	end if
	return path + "\" + className

catch (RuntimeError ex)	
	return path+  '\exception'
End try
end function

on n_objecthelper.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_objecthelper.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

