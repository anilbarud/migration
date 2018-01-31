$PBExportHeader$u_hawkdes.sru
$PBExportComments$Main user object for DES Data encyrption~r~nHawk Data Processing, Inc.
forward
global type u_hawkdes from nonvisualobject
end type
end forward

global type u_hawkdes from nonvisualobject
end type
global u_hawkdes u_hawkdes

type prototypes
function int CypherString(ref string action, ref char  s_Input[], ref char s_output[], int i_MaxBytes, ref char s_Key[],char c_Ascii,char c_Fillchar) library "hawkde32.dll" alias for "CypherString;Ansi"
function long CypherFile(ref string action, ref string  s_InputFile, ref string s_outputFile, ref char s_Key[],ref string s_KeepLth,ref string s_RemoveFile, char c_Ascii) library "hawkde32.dll" alias for "CypherFile;Ansi"
function uint GetProfileStringEnc(ref char s_Key[],ref string pszFileName,ref string pszSection,ref string pszEntry,ref string pszDefault,ref string pszOutput, int iMaxBytes) library "hawkde32.dll" alias for "GetProfileStringEnc;Ansi"
function uint WriteProfileStringEnc(ref char s_Key[],ref string pszFileName,ref string pszSection,ref string pszEntry,ref string pszInput, int iMaxBytes) library "hawkde32.dll" alias for "WriteProfileStringEnc;Ansi"

end prototypes

type variables
//-------------------------------------------------------------------------
// Common Attributes - must be set for both file
//                              and string encryption
//--------------------- ---------------------------------------------------
   string is_Action               // Set to "e" or "d" 
                                        // (encrypt or decrypt)
   char       ic_key[]                // Encryption Key
   boolean  ib_Encrypt_Ascii     // Encrypt to Ascii

//-------------------------------------------------------------------------
// Attributes for buffer encryption
//-------------------------------------------------------------------------
   char ic_Input_Buffer[]       
   char ic_Output_Buffer[]

//-------------------------------------------------------------------------
// File attributes - must be set for File encryption                             
//-------------------------------------------------------------------------
string is_Input_File                   // full path of input file
string is_output_file                  // Output file Name
boolean ib_Delete_Input=false  // remove input file
boolean ib_Keep_length =true   // Keep encrypted file the same
                                             // length as original  

//-------------------------------------------------------------------------
//Profile attributes                             
//-------------------------------------------------------------------------
int ii_Max_Bytes = 100
                     


end variables

forward prototypes
public function integer uf_cypher_file ()
public function string uf_profilestring (ref string as_filename, ref string as_section, ref string as_key, ref string as_default)
public function integer uf_setprofilestring (ref string as_filename, ref string as_section, ref string as_key, ref string as_value)
public function integer uf_cypher_buffer ()
end prototypes

public function integer uf_cypher_file ();/*------------------------------------------------------------------------------------
 -- uf_encrypt_file
 -- Used to encrypt/Decrypt a given file
 -- The following instance variables must be set to use this function
 --  is_action       = string set to "e" to encrypt "d" to decrypt
 --  ic_key          = 8 byte encryption key
 --  is_Input_File          // full path of input file
 --  is_output_file         // Output file Name
 --  ib_Delete_Input 		 // remove input file (default = false)
 --  ib_Keep_length         // Keep the original file length (default = true)
 --  ib_Encrypt_Ascii       // Convert the encrypted value to ascii
 --  Return value
 --  Number of bytes encyphered 
 -------------------------------------------------------------------------------------*/
string ls_keeplth
string ls_RemoveFile = "n"
char lc_Ascii = "n"

if not fileexists(is_input_file) then
	return -1
end if

if ib_Keep_length then
	ls_keeplth = 'y'
else
	ls_keeplth = 'n'
end if

if ib_Delete_input then
	ls_RemoveFile = 'y'
else
	ls_RemoveFile = 'n'
end if

if ib_Encrypt_Ascii then
	lc_Ascii = 'y'
else
	lc_Ascii = 'n'
end if



/*------------------------------------------------------------------------------------
 --	Call function to cypher file
-------------------------------------------------------------------------------------*/	
CypherFile(is_action , is_Input_File, is_output_file, ic_key[],ls_keeplth, ls_RemoveFile,lc_Ascii ) 
return 1

end function

public function string uf_profilestring (ref string as_filename, ref string as_section, ref string as_key, ref string as_default);/*------------------------------------------------------------------------------------
 -- uf_ProfileString
 -- Functions the same as the Powerbuilder function profile string except it
 -- applies the key value specified in ic_keythis.uf_profilestring
 -------------------------------------------------------------------------------------*/
string ls_Output
ls_output = space(ii_max_bytes)
GetProfileStringEnc(ic_key[],as_filename,as_section,as_key,as_default, ls_Output, ii_Max_Bytes) 
return ls_output


end function

public function integer uf_setprofilestring (ref string as_filename, ref string as_section, ref string as_key, ref string as_value);/*------------------------------------------------------------------------------------
 -- uf_SetProfileString
 -- Functions the same as the Powerbuilder function profile string except it
 -- applies the key value specified in ic_keythis.uf_profilestring
 -------------------------------------------------------------------------------------*/


WriteProfileStringEnc(ic_key[],as_filename,as_section,as_key, as_value, ii_Max_Bytes) 
return 1

end function

public function integer uf_cypher_buffer ();/*------------------------------------------------------------------------------------
 -- uf_encrypt_buffer
 -- Used to encrypt/Decrypt a given buffer
 -- The following instance variables must be set to use this function
 --  ic_input_buffer = Buffer to encrypt/decrypt
 --  is_action       = string set to "e" to encrypt "d" to decrypt
 --  ic_key          = 8 byte encryption key
 --  ic_Output_buffer= Cypher output is stored in this buffer
 --
 --  Return value
 --  Number of bytes encyphered 
 -------------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------------
 --	Variable declares and initialization
-------------------------------------------------------------------------------------*/

char lc_init[]
long ll_Output_Size
long ll_input_size
long ll_mult
int  li_rem
char lc_Ascii = "n"

char lc_fill_char 
/*------------------------------------------------------------------------------------
 --	Set for ascii encryption
-------------------------------------------------------------------------------------*/

if ib_Encrypt_Ascii then
	lc_Ascii = 'y'
end if

lc_fill_char = "~000"
/*------------------------------------------------------------------------------------
 --	Set OUtput size to nearest multiple of 8 >= to input size (DES works on 8 byte
 --   blocks
-------------------------------------------------------------------------------------*/
ll_input_size    = UpperBound(ic_input_buffer)
li_rem 			  = mod(ll_input_size,8)
if li_rem > 0 then
	ll_output_size   = ll_input_size + (8 - li_rem)
else
	ll_output_size   = ll_input_size
end if

if ib_Encrypt_Ascii and lower(is_action) = "e" then 
	ll_mult = ll_output_size / 3
   if mod(ll_output_size,3) > 0 then
		ll_mult = ll_mult + 1
	end if
	ll_output_size = ll_mult * 4
end if
ic_output_buffer[] = lc_init[]
ic_output_buffer[ll_output_size] = ' '

/*------------------------------------------------------------------------------------
 --	Call function to cypher block
-------------------------------------------------------------------------------------*/	
CypherString(is_action, ic_input_buffer[],ic_output_buffer[], ll_input_size,ic_key[],lc_ascii,lc_fill_char) 
return ll_Output_size

end function

on u_hawkdes.create
call super::create
TriggerEvent( this, "constructor" )
end on

on u_hawkdes.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

