--  Abstract :
--
--  Types and operations shared by Ada and Ada_Emacs outputs.
--
--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.Packrat;
with WisiToken_Grammar_Runtime;
package WisiToken.BNF.Output_Ada_Common is

   function To_Token_Ada_Name (WY_Name : in String) return String;

   type Common_Data is limited record
      --  Validated versions of Tuple values
      Generate_Algorithm : WisiToken.BNF.Valid_Generate_Algorithm;
      Lexer              : Lexer_Type; --  'none' valid for Libadalang
      Output_Language    : Ada_Output_Language;
      Interface_Kind     : Valid_Interface;
      Text_Rep           : Boolean;

      Lower_File_Name_Root : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Initialize
     (Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Type;
      Tuple             : in Generate_Tuple;
      Output_File_Root  : in String;
      Check_Interface   : in Boolean)
     return Common_Data;

   function File_Name_To_Ada (File_Name : in String) return String;

   procedure Create_Ada_Actions_Spec
     (Output_File_Name :         in String;
      Package_Name     :         in String;
      Input_Data       :         in WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data      :         in Output_Ada_Common.Common_Data;
      Generate_Data    : aliased in WisiToken.BNF.Generate_Utils.Generate_Data);

   procedure Create_Ada_Main_Spec
     (Output_File_Name  : in String;
      Main_Package_Name : in String;
      Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data       : in Output_Ada_Common.Common_Data)
   with Pre => Common_Data.Generate_Algorithm /= External;

   procedure Create_External_Main_Spec
     (Main_Package_Name    : in String;
      Tuple                : in Generate_Tuple;
      Input_Data           : in WisiToken_Grammar_Runtime.User_Data_Type);

   procedure LR_Create_Create_Parser
     (Input_Data    :         in     WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data);
   --  If not Common_Data.Text_Rep, includes LR parse table in generated
   --  source. Otherwise, includes call to LR.Get_Text_Rep; caller must
   --  call Put_Text_Rep to create file.

   procedure Packrat_Create_Create_Parser
     (Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data;
      Packrat_Data  :         in     WisiToken.Generate.Packrat.Data);

   procedure External_Create_Create_Grammar
     (Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data);

   procedure Create_re2c
     (Input_Data            :         in WisiToken_Grammar_Runtime.User_Data_Type;
      Tuple                 :         in Generate_Tuple;
      Generate_Data         : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
      Output_File_Name_Root :         in String);
   --  Create_re2c is called from wisitoken-bnf-generate, which does not declare
   --  Common_Data.

end WisiToken.BNF.Output_Ada_Common;
