--  Abstract :
--
--  Image with auxiliary data for instantiations of parent.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

generic
   type Aux_Data (<>) is private;
   with function Element_Image (Item : in Element_Type; Aux : in Aux_Data) return String;
function SAL.Gen_Unbounded_Definite_Queues.Gen_Image_Aux (Item : in Queue; Aux : in Aux_Data) return String;
