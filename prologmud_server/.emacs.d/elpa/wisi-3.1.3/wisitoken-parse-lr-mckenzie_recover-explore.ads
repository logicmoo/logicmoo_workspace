--  Abstract :
--
--  Code to explore parse table, enqueuing new configs to check.
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

with WisiToken.Parse.LR.McKenzie_Recover.Base;
private package WisiToken.Parse.LR.McKenzie_Recover.Explore is

   procedure Process_One
     (Super         : not null access Base.Supervisor;
      Shared        : not null access Base.Shared;
      Config_Status : out             Base.Config_Status);

end WisiToken.Parse.LR.McKenzie_Recover.Explore;
