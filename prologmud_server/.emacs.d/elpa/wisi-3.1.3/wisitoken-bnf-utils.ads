--  Abstract :
--
--  Utilities for generating source code from BNF source files
--
--  Copyright (C) 2012, 2013, 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package WisiToken.BNF.Utils is

   function Strip_Quotes (Item : in String) return String;
   --  Remove leading and trailing '"', if any.

   function Strip_Parens (Item : in String) return String;
   --  Remove leading and trailing '()', if any.

end WisiToken.BNF.Utils;
