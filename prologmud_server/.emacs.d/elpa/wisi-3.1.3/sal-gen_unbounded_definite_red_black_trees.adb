--  Abstract :
--
--  Generic unbounded red-black tree with definite elements.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

package body SAL.Gen_Unbounded_Definite_Red_Black_Trees is

   --  Local declarations (alphabetical order)

   function Count_Tree (Item : in Node_Access; Nil : in Node_Access) return Ada.Containers.Count_Type
   with Pre => Nil /= null;

   procedure Delete_Fixup (T : in out Tree; X : in out Node_Access);

   function Find (Root : in Node_Access; Key : in Key_Type; Nil : in Node_Access) return Node_Access
   with Pre => Nil /= null;

   procedure Left_Rotate (Tree : in out Pkg.Tree; X : in Node_Access)
   with Pre => X /= null;

   procedure Right_Rotate (Tree : in out Pkg.Tree; X : in Node_Access)
   with Pre => X /= null;

   procedure Transplant (T : in out Pkg.Tree; U, V : in Node_Access)
   with Pre => U /= null and T.Root /= null;

   ----------
   --  local bodies (alphabetical order)

   function Count_Tree (Item : in Node_Access; Nil : in Node_Access) return Ada.Containers.Count_Type
   is
      use all type Ada.Containers.Count_Type;
      Result : Ada.Containers.Count_Type := 0;
   begin
      if Item.Left /= Nil then
         Result := Result + Count_Tree (Item.Left, Nil);
      end if;

      if Item.Right /= Nil then
         Result := Result + Count_Tree (Item.Right, Nil);
      end if;

      return Result + 1;
   end Count_Tree;

   procedure Delete_Fixup (T : in out Tree; X : in out Node_Access)
   is
      W : Node_Access;
   begin
      --  [1] 13.3 RB-Delete-Fixup
      --  X is either "doubly black" or "red and black"
      --  X.Parent is set, even if X = Nil.
      --  In all cases, Nil.Left = null.Right = null.

      while X /= T.Root and X.Color = Black loop
         if X = X.Parent.Left then
            W := X.Parent.Right;
            if W.Color = Red then
               W.Color        := Black;
               X.Parent.Color := Red;
               Left_Rotate (T, X.Parent);
               W              := X.Parent.Right;
            end if;

            if W.Left.Color = Black and W.Right.Color = Black then
               W.Color := Red;
               X := X.Parent;
            else
               if W.Right.Color = Black then
                  W.Left.Color := Black;
                  W.Color      := Red;
                  Right_Rotate (T, W);
                  W            := X.Parent.Right;
               end if;
               W.Color        := X.Parent.Color;
               X.Parent.Color := Black;
               W.Right.Color  := Black;
               Left_Rotate (T, X.Parent);
               X              := T.Root;
            end if;
         else
            W := X.Parent.Left;
            if W.Color = Red then
               W.Color        := Black;
               X.Parent.Color := Red;
               Right_Rotate (T, X.Parent);
               W              := X.Parent.Left;
            end if;

            if W.Right.Color = Black and W.Left.Color = Black then
               W.Color := Red;
               X       := X.Parent;
            else
               if W.Left.Color = Black then
                  W.Right.Color := Black;
                  W.Color       := Red;
                  Left_Rotate (T, W);
                  W             := X.Parent.Left;
               end if;
               W.Color        := X.Parent.Color;
               X.Parent.Color := Black;
               W.Left.Color   := Black;
               Right_Rotate (T, X.Parent);
               X              := T.Root;
            end if;
         end if;
      end loop;
      X.Color := Black;
   end Delete_Fixup;

   function Find (Root : in Node_Access; Key : in Key_Type; Nil : in Node_Access) return Node_Access
   is
      Node : Node_Access := Root;
   begin
      while Node /= Nil loop
         case Key_Compare (Key, Pkg.Key (Node.Element)) is
         when Equal =>
            return Node;
         when Less =>
            Node := Node.Left;
         when Greater =>
            Node := Node.Right;
         end case;
      end loop;
      return null;
   end Find;

   procedure Free_Tree (Item : in out Node_Access; Nil : in Node_Access)
   is begin
      if Item = Nil or Item = null then
         raise Programmer_Error;
      end if;

      if Item.Left /= Nil then
         Free_Tree (Item.Left, Nil);
      end if;

      if Item.Right /= Nil then
         Free_Tree (Item.Right, Nil);
      end if;

      Free (Item);
   end Free_Tree;

   procedure Insert_Fixup (Tree : in out Pkg.Tree; Z : in out Node_Access)
   is
      --  [1] 13.3 RB-Insert-Fixup (T, z)
      Nil : Node_Access renames Tree.Nil;
      Y   : Node_Access;
   begin
      while Z.Parent /= Nil and then Z.Parent.Color = Red loop
         if Z.Parent = Z.Parent.Parent.Left then
            Y := Z.Parent.Parent.Right;
            if Y /= Nil and then Y.Color = Red then
               Z.Parent.Color        := Black;
               Y.Color               := Black;
               Z.Parent.Parent.Color := Red;
               Z                     := Z.Parent.Parent;
            else
               if Z = Z.Parent.Right then
                  Z := Z.Parent;
                  Left_Rotate (Tree, Z);
               end if;
               Z.Parent.Color        := Black;
               Z.Parent.Parent.Color := Red;
               Right_Rotate (Tree, Z.Parent.Parent);
            end if;
         else
            Y := Z.Parent.Parent.Left;
            if Y /= Nil and then Y.Color = Red then
               Z.Parent.Color        := Black;
               Y.Color               := Black;
               Z.Parent.Parent.Color := Red;
               Z                     := Z.Parent.Parent;
            else
               if Z = Z.Parent.Left then
                  Z := Z.Parent;
                  Right_Rotate (Tree, Z);
               end if;
               Z.Parent.Color        := Black;
               Z.Parent.Parent.Color := Red;
               Left_Rotate (Tree, Z.Parent.Parent);
            end if;
         end if;
      end loop;
      Tree.Root.Color := Black;
   end Insert_Fixup;

   procedure Left_Rotate (Tree : in out Pkg.Tree; X : in Node_Access)
   is
      --  [1] 13.2 Left-Rotate (T, x)
      Nil : Node_Access renames Tree.Nil;
      Y   : constant Node_Access := X.Right;
   begin
      X.Right := Y.Left;
      if Y.Left /= Nil then
         Y.Left.Parent := X;
      end if;
      Y.Parent := X.Parent;
      if X.Parent = Nil then
         Tree.Root := Y;
      elsif X = X.Parent.Left then
         X.Parent.Left := Y;
      else
         X.Parent.Right := Y;
      end if;
      Y.Left   := X;
      X.Parent := Y;
   end Left_Rotate;

   function Minimum (Node : in Node_Access; Nil : in Node_Access) return Node_Access
   is begin
      return Result : Node_Access := Node
      do
         while Result.Left /= Nil loop
            Result := Result.Left;
         end loop;
      end return;
   end Minimum;

   procedure Right_Rotate (Tree : in out Pkg.Tree; X : in Node_Access)
   is
      --  [1] 13.2 Right-Rotate (T, x)
      Nil : Node_Access renames Tree.Nil;
      Y   : constant Node_Access := X.Left;
   begin
      X.Left := Y.Right;
      if Y.Right /= Nil then
         Y.Right.Parent := X;
      end if;
      Y.Parent := X.Parent;
      if X.Parent = Nil then
         Tree.Root := Y;
      elsif X = X.Parent.Right then
         X.Parent.Right := Y;
      else
         X.Parent.Left := Y;
      end if;
      Y.Right  := X;
      X.Parent := Y;
   end Right_Rotate;

   procedure Transplant (T : in out Pkg.Tree; U, V : in Node_Access)
   is
      Nil : Node_Access renames T.Nil;
   begin
      --  [1] 13.4 RB-Transplant, 12.3 Transplant

      if U.Parent = Nil then
         T.Root := V;
      elsif U = U.Parent.Left then
         U.Parent.Left := V;
      else
         U.Parent.Right := V;
      end if;
      V.Parent := U.Parent;
   end Transplant;

   ----------
   --  Public subprograms, spec order

   overriding procedure Finalize (Object : in out Tree)
   is begin
      if Object.Root /= null then
         if Object.Root = Object.Nil then
            Free (Object.Nil);
            Object.Root := null;
         else
            Free_Tree (Object.Root, Object.Nil);
            Free (Object.Nil);
         end if;
      end if;
   end Finalize;

   overriding procedure Initialize (Object : in out Tree)
   is begin
      Object.Nil       := new Node;
      Object.Nil.Color := Black;
      Object.Root      := Object.Nil;
   end Initialize;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean
   is begin
      return Cursor.Node /= null;
   end Has_Element;

   function Constant_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Node.all.Element'Access, Dummy => 1);
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Constant_Reference_Type
   is
      Node : constant Node_Access := Find (Container.Root, Key, Container.Nil);
   begin
      if Node = null then
         raise Not_Found;
      else
         --  WORKAROUND: GNAT Community 2019 requires .all here, GNAT Pro 21.0w
         --  20200426 requires it _not_ be here. The code is technically legal
         --  either way, so both compilers have a bug. Keeping .all for now;
         --  just delete it if you are using 21.0w. Hopefully 21 will fix the
         --  bug. AdaCore ticket T503-001 on Eurocontrol support contract.
         return (Element => Node.all.Element'Access, Dummy => 1);
      end if;
   end Constant_Reference;

   function Variable_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Variable_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      --  WORKAROUND: see note in Constant_Reference
      return (Element => Position.Node.all.Element'Access, Dummy => 1);
   end Variable_Reference;

   function Variable_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Variable_Reference_Type
   is
      Node : constant Node_Access := Find (Container.Root, Key, Container.Nil);
   begin
      if Node = null then
         raise Not_Found;
      else
         --  WORKAROUND: see note in Constant_Reference
         return (Element => Node.all.Element'Access, Dummy => 1);
      end if;
   end Variable_Reference;

   function Iterate (Tree : in Pkg.Tree'Class) return Iterator
   is begin
      return (Tree.Root, Tree.Nil);
   end Iterate;

   overriding function First (Iterator : in Pkg.Iterator) return Cursor
   is
      Nil  : Node_Access renames Iterator.Nil;
      Node : Node_Access := Iterator.Root;
   begin
      if Node = Nil then
         return
           (Node       => null,
            Direction  => Unknown,
            Left_Done  => True,
            Right_Done => True);
      else
         loop
            exit when Node.Left = Nil;
            Node := Node.Left;
         end loop;

         return
           (Node       => Node,
            Direction  => Ascending,
            Left_Done  => True,
            Right_Done => False);
      end if;

   end First;

   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   is
      Nil : Node_Access renames Iterator.Nil;
   begin
      if Position.Direction /= Ascending then
         raise Programmer_Error;
      end if;

      if Position.Node = null then
         return
           (Node       => null,
            Direction  => Unknown,
            Left_Done  => True,
            Right_Done => True);

      elsif Position.Left_Done or Position.Node.Left = Nil then
         if Position.Right_Done or Position.Node.Right = Nil then
            if Position.Node.Parent = Nil then
               return
                 (Node       => null,
                  Direction  => Unknown,
                  Left_Done  => True,
                  Right_Done => True);
            else
               declare
                  Node : constant Node_Access := Position.Node.Parent;
                  Temp : constant Cursor      :=
                    (Node       => Node,
                     Direction  => Ascending,
                     Left_Done  => Node.Right = Position.Node or Node.Left = Position.Node,
                     Right_Done => Node.Right = Position.Node);
               begin
                  if Temp.Right_Done then
                     return Next (Iterator, Temp);
                  else
                     return Temp;
                  end if;
               end;
            end if;
         else
            declare
               Node : constant Node_Access := Position.Node.Right;
               Temp : constant Cursor      :=
                 (Node       => Node,
                  Direction  => Ascending,
                  Left_Done  => Node.Left = Nil,
                  Right_Done => False);
            begin
               if Temp.Left_Done then
                  return Temp;
               else
                  return Next (Iterator, Temp);
               end if;
            end;
         end if;
      else
         declare
            Node : constant Node_Access := Position.Node.Left;
            Temp : constant Cursor      :=
              (Node       => Node,
               Direction  => Ascending,
               Left_Done  => Node.Left = Nil,
               Right_Done => False);
         begin
            if Temp.Left_Done then
               return Temp;
            else
               return Next (Iterator, Temp);
            end if;
         end;
      end if;
   end Next;

   overriding function Last (Iterator : in Pkg.Iterator) return Cursor
   is
      Nil  : Node_Access renames Iterator.Nil;
      Node : Node_Access := Iterator.Root;
   begin
      if Node = Nil then
         return
           (Node       => null,
            Direction  => Unknown,
            Left_Done  => True,
            Right_Done => True);
      else
         loop
            exit when Node.Right = Nil;
            Node := Node.Right;
         end loop;
         return
           (Node       => Node,
            Direction  => Descending,
            Right_Done => True,
            Left_Done  => False);
      end if;
   end Last;

   overriding function Previous (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   is
      Nil : Node_Access renames Iterator.Nil;
   begin
      if Position.Direction /= Descending then
         raise Programmer_Error;
      end if;

      if Position.Node = null then
         return
           (Node       => null,
            Direction  => Unknown,
            Left_Done  => True,
            Right_Done => True);

      elsif Position.Right_Done or Position.Node.Right = Nil then
         if Position.Left_Done or Position.Node.Left = Nil then
            if Position.Node.Parent = Nil then
               return
                 (Node       => null,
                  Direction  => Unknown,
                  Left_Done  => True,
                  Right_Done => True);
            else
               declare
                  Node : constant Node_Access := Position.Node.Parent;
                  Temp : constant Cursor      :=
                    (Node       => Node,
                     Direction  => Descending,
                     Right_Done => Node.Left = Position.Node or Node.Right = Position.Node,
                     Left_Done  => Node.Left = Position.Node);
               begin
                  if Temp.Left_Done then
                     return Previous (Iterator, Temp);
                  else
                     return Temp;
                  end if;
               end;
            end if;
         else
            declare
               Node : constant Node_Access := Position.Node.Left;
               Temp : constant Cursor      :=
                 (Node       => Node,
                  Direction  => Descending,
                  Right_Done => Node.Right = Nil,
                  Left_Done  => False);
            begin
               if Temp.Right_Done then
                  return Temp;
               else
                  return Previous (Iterator, Temp);
               end if;
            end;
         end if;
      else
         declare
            Node : constant Node_Access := Position.Node.Right;
            Temp : constant Cursor      :=
              (Node       => Node,
               Direction  => Descending,
               Right_Done => Node.Right = Nil,
               Left_Done  => False);
         begin
            if Temp.Right_Done then
               return Temp;
            else
               return Previous (Iterator, Temp);
            end if;
         end;
      end if;
   end Previous;

   function Previous (Iterator : in Pkg.Iterator; Key : in Key_Type) return Cursor
   is
      Nil  : Node_Access renames Iterator.Nil;
      Node : Node_Access := Iterator.Root;
   begin
      while Node /= Nil loop
         declare
            Current_Key : Key_Type renames Pkg.Key (Node.Element);
         begin
            case Key_Compare (Key, Current_Key) is
            when Equal =>
                  return Previous (Iterator, (Node, Descending, Right_Done => True, Left_Done => False));

            when Less =>
               if Node.Left = Nil then
                  return Previous (Iterator, (Node, Descending, Right_Done => True, Left_Done => True));
               else
                  Node := Node.Left;
               end if;

            when Greater =>
               if Node.Right = Nil then
                  return (Node, Descending, Right_Done => True, Left_Done => False);
               else
                  Node := Node.Right;
               end if;
            end case;
         end;
      end loop;

      return
        (Node       => null,
         Direction  => Unknown,
         Left_Done  => True,
         Right_Done => True);
   end Previous;

   function Find
     (Iterator  : in Pkg.Iterator;
      Key       : in Key_Type;
      Direction : in Direction_Type := Ascending)
     return Cursor
   is
      Nil  : Node_Access renames Iterator.Nil;
      Node : constant Node_Access := Find (Iterator.Root, Key, Nil);
   begin
      if Node = null then
         return
           (Node       => null,
            Direction  => Unknown,
            Left_Done  => True,
            Right_Done => True);
      else
         return
           (Node       => Node,
            Direction  => Direction,
            Left_Done  =>
              (case Direction is
               when Ascending | Unknown => True,
               when Descending => Node.Left = Nil),
            Right_Done =>
              (case Direction is
               when Ascending => Node.Right = Nil,
               when Descending | Unknown => True));
      end if;
   end Find;

   function Find_In_Range
     (Iterator    : in Pkg.Iterator;
      Direction   : in Known_Direction_Type;
      First, Last : in Key_Type)
     return Cursor
   is
      Nil       : Node_Access renames Iterator.Nil;
      Node      : Node_Access := Iterator.Root;
      Candidate : Node_Access := null; -- best result found so far
   begin
      while Node /= Nil loop
         declare
            Current_Key : Key_Type renames Key (Node.Element);
         begin
            case Direction is
            when Ascending =>
               case Key_Compare (First, Current_Key) is
               when Equal =>
                     return (Node, Ascending, Right_Done => False, Left_Done => True);

               when Less =>
                  if Node.Left = Nil then
                     case Key_Compare (Current_Key, Last) is
                     when Less | Equal =>
                        return (Node, Ascending, Right_Done => False, Left_Done => True);
                     when Greater =>
                        if Candidate = null then
                           return No_Element;
                        else
                           return (Candidate, Ascending, Right_Done => False, Left_Done => True);
                        end if;
                     end case;
                  else
                     case Key_Compare (Last, Current_Key) is
                     when Greater | Equal =>
                        Candidate := Node;
                     when Less =>
                        null;
                     end case;
                     Node := Node.Left;
                  end if;

               when Greater =>
                  if Node.Right = Nil then
                     if Candidate = null then
                        return No_Element;
                     else
                        return (Candidate, Ascending, Right_Done => False, Left_Done => True);
                     end if;
                  else
                     Node := Node.Right;
                  end if;
               end case;

            when Descending =>
               if Last = Current_Key then
                  return (Node, Descending, Right_Done => True, Left_Done => False);

               else
                  case Key_Compare (Last, Current_Key) is
                  when Greater =>
                     if Node.Right = Nil then
                        case Key_Compare (Current_Key, First) is
                        when Greater | Equal =>
                           return (Node, Descending, Right_Done => True, Left_Done => False);
                        when Less =>
                           if Candidate = null then
                              return No_Element;
                           else
                              return (Candidate, Ascending, Right_Done => False, Left_Done => True);
                           end if;
                        end case;
                     else
                        case Key_Compare (First, Current_Key) is
                        when Less | Equal =>
                           Candidate := Node;
                        when Greater =>
                           null;
                        end case;
                        Node := Node.Right;
                     end if;
                  when Equal | Less =>
                     if Node.Left = Nil then
                        if Candidate = null then
                           return No_Element;
                        else
                           return (Candidate, Ascending, Right_Done => False, Left_Done => True);
                        end if;
                     else
                        Node := Node.Left;
                     end if;
                  end case;
               end if;
            end case;
         end;
      end loop;

      return No_Element;
   end Find_In_Range;

   function Count (Tree : in Pkg.Tree) return Ada.Containers.Count_Type
   is begin
      if Tree.Root = Tree.Nil then
         return 0;
      else
         return Count_Tree (Tree.Root, Tree.Nil);
      end if;
   end Count;

   function Present (Container : in Tree; Key : in Key_Type) return Boolean
   is
      Nil  : Node_Access renames Container.Nil;
      Node : Node_Access := Container.Root;
   begin
      while Node /= Nil loop
         case Key_Compare (Key, Pkg.Key (Node.Element)) is
         when Equal =>
            return True;
         when Less =>
            Node := Node.Left;
         when Greater =>
            Node := Node.Right;
         end case;
      end loop;
      return False;
   end Present;

   function Insert (Tree : in out Pkg.Tree; Element : in Element_Type) return Cursor
   is
      --  [1] 13.3 RB-Insert (T, z)
      Nil   : Node_Access renames Tree.Nil;
      Z     : Node_Access       := new Node'(Element, Nil, Nil, Nil, Red);
      Key_Z : constant Key_Type := Key (Z.Element);
      Y     : Node_Access       := Nil;
      X     : Node_Access       := Tree.Root;

      Result      : Node_Access;
      Compare_Z_Y : Compare_Result;
   begin
      Nil.Parent := null;
      Nil.Left   := null;
      Nil.Right  := null;

      while X /= Nil loop
         Y := X;
         Compare_Z_Y := Key_Compare (Key_Z, Key (X.Element));
         case Compare_Z_Y is
         when Less =>
            X := X.Left;
         when Equal | Greater =>
            X := X.Right;
         end case;
      end loop;

      Z.Parent := Y;
      if Y = Nil then
         Tree.Root := Z;
      else
         case Compare_Z_Y is
         when Less =>
            Y.Left := Z;
         when Equal | Greater =>
            Y.Right := Z;
         end case;
      end if;

      Result := Z;
      if Z = Tree.Root then
         Z.Color := Black;
      else
         Insert_Fixup (Tree, Z);
      end if;

      return
        (Node       => Result,
         Direction  => Unknown,
         Left_Done  => True,
         Right_Done => True);
   end Insert;

   procedure Insert (Tree : in out Pkg.Tree; Element : in Element_Type)
   is
      Temp : Cursor := Insert (Tree, Element);
      pragma Unreferenced (Temp);
   begin
      null;
   end Insert;

   procedure Delete (Tree : in out Pkg.Tree; Position : in out Cursor)
   is
      Nil          : Node_Access renames Tree.Nil;
      T            : Pkg.Tree renames Tree;
      Z            : constant Node_Access :=
        (if Position.Node = null then raise Parameter_Error else Position.Node);
      Y            : Node_Access          := Z;
      Y_Orig_Color : Color                := Y.Color;
      X            : Node_Access;
   begin
      --  Catch logic errors in use of Nil
      Nil.Parent := null;
      Nil.Left   := null;
      Nil.Right  := null;

      --  [1] 13.4 RB-Delete.
      if Z.Left = Nil then
         X := Z.Right;
         Transplant (T, Z, Z.Right);

      elsif Z.Right = Nil then
         X := Z.Left;
         Transplant (T, Z, Z.Left);

      else
         Y            := Minimum (Z.Right, Nil);
         Y_Orig_Color := Y.Color;
         X            := Y.Right;
         if Y.Parent = Z then
            X.Parent := Y;
            --  This is already true unless X = Nil, in which case delete_fixup
            --  needs the info.
         else
            Transplant (T, Y, Y.Right);
            Y.Right := Z.Right;

            Y.Right.Parent := Y;
            --  This is already true unless Y.Right = Nil, in which case
            --  delete_fixup needs the info.
         end if;

         Transplant (T, Z, Y);
         Y.Left := Z.Left;

         Y.Left.Parent := Y;
         --  This is already true unless Y.Left = Nil, in which case
         --  delete_fixup needs the info.

         Y.Color := Z.Color;
      end if;

      if Y_Orig_Color = Black then
         Delete_Fixup (T, X);
      end if;

      Free (Position.Node);
   end Delete;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees;
