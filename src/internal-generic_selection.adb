package body Internal.Generic_Selection is

   function Select_None return Selection_Type is
   begin
      return (Components => (others => Excluded));
   end Select_None;

   function Select_All return Selection_Type is
   begin
      return (Components => (others => Included));
   end Select_All;

   function Select_Optional return Selection_Type is
   begin
      return (Components => (others => Optional));
   end Select_Optional;

   procedure Include_Component
     (Selection : out Selection_Type;
      Component :     Component_Package.Component_Kind_Type)
   is
   begin
      Selection.Components (Component) := Included;
   end Include_Component;

   procedure Exclude_Component
     (Selection : out Selection_Type;
      Component :     Component_Package.Component_Kind_Type)
   is
   begin
      Selection.Components (Component) := Excluded;
   end Exclude_Component;

   procedure Optional_Component
     (Selection : out Selection_Type;
      Component :     Component_Package.Component_Kind_Type)
   is
   begin
      Selection.Components (Component) := Optional;
   end Optional_Component;

   function Is_Included
     (Selection : Selection_Type;
      Component : Component_Package.Component_Kind_Type) return Boolean
   is
   begin
      return Selection.Components (Component) = Included;
   end Is_Included;

   function Is_Excluded
     (Selection : Selection_Type;
      Component : Component_Package.Component_Kind_Type) return Boolean
   is
   begin
      return Selection.Components (Component) = Excluded;
   end Is_Excluded;

   function Is_Optional
     (Selection : Selection_Type;
      Component : Component_Package.Component_Kind_Type) return Boolean
   is
   begin
      return Selection.Components (Component) = Optional;
   end Is_Optional;

   function "="
     (Selection        : Selection_Type;
      Components_Array : Component_Package.Component_Boolean_Array_Type)
      return Boolean
   is
      Valid : Boolean := True;
   begin
      for Kind in Component_Package.Component_Kind_Type loop
         case Selection.Components (Kind) is
            when Included =>
               if not Components_Array (Kind) then
                  Valid := False;
               end if;
            when Excluded =>
               if Components_Array (Kind) then
                  Valid := False;
               end if;
            when Optional =>
               null;
         end case;
      end loop;
      return Valid;
   end "=";

end Internal.Generic_Selection;
