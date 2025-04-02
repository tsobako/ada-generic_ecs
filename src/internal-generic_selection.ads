with Internal.Generic_Component;

generic
   with package Component_Package is new Internal.Generic_Component (<>);
package Internal.Generic_Selection with
  Preelaborate, Pure
is

   type Component_Selection_Kind_Type is
     (Included, --  Component is needed
      Excluded, --  Component is excluded
      Optional   --  Component is optional
   );

   type Selection_Type is tagged private;

   --  Return an object with no components selected
   function Select_None return Selection_Type;

   --  Return an object with every components selected
   function Select_All return Selection_Type;

   function Select_Optional return Selection_Type;

   --  Select a component
   procedure Include_Component
     (Selection : out Selection_Type;
      Component :     Component_Package.Component_Kind_Type);

   --  Unselect a component
   procedure Exclude_Component
     (Selection : out Selection_Type;
      Component :     Component_Package.Component_Kind_Type);

   procedure Optional_Component
     (Selection : out Selection_Type;
      Component :     Component_Package.Component_Kind_Type);

   --  Return whether the component is selected or not
   function Is_Included
     (Selection : Selection_Type;
      Component : Component_Package.Component_Kind_Type) return Boolean;

   --  Return whether the component is selected or not
   function Is_Excluded
     (Selection : Selection_Type;
      Component : Component_Package.Component_Kind_Type) return Boolean;

   --  Return whether the component is selected or not
   function Is_Optional
     (Selection : Selection_Type;
      Component : Component_Package.Component_Kind_Type) return Boolean;

   function "="
     (Selection        : Selection_Type;
      Components_Array : Component_Package.Component_Boolean_Array_Type)
      return Boolean;

private

   type Component_Selection_Array_Type is
     array
       (Component_Package
          .Component_Kind_Type) of Component_Selection_Kind_Type with
     Pack;

   type Selection_Type is tagged record
      Components : Component_Selection_Array_Type := (others => Optional);
   end record;

end Internal.Generic_Selection;
