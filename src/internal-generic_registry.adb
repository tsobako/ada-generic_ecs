package body Internal.Generic_Registry is

   --------------------------
   -- Registry management --
   --------------------------

   function Initialize return Registry_Type is
   begin
      return Registry : Registry_Type do
         Registry.Last_Entity := Entity_Type'First;
         Registry.Entities    := Entity_Component_Hashed_Maps.Empty_Map;
         Registry.Resources   := Resource_Hashed_Maps.Empty_Map;
         Registry.Entities_To_Delete := Entity_Hash_Set.Empty_Set;
      end return;
   end Initialize;

   procedure Clear (Registry : in out Registry_Type) is
   begin
      Registry.Entities.Clear;
      Registry.Entities_To_Delete.Clear;
   end Clear;

   function Count
     (Registry : Registry_Type)
      return Natural
   is
   begin
      return Natural (Registry.Entities.Length);
   end Count;

   -----------------------
   -- Entity management --
   -----------------------

   function Create
     (Registry : in out Registry_Type)
      return Entity_Type
   is
      Entity : constant Entity_Type := Registry.Last_Entity;
   begin
      Registry.Entities.Include (Entity, (others => null));
      Registry.Last_Entity := Entity_Type'Succ (Registry.Last_Entity);

      return Entity;
   end Create;

   procedure Destroy
     (Registry : in out Registry_Type;
      Entity   :        Entity_Type)
   is
   begin
      Registry.Entities_To_Delete.Include(Entity);
   end Destroy;

   function Has
     (Registry : Registry_Type;
      Entity   : Entity_Type)
      return Boolean
   is
   begin
      return Registry.Entities.Contains (Entity) and then not Registry.Entities_To_Delete.Contains (Entity);
   end Has;

   -- Remove marked to delete entities
   procedure Maintain
      (Registry: in out Registry_Type)
   is
   begin
      for E of Registry.Entities_To_Delete loop
         Registry.Entities.Delete (E);
      end loop;
      Registry.Entities_To_Delete.Clear;
   end Maintain;

   --------------------------
   -- Component management --
   --------------------------

   procedure Set
     (Registry  : in out Registry_Type;
      Entity    :        Entity_Type;
      Kind      :        Component_Package.Component_Kind_Type;
      Component :        Component_Package.Component_Interface_Class_Access_Type)
   is
   begin
      if not Registry.Has (Entity) then
         return;
      end if;

      Registry.Entities (Entity) (Kind) := Component;
   end Set;

   function Set
     (Registry  : in out Registry_Type;
      Entity    :        Entity_Type;
      Kind      :        Component_Package.Component_Kind_Type;
      Component :        Component_Package.Component_Interface_Class_Access_Type)
      return Component_Package.Component_Interface_Class_Access_Type
   is
   begin
      Registry.Set
        (Entity    => Entity,
         Kind      => Kind,
         Component => Component);

      return Registry.Get
          (Entity    => Entity,
           Component => Kind);
   end Set;

   procedure Unset
     (Registry  : in out Registry_Type;
      Entity    :        Entity_Type;
      Component :        Component_Package.Component_Kind_Type)
   is
   begin
      if not Registry.Has (Entity) then
         return;
      end if;

      Registry.Entities (Entity) (Component) := null;
   end Unset;

   procedure Unset
     (Registry   : in out Registry_Type;
      Entity     :        Entity_Type;
      Components :        Selection_Package.Selection_Type)
   is
      procedure Unset_Components is
      begin
         for Kind in Component_Package.Component_Kind_Type loop
            if Components.Is_Selected (Kind) then
               Registry.Entities (Entity) (Kind) := null;
            end if;
         end loop;
      end Unset_Components;
   begin
      if not Registry.Has (Entity) then
         return;
      end if;

      case Components.Selection_Kind is
         when Selection_Package.Inclusive =>
            Unset_Components;
         when Selection_Package.Exact =>
            Unset_Components;
         when Selection_Package.Exclusive =>
            if Registry.Has (Entity, Components) then
               Unset_Components;
            end if;
      end case;
   end Unset;

   procedure Unset
     (Registry   : in out Registry_Type;
      Components :        Selection_Package.Selection_Type)
   is
   begin
      for Entity_Cursor in Registry.Entities.Iterate loop
         Registry.Unset (Entity_Component_Hashed_Maps.Key (Entity_Cursor), Components);
      end loop;
   end Unset;

   function Has
     (Registry  : Registry_Type;
      Entity    : Entity_Type;
      Component : Component_Package.Component_Kind_Type)
      return Boolean
   is
      use type Component_Package.Component_Interface_Class_Access_Type;
   begin
      if not Registry.Has (Entity) then
         return False;
      end if;

      return Registry.Entities (Entity) (Component) /= null;
   end Has;

   function Has
     (Registry   : Registry_Type;
      Entity     : Entity_Type;
      Components : Selection_Package.Selection_Type)
      return Boolean
   is
      use type Selection_Package.Selection_Type;

      Entity_Components : Component_Package.Component_Boolean_Array_Type;
   begin
      if not Registry.Has (Entity) then
         return False;
      end if;

      Entity_Components := Registry.Get_Set_Components (Entity);

      return Components = Entity_Components;
   end Has;

   function Get
     (Registry  : Registry_Type;
      Entity    : Entity_Type;
      Component : Component_Package.Component_Kind_Type)
      return Component_Package.Component_Interface_Class_Access_Type
   is
   begin
      if not Registry.Has (Entity) then
         return null;
      end if;

      return Registry.Entities (Entity) (Component);
   end Get;

   function Get_Set_Components
     (Registry : Registry_Type;
      Entity   : Entity_Type)
      return Component_Package.Component_Boolean_Array_Type
   is
      Components : Component_Package.Component_Boolean_Array_Type := (others => False);
   begin
      if not Registry.Has (Entity) then
         return Components;
      end if;

      for Component_Kind in Component_Package.Component_Kind_Type loop
         Components (Component_Kind) := Registry.Has (Entity, Component_Kind);
      end loop;

      return Components;
   end Get_Set_Components;

   ------------
   -- System --
   ------------
   procedure Each
     (Registry   : in out Registry_Type;
      Components :        Selection_Package.Selection_Type;
      System     :        System_Type)
   is
      Entity : Entity_Type;
   begin
      for Cursor in Registry.Entities.Iterate loop
         Entity := Entity_Component_Hashed_Maps.Key (Cursor);
         if Registry.Has (Entity, Components) then
            System (Registry, Entity);
         end if;
      end loop;
   end Each;

   procedure Each
     (Registry : in out Registry_Type;
      System   :        System_Type)
   is
   begin
      for Cursor in Registry.Entities.Iterate loop
         System (Registry, Entity_Component_Hashed_Maps.Key (Cursor));
      end loop;
   end Each;

   procedure Each
     (Registry   : in out Registry_Type;
      Components :        Selection_Package.Selection_Type;
      System     : in out System_Interface_Type'Class)
   is
      Entity : Entity_Type;
   begin
      for Cursor in Registry.Entities.Iterate loop
         Entity := Entity_Component_Hashed_Maps.Key (Cursor);
         if Registry.Has (Entity, Components) then
            System.Run (Registry'Unchecked_Access, Entity);
         end if;
      end loop;
   end Each;

   procedure Each
     (Registry : in out Registry_Type;
      System   : in out System_Interface_Type'Class)
   is
   begin
      for Cursor in Registry.Entities.Iterate loop
         System.Run (Registry'Unchecked_Access, Entity_Component_Hashed_Maps.Key (Cursor));
      end loop;
   end Each;

      -------------------------
   -- Resource management --
   -------------------------

   -- Assign the resource to the registry
   procedure Add_Resource
     (Registry  : in out Registry_Type;
      Resourse_Type    :        Resource_Package.Resource_Kind_Type;
      Resource      :        Resource_Package.Resource_Interface_Class_Access_Type) is
   begin
      Resource_Hashed_Maps.Include (Registry.Resources, Resourse_Type, Resource);
   end Add_Resource;


   -- Return an access to the resource specified by kind
   function Get_Resource
     (Registry  : Registry_Type;
      Resourse_Type    :        Resource_Package.Resource_Kind_Type)
      return Resource_Package.Resource_Interface_Class_Access_Type is begin
      return Resource_Hashed_Maps.Element (Registry.Resources, Resourse_Type);
   end;

   
   -- Return whether or not the registry has the resource
   function Has_Resource
     (Registry : Registry_Type;
      Resourse_Type    :        Resource_Package.Resource_Kind_Type)
      return Boolean is begin
      return Resource_Hashed_Maps.Contains (Registry.Resources, Resourse_Type);
   end Has_Resource;

   -- Remove the resource
   -- Do nothing if the resource was already removed or doesn't exist
   procedure Remove_Resource
     (Registry : in out Registry_Type;
      Resourse_Type    :        Resource_Package.Resource_Kind_Type) is begin

      Resource_Hashed_Maps.Exclude (Registry.Resources, Resourse_Type);
   end Remove_Resource;


   -------------
   -- Private --
   -------------

   function Hash_Entity
     (Entity : Entity_Type)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (Entity);
   end Hash_Entity;

   function Hash_Resource_Type
     (Resource_Kind :  Resource_Package.Resource_Kind_Type)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (Resource_Package.Resource_Kind_Type'Pos(Resource_Kind));
   end Hash_Resource_Type;

end Internal.Generic_Registry;
