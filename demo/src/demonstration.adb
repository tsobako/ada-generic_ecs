with Ada.Text_IO;
with Ada.Calendar;
with Generic_ECS;

procedure Demonstration is
   type Component_Kind_Type is (Position_Kind);
   type Resource_Kind_Type is (Time_Kind);
   --   if you are not planning to use any resource, declare resource kind as empty range
   --   type Resource_Kind_Type is range 1 .. 0;
   --   But adding/updating/deleting any resource will cause Constraint_Error;
   package ECS is new Generic_ECS (Component_Kind_Type, Resource_Kind_Type);

   type Position_Type is new ECS.Component_Interface_Type with record
      X, Y, Z : Float;
   end record;
   type Position_Access_Type is access all Position_Type;


   type Time_Type is new ECS.Resource_Interface_Type with record
      Seconds: Natural;
   end record;
   type Time_Access_Type is access all Time_Type;

   procedure System
     (Registry : ECS.Registry_Type;
      Entity   : ECS.Entity_Type)
   is
      Position : constant Position_Access_Type := Position_Access_Type (Registry.Get (Entity, Position_Kind));
      Current_Time : constant Time_Access_Type := Time_Access_Type (Registry.Get_Resource(Time_Kind));
   begin
      Ada.Text_IO.Put_Line ("X at time" & Current_Time.Seconds'Image & " :" & Position.X'Image);
   end System;

   Registry : ECS.Registry_Type := ECS.Initialize;
begin

   --  Add time resource
   declare
      Current_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Time : constant Time_Access_Type := new Time_Type;
   begin
      Time.Seconds := Natural(Ada.Calendar.Seconds(Current_Time));
      Registry.Add_Resource(Time_Kind, ECS.Resource_Interface_Class_Access_Type(Time));
   end;

   --  Create 5 entities
   for Index in 1 .. 5 loop
      declare
         Entity   : constant ECS.Entity_Type      := Registry.Create;
         Position : constant Position_Access_Type := new Position_Type;
      begin
         Position.X := Float (Index);

         --  Set the component only for odd indexes
         if Index mod 2 = 1 then
            Registry.Set (Entity, Position_Kind, ECS.Component_Interface_Class_Access_Type (Position));
         end if;
      end;
   end loop;

   --  Print x for each entities implementing the Position component
   declare
      Selection : ECS.Selection.Selection_Type;
   begin
      Selection.Select_Component (Position_Kind);
      Selection.Selection_Kind (ECS.Selection.Inclusive);

      Registry.Each (Selection, System'Access);
   end;
end Demonstration;
