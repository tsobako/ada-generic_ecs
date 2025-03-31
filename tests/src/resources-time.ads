with ECS;

package Resources.Time is

   type Time_Resource_Type is new ECS.Resource_Interface_Type with record
      Value: Natural;
   end record;

   type Time_Resource_Access_Type is access all Time_Resource_Type;

end Resources.Time;
