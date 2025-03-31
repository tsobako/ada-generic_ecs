with ECS;

package Resources.Level is

   type Level_Resource_Type is new ECS.Resource_Interface_Type with record
      Value: Positive;
   end record;

   type Level_Resource_Access_Type is access all Level_Resource_Type;

end Resources.Level;
