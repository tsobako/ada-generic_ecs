generic
   type Resource_Kind_Type is (<>);
package Internal.Generic_Resource with
  Preelaborate, Pure
is

   type Resource_Interface_Type is interface;
   type Resource_Interface_Class_Access_Type is
     access all Resource_Interface_Type'Class;

end Internal.Generic_Resource;
