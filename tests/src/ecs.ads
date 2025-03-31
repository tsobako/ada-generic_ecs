with Components;
with Resources;
with Generic_ECS;

package ECS is new Generic_ECS (Component_Kind_Type => Components.Component_Kind_Type, Resource_Kind_Type => Resources.Resource_Kind_Type);
