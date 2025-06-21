with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

package body DX7.XML is

   function Element (Name : String; Attributes : Attributes_Type) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Append (Result, +"<");
      Append (Result, +Name);
      Append (Result, +" ");

      for Attr in Attributes.Iterate loop
         Append (Result, Attribute_Maps.Key (Attr) & "=""" & Attributes (Attr) & """ ");
      end loop;

      Append (Result, +">");
      return Result;
   end Element;

   function Element (Name : String; Attributes : Attributes_Type; Content : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := Element (Name, Attributes);
      Append (Result, Content);
      return Result;
   end Element;

   function To_String (Value : Integer) return String is
      Result : String (1 .. Value'Image'Length);
   begin
      Ada.Integer_Text_IO.Put (To => Result, Item => Value);
      return Result;
   end To_String;

   function To_String (Value : Boolean) return String is
   begin
      return Ada.Characters.Handling.To_Lower (Value'Image);
   end To_String;
end DX7.XML;
