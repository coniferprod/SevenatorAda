with Helpers; use Helpers;

-- The various parts of the data model are split into
-- child packages. "If your root pkg needs the child
-- in its spec then you have a design problem." (Jeffrey R. Carter)

package DX7 is

   Parse_Error : exception;

end DX7;
