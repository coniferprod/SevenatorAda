-- Child package of DX7 for envelope generator definitions
package DX7.Envelopes is
   -- Define type for level. Also define an array index
   -- for an array of level values, to be used in an envelope.
   subtype Level_Type is Integer range 0 .. 99;
   subtype Level_Index is Integer range 1 .. 4;
   type Level_Array is array (Level_Index) of Level_Type;

   -- Similarly to level, define range, array type and array index.
   subtype Rate_Type is Integer range 0 .. 99;
   subtype Rate_Index is Integer range 1 .. 4;
   type Rate_Array is array (Rate_Index) of Rate_Type;

   -- An envelope is represented by rates and levels.
   type Envelope_Type is record
      Rates  : Rate_Array := (others => 99); -- 99,99,99,99
      Levels : Level_Array := (4 => 0, others => 99); -- 99,99,99,0
   end record;

   -- MIDI System Exclusive data length of envelope
   Envelope_Data_Length : constant := Rate_Array'Length + Level_Array'Length;

   subtype Envelope_Data_Type is Byte_Array (1 .. Envelope_Data_Length);

   -- Gets the MIDI System Exclusive data for an envelope.
   procedure Emit (Envelope : in Envelope_Type; Data : out Envelope_Data_Type);

   -- Makes an envelope with random parameters.
   function Random_Envelope return Envelope_Type;

   procedure Parse_Envelope (Data : in Envelope_Data_Type; Result : out Envelope_Type);
   procedure New_Parse_Envelope (Data : in Byte_Array; Result : out Envelope_Type);

   Init_Envelope : constant Envelope_Type := (others => <>);
   Init_Pitch_Envelope : constant Envelope_Type :=
      (Levels => (50, 50, 50, 50), others => <>);
end DX7.Envelopes;
