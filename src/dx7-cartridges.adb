with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with DX7.Envelopes;

package body DX7.Cartridges is
   procedure Parse (Data : in Byte_Array; Result : out Cartridge_Type) is
      Packed_Voice_Data : Byte_Array (1 .. Packed_Voice_Data_Length);
      Voice_Data : Byte_Array (1 .. Voice_Data_Length);
      Voice_First, Voice_Last : Natural;
   begin
      if Debugging then
         Ada.Text_IO.Put_Line ("Parsing cartridge...");   
         Put_Byte_Array_Information (Data, "Cartridge data");
      end if;

      Voice_First := 1;
      for I in Voice_Index loop
         Voice_Last := Voice_First + Packed_Voice_Data_Length - 1;
         Packed_Voice_Data := Data (Voice_First .. Voice_Last);
         if Debugging then
            Ada.Text_IO.Put_Line ("Parsing voice #" & I'Image);
            Put_Byte_Array_Information (Packed_Voice_Data, "Packed voice data");
         end if;

         Unpack_Voice (Data => Packed_Voice_Data, Result => Voice_Data);
         if Debugging then
            Put_Byte_Array_Information (Voice_Data, "Unpacked voice data");
         end if;
         Parse_Voice (Data => Voice_Data, Result => Result.Voices (I));
         Voice_First := Voice_First + Packed_Voice_Data_Length;
      end loop;
   end Parse;

   -- Gets the cartridge data as bytes for MIDI System Exclusive.
   procedure Emit (Cartridge : in Cartridge_Type; Result : out Cartridge_Data_Type) is
      Data : Cartridge_Data_Type;
      Offset : Natural;
   begin
      Offset := 1;
      for I in Voice_Index loop
         declare
            Original : Voice_Data_Type;
            Packed : Packed_Voice_Data_Type;
            Start_Index : Natural;
            End_Index : Natural;
         begin
            Emit (Cartridge.Voices (I), Original);
            Pack_Voice (Original, Packed);

            Start_Index := Offset;
            End_Index := Offset + Packed_Voice_Data_Length;
            Result (Start_Index .. End_Index) :=
               Packed (1 .. Packed_Voice_Data_Length);
            Offset := Offset + Packed_Voice_Data_Length;
         end;
      end loop;
   end Emit;

   procedure To_XML (Cartridge: in Cartridge_Type; Result : out DX7.XML.Document_Type) is

      function EG_XML (EG : DX7.Envelopes.Envelope_Type) return Unbounded_String is
         Rates_Element, Levels_Element, Result : Unbounded_String;
      begin
         Append (Rates_Element, +"<rates>");
         for Index in DX7.Envelopes.Rate_Index loop
            Append (Rates_Element, DX7.XML.To_String (EG.Rates (Index)));
            if Index /= DX7.Envelopes.Rate_Index'Last then
               Append (Rates_Element, +" ");
            end if;
         end loop;
         Append (Rates_Element, +"</rates>");
         Append (Result, Rates_Element);

         Append (Levels_Element, +"<levels>");
         for Index in DX7.Envelopes.Level_Index loop
            Append (Levels_Element, DX7.XML.To_String (EG.Levels (Index)));
            if Index /= DX7.Envelopes.Level_Index'Last then
               Append (Levels_Element, +" ");
            end if;
         end loop;
         Append (Levels_Element, +"</levels>");
         Append (Result, Levels_Element);
         return Result;
      end EG_XML;

   begin
      Result.Append (+"<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no""?>");
      Result.Append (+"<cartridge>");
      Result.Append (+"<voices>");

      for Voice of Cartridge.Voices loop
         declare
            Attributes : Attributes_Type;
         begin
            Attributes.Include ("algorithm", DX7.XML.To_String (Voice.Algorithm));
            Attributes.Include ("feedback", DX7.XML.To_String (Voice.Feedback));
            Attributes.Include ("name", Voice.Name);
            Attributes.Include ("oscillatorSync", DX7.XML.To_String (Voice.Oscillator_Sync));
            Attributes.Include ("pitchModulationSensitivity", DX7.XML.To_String (Voice.Pitch_Modulation_Sensitivity));
            Attributes.Include ("transpose", DX7.XML.To_String (Voice.Transpose));
            Result.Append (Element ("voice", Attributes));
         end;

         Result.Append (+"<peg>");
         Result.Append (EG_XML (Voice.Pitch_Envelope));
         Result.Append (+"</peg>");

         declare
            Attributes : Attributes_Type;
         begin
            Attributes.Include ("amd", DX7.XML.To_String (Voice.LFO.Amplitude_Modulation_Depth));
            Attributes.Include ("delay", DX7.XML.To_String (Voice.LFO.Delay_Time));
            Attributes.Include ("pmd", DX7.XML.To_String (Voice.LFO.Pitch_Modulation_Depth));
            Attributes.Include ("speed", DX7.XML.To_String (Voice.LFO.Speed));
            Attributes.Include ("sync", DX7.XML.To_String (Voice.LFO.Key_Sync));
            Attributes.Include ("wave", DX7.XML.To_String (Voice.LFO.Waveform));
            Result.Append (Element ("lfo", Attributes, Is_Empty => True));
         end;

         Result.Append (+"<operators>");
         for Op of Voice.Operators loop
            declare
               Attributes : Attributes_Type;
            begin
               Attributes.Include ("amplitudeModulationSensitivity", DX7.XML.To_String (Op.Amplitude_Modulation_Sensitivity));
               Attributes.Include ("coarse", DX7.XML.To_String (Op.Coarse));
               Attributes.Include ("fine", DX7.XML.To_String (Op.Fine));
               Attributes.Include ("detune", DX7.XML.To_String (Op.Detune));
               Attributes.Include ("keyVelocitySensitivity", DX7.XML.To_String (Op.Touch_Sensitivity));
               Attributes.Include ("keyboardRateScaling", DX7.XML.To_String (Op.Keyboard_Rate_Scaling));
               Attributes.Include ("level", DX7.XML.To_String (Op.Output_Level));
               Attributes.Include ("mode", DX7.XML.To_String (Op.Mode));

               Result.Append (Element ("operator", Attributes));

               Result.Append (+"<eg>");
               Result.Append (EG_XML (Op.EG));
               Result.Append (+"</eg>");

               declare
                  Attributes : Attributes_Type;
                  KLS_Content : Unbounded_String;
               begin
                  Attributes.Include ("breakpoint", DX7.XML.To_String (Op.Keyboard_Level_Scaling.Breakpoint));

                  declare
                     Depth_Attributes : Attributes_Type;
                     Curve_Attributes : Attributes_Type;
                  begin
                     Depth_Attributes.Include ("left", DX7.XML.To_String (Op.Keyboard_Level_Scaling.Left.Depth));
                     Depth_Attributes.Include ("right", DX7.XML.To_String (Op.Keyboard_Level_Scaling.Right.Depth));
                     Append (KLS_Content, Element ("depth", Depth_Attributes, Is_Empty => True));

                     Curve_Attributes.Include ("left", DX7.XML.To_String (Op.Keyboard_Level_Scaling.Left.Curve));
                     Curve_Attributes.Include ("right", DX7.XML.To_String (Op.Keyboard_Level_Scaling.Left.Curve));
                     Append (KLS_Content, Element ("curve", Curve_Attributes, Is_Empty => True));
                  end;

                  Result.Append (Element ("keyboardLevelScaling", Attributes, KLS_Content));
               end;
               Result.Append (+"</keyboardLevelScaling>");
            end;
            Result.Append (+"</operator>");
         end loop;
         Result.Append (+"</operators>");
         Result.Append (+"</voice>");
      end loop;

      Result.Append (+"</voices>");
      Result.Append (+"</cartridge>");
   end To_XML;

end DX7.Cartridges;
