--  with Ada.Text_IO;
--  with Layout;
--  with Layout.Search;
--
--  procedure Test_Program is
--     Blocks_Under : Layout.Search.Block_List (5);
--     Turnouts_Under : Layout.Search.Turnout_List (8);
--     Success        : Boolean;
--
--  begin
--     Layout.Search.Blocks_Beneath (Loco     => 11,
--                                   Caboose  => 9,
--                                   Blocks   => Blocks_Under,
--                                   Turnouts => Turnouts_Under,
--                                   Success  => Success);
--
--     for Index in 1 .. Blocks_Under.Size loop
--        Ada.Text_IO.Put_Line ( "Blocks at " & Integer'Image (Index) & " : "
--                              & Layout.Block_ID'Image (Blocks_Under.Items (Index).Block));
--        Ada.Text_IO.Put_Line ( "     Direction : "
--                              & Layout.Block_Polarity'Image (Blocks_Under.Items (Index).Direction));
--     end loop;
--  end Test_Program;


--with MaRTE_OS;
--pragma Warnings(Off, MaRTE_OS);

with Layout.Search;
with Layout;
with Trains;
with Engineers;
with Trains.Operations;
with Engineers.Operations;
with Hand_Controller;
with Locomotives;
with Cabs;
with DAC;
with Shared_Types;
with Blocks;
with Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;
with DoubleTalk;
with Ada.Strings.Bounded;
with Hand_Controller; use Hand_Controller;
with Halls;
with Turnouts;

procedure test_program is
   Blocks_Under : Layout.Search.Block_List(5);
   Turns_Under  : Layout.Search.Turnout_List(8);
   Success      : Boolean;
   Phrase_One : DoubleTalk.Phrase_Strings.Bounded_String :=
     DoubleTalk.Phrase_Strings.To_Bounded_String(",,Error,,,,,,4,O,4,,,,,,Train,,,,,,,Ready");
   Phrase_Two : DoubleTalk.Phrase_Strings.Bounded_String :=
     DoubleTalk.Phrase_Strings.To_Bounded_String(",,Turnouts,,,,,,Ready");


   --Settings : Hand_Controller.Controller_Settings;

begin
   doubletalk.Speak (Phrase_One, DoubleTalk.Paul);
   Ada.Text_IO.Put_Line ("Layout Search");
   Layout.Search.Blocks_Beneath (Loco     => 11,
                                 Caboose  => 9,
                                 Blocks   => Blocks_Under,
                                 Turnouts => Turns_Under,
                                 Success  => Success);

   Ada.Text_IO.Put_Line ("Trains.Operations" & Boolean'Image (Success));
   Trains.Operations.Initialize_Train (1, Locomotives.Available_Locos (1), Blocks_Under, Turns_Under, 1);


   Ada.Text_IO.Put_Line ("Cabs.Set_Limit");
   Cabs.Set_Limit (Cab   => 1,
                   Value => 100);

   Engineers.Operations.Enable (1, 1, Hand_Controller.A);

   Halls.Initialize;
   Halls.Enable (Callback => Trains.Operations.Update_Location'Access);

   Ada.Text_IO.Put_Line ("Press Enter to begin setting turnouts");
   Ada.Text_IO.Skip_Line;

   Turnouts.Set (Requestor => 0,
                 Turnout   => 10,
                 Direction => Layout.Right);
   Turnouts.Set (Requestor => 0,
                 Turnout   => 22,
                 Direction => Layout.Right);
   Turnouts.Set (Requestor => 0,
                 Turnout   => 21,
                   Direction => Layout.Right);
   Turnouts.Set (Requestor => 0,
                 Turnout   => 11,
                   Direction => Layout.Right);
   Turnouts.Set (Requestor => 0,
                 Turnout   => 5,
                   Direction => Layout.Right);
   Turnouts.Set (Requestor => 0,
                 Turnout   => 18,
                   Direction => Layout.Right);
   Turnouts.Set (Requestor => 0,
                 Turnout   => 3,
                 Direction => Layout.Right);



   doubletalk.Speak (Phrase_Two, DoubleTalk.Vader);
   Ada.Text_IO.Put_Line ("Press Enter to exit");
   Ada.Text_IO.Skip_Line;

--     DAC.Set_Voltage (Channel => 1,
--                      Voltage => 5.0);
--     Ada.Text_IO.Put_Line ("Chabnnel 1: Press Enter to continue");
--     Ada.Text_IO.Skip_Line;
--     DAC.Set_Voltage (Channel => 2,
--                      Voltage => 5.0);
--     Ada.Text_IO.Put_Line ("Channel 2: Press Enter to continue");
--     Ada.Text_IO.Skip_Line;
--     DAC.Set_Voltage (Channel => 3,
--                      Voltage => 5.0);
--     Ada.Text_IO.Put_Line ("Channel 3: Press Enter to continue");
--     Ada.Text_IO.Skip_Line;
--     DAC.Set_Voltage (Channel => 4,
--                      Voltage => 5.0);
--     Ada.Text_IO.Put_Line ("Channel 4: Press Enter to continue");
--     Ada.Text_IO.Skip_Line;
--     DAC.Set_Voltage (Channel => 5,
--                      Voltage => 5.0);
--     Ada.Text_IO.Put_Line ("Channel 5: Press Enter to continue");
--     Ada.Text_IO.Skip_Line;



--     Blocks.Assign_Cab (Block    => Layout.Block_ID(9),
--                        Cab      => Cabs.Cab_ID(1),
--                        Polarity => Layout.Normal);
--     Blocks.Assign_Cab (Block    => Layout.Block_ID(10),
--                        Cab      => Cabs.Cab_ID(1),
--                        Polarity => Layout.Normal);
   --Ada.Text_IO.Put_Line ("Enabling Engineer");
   --Ada.Text_IO.Put_Line ("Done Enabling");

--     Ada.Text_IO.Put_Line ("Speaking Phrase One");
--     doubletalk.Speak (Phrase_One, DoubleTalk.Paul);
--     Ada.Text_IO.Put_Line ("Speaking Phrase Two");
--     doubletalk.Speak (Phrase_Two, DoubleTalk.Vader);
--     Ada.Text_IO.Put_Line ("Speaking Phrase Three");
--     doubletalk.Speak (Phrase_Three, DoubleTalk.Robo);
--     Ada.Text_IO.Put_Line ("Speaking Phrase Four");
--     doubletalk.Speak (Phrase_Four, DoubleTalk.Randy);
--
--
--     --***Testing Hand_Controller Settings***
--     Ada.Text_IO.Put_Line ("Entering Loop");
--     loop
--        Settings :=
--          Hand_Controller.Get_Controller_State
--            (Controller_Id => Hand_Controller.Controller_Id_Type (A));
--
--        Ada.Text_IO.Put_Line ("*****************************************************");
--        Ada.Text_IO.Put_Line ("Horn State: ");
--        Ada.Text_IO.Put_Line(Hand_Controller.Button_Type'Image(Settings.Horn_State));
--        Ada.Text_IO.Put_Line ("EM State: ");
--        Ada.Text_IO.Put_Line(Hand_Controller.Button_Type'Image
--                                                   (Settings.Emergency_State));
--        Ada.Text_IO.Put_Line ("Throttle State: ");
--        Ada.Text_IO.Put_Line(Hand_Controller.Percent_Type'Image
--                             (Settings.Throttle_State));
--        Ada.Text_IO.New_Line (1);
--        Ada.Text_IO.Put_Line ("Turnout State: " & Layout.Turnout_Limb'Image
--                                                   (Settings.Left_Right));
--        Ada.Text_IO.Put_Line ("Direction State: " & Trains.Direction_Type'Image
--                              (Settings.Direction));
--
--        delay 3.0;
--
--        exit when (Settings.Horn_State = Hand_Controller.Down) and
--                  (Settings.Emergency_State = Hand_Controller.Down);
--     end loop;
--     Ada.Text_IO.Put_Line ("Loop Ended");

   --DAC.Set_Voltage (Channel => 0,
   --Voltage => 5.0);

   --***Turns on all the Block at once except for 40 (something gets angry)***
   --for x in 1 .. 39 loop
   	--Blocks.Assign_Cab (Block    => Layout.Block_ID(x),
                           --Cab      => Cabs.Cab_ID(1),
                            --Polarity => Layout.Normal);
   --end loop;


end test_program;
