---------------------------------------------------------------------------------
-- Dalton Lee
--
-- Sets up the trains that the user wants to run.
--
-- **Should also display all the info for all the trains but it is currently
-- lacking that**
---------------------------------------------------------------------------------


--with MaRTE_OS;
--pragma Warnings (Off, MaRTE_OS);

with Layout.Search;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Locomotives;
with Ada.IO_Exceptions;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Text_IO.Unbounded_IO; use Ada.Strings.Unbounded;

procedure Dispatcher is

   type Direction_Type is (Normal, Backward);
   subtype Block_Type is Integer range 1 .. 40;

   type Train_Info_Type is
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String;
         Throttle      : Integer := -1;
         Engine_Block  : Block_Type;
         Caboose_Block : Block_Type;
         Direction     : Direction_Type;
         Blocks_Occ    : Layout.Search.Block_List (5);
      end record;

   -----------------------------------------------------------------------------
   function New_Train return Train_Info_Type is

      train_info : Train_Info_Type;

      Name   : Ada.Strings.Unbounded.Unbounded_String;
      Model  : Ada.Strings.Unbounded.Unbounded_String;
      Number : Ada.Strings.Unbounded.Unbounded_String;

      -------------------------------------------------------------
      procedure get_name is
      begin
         loop
            Ada.Text_IO.Put ("What is the train's name? ");
            Name := Ada.Text_IO.Unbounded_IO.Get_Line;

         exit when Length (Name) > 1 and Length (Name) <= 19;
            Ada.Text_IO.Put_Line ("THAT NAME IS TO LONG. MUST " &
                                  "BE LESS THAN 19 CHARACTERS.");
            Ada.Text_IO.New_Line (1);
         end loop;
      end get_name;

      -------------------------------------------------------------
      --Gets the model number from the user
      procedure get_model is
      begin
         loop
            Ada.Text_IO.Put ("What is the trian's model? ");
            Ada.Text_IO.Unbounded_IO.Get_Line (Model);

         exit when Length (Model) > 1 and Length (Model) <= 12;
            Ada.Text_IO.Put_Line ("THAT MODEL NAME IS TO LONG. MUST " &
                                  "BE LESS THAN 19 CHARACTERS.");
            Ada.Text_IO.New_Line (1);
         end loop;
      end get_model;

      -------------------------------------------------------------
      --Gets the train number from the user
      procedure get_number is
      begin
         loop
            Ada.Text_IO.Put ("What is the train's number? ");
            Ada.Text_IO.Unbounded_IO.Get_Line (Number);

         exit when Length (Number) > 1 or Length (Number) <= 4;
            Ada.Text_IO.Put_Line ("THAT NUMBER IS INCORRECT. MUST " &
                                  "BE BETWEEN 0 AND 9999.");
            Ada.Text_IO.New_Line (1);
         end loop;


      end get_number;

      -------------------------------------------------------------
      --Gets the minimum throttle for the train to run
      procedure get_throttle is
      begin
         loop
            Block :
               begin
                  Ada.Text_IO.Put_Line ("What is the minimum percent throttle setting required for this " &
                                        "locomotive to move? (0 .. 100)");
                  Ada.Text_IO.Put ("If you have not yet calibrated this locomotive, enter 10. ");
         Ada.Integer_Text_IO.Get (train_info.Throttle);


         exit when train_info.Throttle > 0 and train_info.Throttle <= 100;
                     Ada.Text_IO.Put_Line ("THAT NUMBER IS TO BIG. MUST " &
                                           "BE BETWEEN 0 AND 100");

               exception --If they try and enter anything not an integer
                  when Ada.IO_Exceptions.Data_Error =>
                     Ada.Text_IO.Put_Line ("INCORRECT NUMBER. MUST BE BETWEEN 0 AND 100 (new_train 1)");
                     Ada.Text_IO.New_Line (1);
            end Block;
         end loop;
      end get_throttle;

   ----------------------------------------------------------------
   begin

      get_name;
      get_model;
      get_number;
      get_throttle;


      -- Putting it all into train_info.Name
      for x in 1 .. 21 - Length (Name) loop
         Append (Name, " ");
      end loop;

      Append (Name, Model);

      for x in 1 .. 14 - Length (Model) loop
         Append (Name, " ");
      end loop;

      Append (Name, "#");
      Append (Name, Number);

      train_info.Name := Name;

      return train_info;

   end New_Train;

   -----------------------------------------------------------------------------
   procedure train_location (train_info : in out Train_Info_Type) is

      char : Character;

   begin
      loop
         Block :
            begin
               Ada.Text_IO.Put ("Which block is the locomotive on? ");
               Ada.Integer_Text_IO.Get (train_info.Engine_Block);
               Ada.Text_IO.New_Line (1);

               Ada.Text_IO.Put ("whick block is the caboose (or last car) on? ");
               Ada.Integer_Text_IO.Get (train_info.Caboose_Block);
               Ada.Text_IO.New_Line (1);
               exit;
         exception
            when CONSTRAINT_ERROR =>
               Ada.Text_IO.Put_Line ("INCORRECT NUMBER. MUST BE BETWEEN 1 AND 40");
               Ada.Text_IO.New_Line (1);
         end Block;
      end loop;


      loop
      exit when train_info.Engine_Block /= train_info.Caboose_Block; -- We only have the train engine

         Ada.Text_IO.Put_Line ("What direction is the train going?");
         Ada.Text_IO.Put_Line ("   N = Normal");
         Ada.Text_IO.Put_Line ("   R = Reverse");

         Ada.Text_IO.Get (char);
         char := Ada.Characters.Handling.To_Lower (char); --Converts the character to uppercase

         case char is
            when 'N' =>
               train_info.Direction := Normal;
               exit;

            when 'R' =>
               train_info.Direction := Backward;
               exit;

            when others =>
               Ada.Text_IO.Put_Line ("************ Char is: " & char & "********************");
               Ada.Text_IO.Put_Line ("INCORRECT DIRECTION CHOICE (train_location 2)");
               Ada.Text_IO.New_Line (1);
         end case;
      end loop;

   end train_location;

   -----------------------------------------------------------------------------
   function trains_available (train_num : Positive) return Train_Info_Type is

      chosen : Integer;
      train_info : Train_Info_Type;

   begin

      loop
         Ada.Text_IO.Put_Line ("The following locomotives are available");
         Ada.Text_IO.New_Line (2);

         -- Prints out all available Locomotives for the user to pick from
         for x in Locomotives.Available_Locos'First .. Locomotives.Available_Locos'Last loop
            Ada.Integer_Text_IO.Put (x, 1);

            if x = 10 then
               Ada.Text_IO.Put_Line ("    " & Locomotives.Available_Locos (x).Name);
            else
               Ada.Text_IO.Put_Line ("     " & Locomotives.Available_Locos (x).Name);
            end if;
         end loop;
         Ada.Text_IO.Put_Line ("11    Other");
         Ada.Text_IO.New_Line (1);


         Ada.Text_IO.Put_Line ("Enter the number from the table above pulling Train "
                               & Integer'Image (train_num));
         Ada.Integer_Text_IO.Get (chosen);

         if chosen >= 1 and chosen <= 10 then
            train_info.Name := To_Unbounded_String (Locomotives.Available_Locos (chosen).Name);
            train_info.Throttle := Locomotives.Available_Locos (chosen).Min_Throttle;

         elsif chosen = 11 then
            train_info := New_Train;
         end if;

      exit when chosen >= 1 or chosen <= 11;
         Ada.Text_IO.Put_Line ("INCORRECT TRAIN CHOICE (trains_available)");
         Ada.Text_IO.New_Line (2);

      end loop;


      return train_info;

   end trains_available;
   -----------------------------------------------------------------------------
   function welcome_screen return natural is

      trains : Natural;

   begin

      loop
         Ada.Text_IO.New_Line (8);
         Ada.Text_IO.Put_Line ("                                         Error 404 Train Lab"); --Better way?
         Ada.Text_IO.New_Line (2);

         Ada.Text_IO.Put_Line (" How many trains do you wish to run? (1, 2, 3)");
         Ada.Text_IO.New_Line (1);

         Ada.Text_IO.Put_Line (" There must be at least one unoccupied block bewteen each pair of trains");

         Ada.Integer_Text_IO.Get (trains);

      exit when trains > 0 and trains <= 3; --*******A train doesn't have a block between it and another*****

         Ada.Text_IO.New_Line (1);
         Ada.Text_IO.Put_Line ("INVALID INPUT. The number of trains must be 1, 2, or 3");
         Ada.Text_IO.New_Line (2);

      end loop;

      return trains;


   end welcome_screen;

   -----------------------------------------------------------------------------
   procedure num_blocks_covered (train : in out Train_Info_Type;
                                 Num_Blocks : out Natural) is
      Block_List : Layout.Search.Block_List (5);
      Turnouts : Layout.Search.Turnout_List (10);
      Trash : Boolean;

   begin

      Ada.Text_IO.Put_Line ("Engine_Block: " & Integer'Image (train.Engine_Block)); --*********************
      Ada.Text_IO.Put_Line ("Engine_Block: " & Integer'Image (train.Caboose_Block)); --***************


      Layout.Search.Blocks_Beneath (Loco     => Layout.Block_ID (train.Engine_Block),
                                    Caboose  => Layout.Block_ID (train.Caboose_Block),
                                    Blocks   => Block_List, --train.Blocks_Occ,
                                    Turnouts => Turnouts,
                                    Success  => Trash);

      Ada.Text_IO.Put_Line ("Success is: " & Boolean'Image (Trash)); --***************************
      Ada.Text_IO.Put ("Block_List.Size: ");
      Ada.Integer_Text_IO.Put (Block_List.Size);
      Ada.Text_IO.New_Line (1);
      Ada.Text_IO.Put_Line ("Value 1 is: " & Layout.Block_ID'Image (Block_List.Items (1).Block));
      Ada.Text_IO.Put_Line ("Value 2 is: " & Layout.Block_ID'Image (Block_List.Items (2).Block));
      Ada.Text_IO.Put_Line ("Value 3 is: " & Layout.Block_ID'Image (Block_List.Items (3).Block));
      Ada.Text_IO.Put_Line ("Value 4 is: " & Layout.Block_ID'Image (Block_List.Items (4).Block));
      Ada.Text_IO.New_Line (1); --***************************************

      train.Blocks_Occ := Block_List;
      Num_Blocks := Block_List.Size;

   end num_blocks_covered;

   -----------------------------------------------------------------------------
   function summarize (train     : Train_Info_Type;
                       train_num : Positive) return Character is

      char : Character;

   begin

      loop
         -- ***************** Revisit these two lines ****************************
         Ada.Text_IO.Put_Line ("                                        Train " & Integer'Image (train_num));
         Ada.Text_IO.Put_Line ("                              Confirmation of Information");
         Ada.Text_IO.New_Line (1);

         Ada.Text_IO.Unbounded_IO.Put (train.Name & "  ");
         Ada.Text_IO.New_Line (1);

         Ada.Text_IO.Put_Line ("Locomotive on block   " &
                               Integer'Image (train.Engine_Block));
         Ada.Text_IO.Put_Line ("Caboose    on block   " &
                               Integer'Image (train.Caboose_Block));
         Ada.Text_IO.Put_Line ("Train occupies blocks ");

         Ada.Text_IO.Put ("BLocks size is: "); -----*********************************************
         Ada.Integer_Text_IO.Put (Integer (train.Blocks_Occ.Size), 1);
         Ada.Text_IO.New_Line (1);


         for x in 1 .. train.Blocks_Occ.Size loop
            Ada.Text_IO.Put ("x is: "); --*********************************
            Ada.Integer_Text_IO.Put (x, 1);
            Ada.Text_IO.New_Line (1);
            Ada.Integer_Text_IO.Put (Integer (train.Blocks_Occ.Items (x).Block), 1);
            if x /= train.Blocks_Occ.Size then
               Ada.Text_IO.Put (", ");
            end if;
         end loop;

         Ada.Text_IO.New_Line (2);

         Ada.Text_IO.Put_Line ("Is this information correct? Enter on of the following: ");
         Ada.Text_IO.Put_Line ("   Y = Yes, the information for this train is correct.");
         Ada.Text_IO.Put_Line ("   N = No,  I wish to enter different information for this train.");
         Ada.Text_IO.Put_Line ("   R = No,  I wish to restart setting up from the beginning.");
         Ada.Text_IO.Put_Line ("   Q = NO,  I wish to terminate this operating session");

         Ada.Text_IO.Get (char);
         --Converts the character to uppercase
         char := Ada.Characters.Handling.To_Upper (char);

      exit when (char = 'Y') or (char = 'N') or (char = 'R') or (char = 'Q');
         Ada.Text_IO.Put_Line ("************ Char is: " & char & "********************");
         Ada.Text_IO.Put_Line ("THAT IS AN INVALID CHOICE (summarize)");
         Ada.Text_IO.New_Line (1);
      end loop;

      return char;


   end summarize;

--------------------------------------------------------------------------------



   train_array : array (1 .. 3) of Train_Info_Type;


   char : Character;
   trains : Natural;
   num_Blocks : Natural;


begin

   trains := welcome_screen;


   for x in 1 .. trains loop

      <<RESTART>> -- We are able to jump to the top of the loop with out hitting the end loop
                  -- I fixed the others but these ones are staying
      train_array (x) := trains_available (x);

      <<REENTER>>
      train_location (train_array (x));


      -- **************** If the train is > 1 need to figure out direction ******************************
      num_blocks_covered (train      => train_array (x),
                          Num_Blocks => num_Blocks);

      if num_Blocks > 3 then
         Ada.Text_IO.Put_Line ("The maximum number of blocks beneath a train is 3.");
         Ada.Text_IO.Put_Line ("There are more than 3 blocks beneath your train.");
         Ada.Text_IO.New_Line (2);

         Ada.Text_IO.Put_Line ("Would you like to reenter the information?");
         Ada.Text_IO.Put_Line ("   Y = Yes, I wish to reenter this train's location");
         Ada.Text_IO.Put_Line ("   N = No,  I wish to restart set-up from the beginning");
         Ada.Text_IO.Put_Line ("   Q = No, I wish to terminate this operating session");

         Ada.Text_IO.Get (char);
         char := Ada.Characters.Handling.To_Upper (char); --Converts the character to uppercase

         <<INCORRECT>>
         case char is
            when 'Y' =>
               goto REENTER;

            when 'N' =>
               goto RESTART;

            when 'Q' =>
               Ada.Text_IO.Put_Line ("Goodbye");
               GNAT.OS_Lib.OS_Exit (0); --Ends the program artificially

            when others =>
               Ada.Text_IO.Put_Line ("************ Char is: " & char & "********************");
               Ada.Text_IO.Put_Line ("THAT IS AN INVALID CHOICE (main)");
               goto INCORRECT;

         end case;
      end if;


      char := summarize (train_array (x), x);

      case char is
            when 'Y' =>
               null;

            when 'N' =>
               goto REENTER;

            when 'R' =>
               goto RESTART;

            when 'Q' =>
               Ada.Text_IO.Put_Line ("Goodbye");
               GNAT.OS_Lib.OS_Exit (0); --Ends the program artificially

            when others => -- we can never get here
               Ada.Text_IO.Put_Line ("SOMETHING WENT REALLY WRONG");

         end case;
   end loop;
end Dispatcher;



