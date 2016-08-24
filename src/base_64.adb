Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Unchecked_Conversion;

Package Body Base_64 is

    Package Body Internals is

	Function Internal_Convert is new Unchecked_Conversion(
	    Source => Tripple,
	    Target => Quad
	   );

	Function Internal_Convert is new Unchecked_Conversion(
	    Source => Quad,
 	    Target => Tripple
	   );

	Function Convert( Source : Quad ) return Tripple is
	  ( Internal_Convert(Source) );

	Function Convert( Source : Tripple ) return Quad is
	  ( Internal_Convert(Source) );

	Function Convert (Source : Base_64_Chunk) return Quad is
	(if (for all C of Source => C in Nonpad_Character) then
	  (  Internal_Character( Source(1) ),
	     Internal_Character( Source(2) ),
	     Internal_Character( Source(3) ),
	     Internal_Character( Source(4) )
	  ) else raise Program_Error with "Cannot convert a chunk with padding."
	);


	Function Convert (Source : Quad) return Base_64_Chunk is
	  (Base_64_Character( Source.A ),
	   Base_64_Character( Source.B ),
	   Base_64_Character( Source.C ),
	   Base_64_Character( Source.D )
	  );

    end Internals;


    Function Convert( Ch : Character ) return Base_64_Character is
      (  Base_64_Character'Value((''', Ch,'''))  );

    Function Character_Check( Ch : Character ) return Boolean is
    begin
	declare
	    -- Constraint_Error is raised if 'Value cannot return a valid result.
	    C : Base_64_Character renames Base_64_Character'Value((''', Ch,'''));
	begin
	    Return True; -- If we get here, we could convert it.
	end;
    exception
	when Constraint_Error => Return False; -- If we get her, we couldn't.
    End Character_Check;


    Function Encode( Data : String ) return Base_64_String is
	State : Encoder;
	Function Convert is new Unchecked_Conversion(
	    Source => Character,
	    Target => Interfaces.Integer_8
	   );
    begin
	-- We feed the characters of the string (converted to bytes) to the encoder.
	for Item of Data loop
	    Feed( State, Convert(Item) );
	end loop;

	-- And return the result.
	Return Get_Data( State );
    end Encode;

    Function Decode( Data : Base_64_String ) return String is
	State : Encoder;
	Function Convert is new Unchecked_Conversion(
	    Source => Interfaces.Integer_8,
	    Target => Character
	   );
    begin
	-- If it's an empty string we can return an empty string.
	if Data'Length not in Positive then
	    Return "";
	end if;

	-- Otherwise we have to set the internal state of our encoder...
	Reset( State, Data );

	declare
	    -- get the bytes that are represented by the Base64 string...
	    Data : Bytes renames Get_Data( State );
	begin
	    -- and convert them to characters.
	    Return Result : String(Data'Range) do
		For Index in Data'Range loop
		    Result(Index):= Convert( Data(Index) );
		end loop;
	    end return;
	end;
    End Decode;


    Function To_String( Data : Base_64_String ) return String is
    Begin
	-- We take the image of every character of the Base64 string as the
	-- character of our return-string; this results in a normal string
	-- containing the 'image' of the given Base64 string.
	Return Result : String( Data'Range ) do
	    for Index in Data'Range loop
		declare
		    B64C  : Base_64_Character renames Data(Index);
		    Image : String            renames Base_64_Character'Image(B64C);
		    CI    : Character         renames Image(2); -- Remove single-quotes.
		begin
		    Result(Index):= CI;
		end;
	    end loop;
	End return;
    End To_String;


    Function As_String( Data : String ) return Base_64_String is
    begin
	-- Essentially the reverse of To_String, we convert all the characters
	-- into the Base_64_Character character-set.
	Return Result : Base_64_String(Data'Range) do
	    For Index in Result'range loop
		Result(Index) := Convert( Data(Index) );
	    end loop;
	end return;
    end As_String;

    -------


    Procedure Feed( State : in out Encoder; Data : Bytes ) is
    begin
	-- Just give the bytes in order.
	For Item of Data loop
	    Feed( State, Item );
	end loop;
    end Feed;

    Procedure Feed( State : in out Encoder; Data : Interfaces.Integer_8 ) is
	Use Internals;
    begin
	-- Every three bytes (24-bits) gives us another quad. This manages
	-- the internal state and registers, adding the quad when it is
 	-- fully formed.
	case State.Current is
	when 1 =>
	    State.B1:= Data;
	    State.Current:= Internal_Count'Succ( State.Current );
	when 2 =>
	    State.B2:= Data;
	    State.Current:= Internal_Count'Succ( State.Current );
	when 3 =>
	    Process:
	    Declare
		Three_Bytes : Constant Tripple := (State.B1, State.B2, Data);
		Four_Chars  : Quad renames Convert(Three_Bytes);
	    Begin
		State.Result.Append( Four_Chars );
		State.Current:= Internal_Count'First;
	    End Process;
	end case;
    End Feed;

    Function  Get_Data( State : in Encoder ) return Bytes is
	Use Ada.Containers, Internals.Internal_Data_Vector, Internals;
	Chunk_Size : Constant := 3;
	Padding : Constant Natural := (case State.Current is
				    when 1 => 0,
				    when 2 => 1,
				    when 3 => 2
			       );
	Subtype Result_Range is Positive range 1..Natural(State.Result.Length);
	Subtype Padded_Range is Positive range 1..Result_Range'last*Chunk_Size+Padding;
    begin
	Return Result : Bytes(Padded_Range) do
	    Transpose_Result:
	    For Index in Result_Range loop
		declare
		    Element : Quad renames State.Result(Index);
		    Three   : Tripple renames Convert(Element);
		    Offset  : Constant Positive :=
		      Positive'Succ(Chunk_Size*Natural'Pred(Index));

		    Subtype Chunk is Positive range Offset..Positive'Pred(Offset+Chunk_Size);
		begin
		    Result(Chunk):= (Three.A, Three.B, Three.C);
		end;
	    End Loop Transpose_Result;

	    Handle_Tail:
	    declare
	    begin
		case State.Current is
		when 1 => null;
		when 2 => Result(Result'Last)   := State.B1;
		when 3 => Result(Result'Last-1) := State.B1;
			  Result(Result'Last)   := State.B2;
		end case;
	    End Handle_Tail;
	end return;

    end Get_Data;

    Function  Get_Data( State : in Encoder ) return Base_64_String is
	Use Ada.Containers, Internals.Internal_Data_Vector, Internals;
	Chunk_Size : Constant := Base_64_Chunk'Length;
	Padding : Constant Natural := (if State.Current = 1 then 0 else 1);
	Subtype Result_Range is Positive range 1..Natural(State.Result.Length);
	Subtype Padded_Range is Positive range 1..(Result_Range'last+Padding)*Chunk_Size;
    begin
	Return Result : Base_64_String(Padded_Range) := (others => '=') do
	    Transpose_Result:
	    For Index in Result_Range loop
		declare
		    Offset : Constant Natural := Chunk_Size*Natural'Pred(Index);
		    Start  : Constant Positive:= Offset+1;
		    Stop   : Constant Positive:= Positive'Pred(Start+Chunk_Size);
		    Temp   : Base_64_Chunk renames Convert(State.Result(Index));
		begin
		    Result(Start..Stop):= Base_64_String( Temp );
		end;
	    End Loop Transpose_Result;

	    Handle_Tail:
	    declare
	    begin
		case State.Current is
		when 1 => null;
		when others =>
		    declare
			Three : Constant Tripple := (State.B1, State.B2, 0);
			Four  : Base_64_Chunk renames Convert(Convert(Three));
		    begin
			Result(Padded_Range'Last-3):= Four(1);
			Result(Padded_Range'Last-2):= Four(2);
			if State.Current = 3 then
			    Result(Padded_Range'Last-1):= Four(3);
			end if;
		    end;
		end case;
	    End Handle_Tail;
	end return;
    end Get_Data;

    Function  Reset( State : in out Encoder) return Base_64_String is
    Begin
	Return Result : constant Base_64_String := Get_Data(State) do
	    Reset(State);
	End return;
    End Reset;

    Procedure Reset( State : in out Encoder; Data : Base_64_String ) is
	Subtype Index_Range is Positive range 1..Data'Length/4;
    begin
	Reset(State);

	if Data'Length not in Positive then
	    Return;
	else
	    Decode:
	    declare
		Use Internals;
		Has_Pad     : Constant Boolean := Data(Data'Last) not in Nonpad_Character;
		Two_Pad     : Constant Boolean := Data(Data'Last-1) not in Nonpad_Character;
		Data_Length : Constant Integer := Data'Length / 4 - (if Has_Pad then 1 else 0);
		Subtype Chunk_Index is Positive range Data'First..Data_Length;
	    begin
		Common_Portion:
		For Index in Chunk_Index loop
		    declare
			Offset : Constant Positive := 1+(Index-1)*4;
			Chunk  : Constant Base_64_Chunk := Base_64_Chunk(Data(Offset..Offset+3));
			Item   : Quad renames Convert( Chunk );
		    begin
			State.Result.Append( Item );
		    end;
		End Loop Common_Portion;

		if Has_Pad then
		    Handle_Padding:
		    Declare
			Tail : Base_64_String renames Data(Data'Last-3..Data'Last);
			B64C : Constant Base_64_Chunk := Base_64_Chunk(Tail);
			Last : Constant Quad :=
			  ( Internal_Character( B64C(1) ),
			    Internal_Character( B64C(2) ),
			    (if Two_Pad then Internal_Character'First
			     else Internal_Character( B64C(3) )),
			    Internal_Character'First
			  );
			Item : Tripple renames Convert( Last );
		    Begin
			Feed( State, Item.A );
			if not Two_Pad then
			    Feed( State, Item.B );
			end if;
		    End Handle_Padding;
		end if;
	    End Decode;
	end if;
    end Reset;

    Procedure Reset( State : in out Encoder ) is
    Begin
	State.Result.Clear;
	State.Current:= 1;
	State.B1 := Interfaces.Integer_8'First;
	State.B2 := Interfaces.Integer_8'First;
    End Reset;

End Base_64;
