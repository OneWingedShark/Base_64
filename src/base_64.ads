Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Ada.Containers.Indefinite_Vectors,
Interfaces;

Package Base_64 is

    -- Enumeration of the 65 characters that can appear in a Base64 string.
    Type Base_64_Character is private;


    -- Returns true if the given character is in the Base_64_Character type.
    Function Character_Check( Ch : Character ) return Boolean;

    -- Either returns true or raises an exception.
    Function Validate_String( S : String ) return Boolean;

    -- Converts the given character to a Base64 character.
    Function Convert( Ch : Character ) return Base_64_Character
      with Pre => Character_Check( Ch ) or else
                  raise Constraint_Error with "Invalid Base64 character.";

    -- This type represents a valid Base-64 string; its characters are that of
    -- the above character-set and therefore has no relation to the internal
    -- string-type.
    --
    -- NOTE:	The only reason these predicates are not function-calls to
    --		functions hidden in the PRIVATE section is the presence of
    --		a compiler-bug which incorrectly prohibits usage of aspects
    --		in a forward-referential manner.
    type Base_64_String is Array(Positive range <>) of Base_64_Character
      with Dynamic_Predicate =>
	-- Base64 Strings always have a length that is a multiple of 4.
       (Base_64_String'Length mod 4 = 0
	or else raise Constraint_Error with "Illegal Base64 length:"
	              & Natural'Image(Base_64_String'Length))
      and then
	-- Only the last two characters can be padding.
      ((for all Item of Base_64_String(Base_64_String'First..Positive'Pred(Positive'Pred( Base_64_String'Last ))) =>
	  Not_Padding(Item)) or else raise Constraint_Error with "Malformed Base64 string.")
      and then
        -- Pad characters must come at the end, if present.
      (if Base_64_String'Length > 1 and then Base_64_String(Base_64_String'Last-1) = '=' then
	 (Base_64_String(Base_64_String'Last) = '=' or else
	  raise Constraint_Error with "Nonterminal pad character detected.")
      );


    -- Encode to and decode from strings.
    Function Encode( Data : String ) return Base_64_String;
    Function Decode( Data : Base_64_String ) return String;

    -- An array of bytes, used for feedign data to the encoder.
    Type Bytes is Array(Positive range <>) of Interfaces.Integer_8;

    -- Converts a Base_64_String to a String.
    -- NOTE: This does NOT entail decoding.
    Function To_String( Data : Base_64_String ) return String;

    -- Converts a string to a base-64 string, if able.
    Function As_String( Data : String ) return Base_64_String
      with Pre => Validate_String( String'(Data) );

    -- This generator holds the internal state of a Base64 string, essentially
    -- acting as a buffer until cleared.
    Type Encoder is private;

    -- Feeds a byte to the encoder.
    Procedure Feed( State : in out Encoder; Data : Interfaces.Integer_8 );

    -- Feeds the giver series of bytes to the encoder.
    Procedure Feed( State : in out Encoder; Data : Bytes );

    -- Retrieves the Base64 string represented by the encoder's state.
    Function  Get_Data( State : in Encoder ) return Base_64_String;

    -- Retrives the byte-sequence that was fed into the encoder.
    Function  Get_Data( State : in Encoder ) return Bytes;

    -- Gets the Base64 string represented by the encoder; resets the state to default.
    Function  Reset( State : in out Encoder) return Base_64_String;

    --Sets the state of the encoder to the given Base64 string.
    Procedure Reset( State : in out Encoder; Data : Base_64_String );

    -- Resets the state to default.
    Procedure Reset( State : in out Encoder );

Private

    Type Base_64_Character is
      (	'A',	'B',	'C',	'D',	'E',
       	'F',	'G',	'H',	'I',	'J',
	'K',	'L',	'M',	'N',	'O',
	'P',	'Q',	'R',	'S',	'T',
	'U',	'V',	'W',	'X',	'Y',
	'Z',	'a',	'b',	'c',	'd',
	'e',	'f',	'g',	'h',	'i',
	'j',	'k',	'l',	'm',	'n',
	'o',	'p',	'q',	'r',	's',
	't',	'u',	'v',	'w',	'x',
	'y',	'z',	'0',	'1',	'2',
	'3',	'4',	'5',	'6',	'7',
	'8',	'9',	'+',	'/',	'='
      ) with Size => 8, Object_Size => 8;


    -- A subtype excluding the pad-character; exactly 64 symbols.
    Subtype Nonpad_Character is Base_64_Character range
      Base_64_Character'First..Base_64_Character'Pred(Base_64_Character'Last);

    Package Internals is

	----------------------
	--  INTERNAL TYPES  --
        ----------------------

	Type Base_64_Chunk  is array(Positive range 1..4) of Base_64_Character;

	-- This is to keep track of how many bytes have been fed into the encoder.
	Type Internal_Count is range 1..3;

	-- When we exclude the padding character we get 64 symbols, this is
	-- exactly representable in 6 bits.
	Type Internal_Character is new Nonpad_Character
	  with Size => 6;

	-- A set of four internal characters, takes up 24 bits.
	Type Quad    is record
	    A, B, C, D : Internal_Character;
	end record
	  with Size => 24, Object_Size => 24, Pack;

	For Quad use record at mod 8;
	    D at 0 range 0..5;
	    C at 0 range 6..11;
	    B at 0 range 12..17;
	    A at 0 range 18..23;
	end record;

	-- A set of three bytes; takes up 24 bits.
	Type Tripple is record
	    A, B, C : Interfaces.Integer_8;
	end record
	  with Size => 24, Object_Size => 24, Pack;

	For Tripple use record at mod 8;
	    A at 2 range 0..7;
	    B at 1 range 0..7;
	    C at 0 range 0..7;
	end record;

	-------------------------
	--  INTERNAL PACKAGES  --
        -------------------------

	-- This defines a vector to keep fully-constructed Quads in.
	Package Internal_Data_Vector is new Ada.Containers.Indefinite_Vectors(
	    Index_Type   => Positive,
 	    Element_Type => Quad,
	    "="          => "="
	);

	--------------------------
	--  INTERNAL FUNCTIONS  --
        --------------------------

	-- Conversion functions.
	Function Convert( Source : Quad ) return Tripple with Inline, Pure_Function;
	Function Convert( Source : Tripple ) return Quad with Inline, Pure_Function;
	Function Convert( Source : Base_64_Chunk) return Quad with Inline, Pure_Function;
	Function Convert( Source : Quad) return Base_64_Chunk with Inline, Pure_Function;
    End Internals;


    -- The encoder keeps track of two bytes, an indicator of how many
    -- bytes (mod 3) have been processed, and a sequence of 24-bit quads.
    Type Encoder is record
	Result : Internals.Internal_Data_Vector.Vector;
	B1, B2 : Interfaces.Integer_8:= Interfaces.Integer_8'First;

	Current : Internals.Internal_Count := Internals.Internal_Count'First;
    end record;

    -- Compiler bug workaround. (Unable to use the IN operator in the given context.)
    Function Not_Padding( C : Base_64_Character ) return Boolean is
      ( C in Nonpad_Character );
    Function Not_Padding( C : Character ) Return Boolean is
      ( Not_Padding( Convert(C) ) ) with Inline;


    Function Valid_Length( Length : Natural ) Return Boolean is
      (Length mod 4 = 0
	or else raise Constraint_Error with "Illegal Base64 length:"
	              & Natural'Image(Length));

    Function Not_Pading_Raise( C : Base_64_Character ) return Boolean is
      (Not_Padding(C) or else raise Constraint_Error with "Malformed Base64 string.");
    Function Not_Pading_Raise( C : Character ) return Boolean is
      (Not_Pading_Raise( Convert(C) )) with Inline;

    Function Validate_Padding( C1, C2 : Base_64_Character ) return Boolean is
      ((C1 = '=') <= (C2 = '=') or else
	  raise Constraint_Error with "Nonterminal pad character detected.");
    Function Validate_Padding( C1, C2 : Character ) return Boolean is
      (Validate_Padding( Convert(C1), Convert(C2) )) with Inline;


    Function Validate_String( S : String ) return Boolean is
      (Valid_Length(S'Length) and then
       (for all Item of S(S'First..Positive'Pred(Positive'Pred( S'Last ))) =>
	    Not_Pading_Raise(Item)) and then
       (if S'Length /= 0 then Validate_Padding( S(S'Last), S(S'Last) ))
      );

End Base_64;
