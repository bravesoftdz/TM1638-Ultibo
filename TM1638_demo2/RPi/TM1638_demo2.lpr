program TM1638_demo2;


{$mode objfpc}{$H+}

{ Raspberry Pi Application                                                     }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  Console,
  TM1638;

var
  TM1630Ac: TTM1630;
  Handle: TWindowHandle;

(*
// definition for the displayable ASCII chars
const
  FONT_DEFAULT: array [] of Byte = (
  0b00000000, // (32)  <space>
  0b10000110, // (33)  !
  0b00100010, // (34) "
  0b01111110, // (35) #
  0b01101101, // (36) $
  0b00000000, // (37) %
  0b00000000, // (38) &
  0b00000010, // (39) '
  0b00110000, // (40) (
  0b00000110, // (41) )
  0b01100011, // (42) *
  0b00000000, // (43) +
  0b00000100, // (44) ,
  0b01000000, // (45) -
  0b10000000, // (46) .
  0b01010010, // (47) /
  0b00111111, // (48) 0
  0b00000110, // (49) 1
  0b01011011, // (50) 2
  0b01001111, // (51) 3
  0b01100110, // (52) 4
  0b01101101, // (53) 5
  0b01111101, // (54) 6
  0b00100111, // (55) 7
  0b01111111, // (56) 8
  0b01101111, // (57) 9
  0b00000000, // (58) :
  0b00000000, // (59) ;
  0b00000000, // (60) <
  0b01001000, // (61) =
  0b00000000, // (62) >
  0b01010011, // (63) ?
  0b01011111, // (64) @
  0b01110111, // (65) A
  0b01111111, // (66) B
  0b00111001, // (67) C
  0b00111111, // (68) D
  0b01111001, // (69) E
  0b01110001, // (70) F
  0b00111101, // (71) G
  0b01110110, // (72) H
  0b00000110, // (73) I
  0b00011111, // (74) J
  0b01101001, // (75) K
  0b00111000, // (76) L
  0b00010101, // (77) M
  0b00110111, // (78) N
  0b00111111, // (79) O
  0b01110011, // (80) P
  0b01100111, // (81) Q
  0b00110001, // (82) R
  0b01101101, // (83) S
  0b01111000, // (84) T
  0b00111110, // (85) U
  0b00101010, // (86) V
  0b00011101, // (87) W
  0b01110110, // (88) X
  0b01101110, // (89) Y
  0b01011011, // (90) Z
  0b00111001, // (91) [
  0b01100100, // (92) \ (this can't be the last char on a line, even in comment or it'll concat)
  0b00001111, // (93) ]
  0b00000000, // (94) ^
  0b00001000, // (95) _
  0b00100000, // (96) `
  0b01011111, // (97) a
  0b01111100, // (98) b
  0b01011000, // (99) c
  0b01011110, // (100)  d
  0b01111011, // (101)  e
  0b00110001, // (102)  f
  0b01101111, // (103)  g
  0b01110100, // (104)  h
  0b00000100, // (105)  i
  0b00001110, // (106)  j
  0b01110101, // (107)  k
  0b00110000, // (108)  l
  0b01010101, // (109)  m
  0b01010100, // (110)  n
  0b01011100, // (111)  o
  0b01110011, // (112)  p
  0b01100111, // (113)  q
  0b01010000, // (114)  r
  0b01101101, // (115)  s
  0b01111000, // (116)  t
  0b00011100, // (117)  u
  0b00101010, // (118)  v
  0b00011101, // (119)  w
  0b01110110, // (120)  x
  0b01101110, // (121)  y
  0b01000111, // (122)  z
  0b01000110, // (123)  {
  0b00000110, // (124)  |
  0b01110000, // (125)  }
  0b00000001, // (126)  ~
);
*)

function Counting(): Byte;
var
  position: Byte;

const                             {0}  {1}  {2}  {3}  {4}  {5}  {6}  {7}  {8}  {9}
  digits: array[0..9] of Byte  = ($3f, $06, $5b, $4f, $66, $6d, $7d, $07, $7f, $6f);
  digit: Byte = 0;
begin
  TM1630Ac.SendCommand($40);
  TM1630Ac.Start();
  TM1630Ac.ShiftOut(soLsbFirst, $C0);

  for position := 0 to 7 do
  begin
    TM1630Ac.ShiftOut(soLsbFirst, digits[digit]);
    TM1630Ac.ShiftOut(soLsbFirst, $00);
  end;

  {
  for position := 0 to 7 do
  begin
    TM1630Ac.ShiftOut(soLSBFIRST, FONT_DEFAULT[digit]);
    TM1630Ac.ShiftOut(LSBFIRST, 0x00);
  end;
  }

  TM1630Ac.Stop();

  digit := (digit + 1) mod 10;

  if digit = 0 then
    Result := 1
  else
    Result := 0;
end;

function Scroll(): Byte;
var
  scrollLength, i, c: Byte;
const
  scrollText: array [0..31] of Byte =
     (* *)(* *)(* *)(* *)(* *)(* *)(* *)(* *)
    ($00, $00, $00, $00, $00, $00, $00, $00,
     (*H*)(*E*)(*L*)(*L*)(*O*)(*.*)(*.*)(*.*)
     $76, $79, $38, $38, $3f, $80, $80, $80,
     (* *)(* *)(* *)(* *)(* *)(* *)(* *)(* *)
     $00, $00, $00, $00, $00, $00, $00, $00,
     (*H*)(*E*)(*L*)(*L*)(*O*)(*.*)(*.*)(*.*)
     $76, $79, $38, $38, $3f, $80, $80, $80);
  index: Byte = 0;
begin
  scrollLength := Length(scrollText);
  TM1630Ac.SendCommand($40);
  TM1630Ac.Start();
  TM1630Ac.ShiftOut(soLsbFirst, $C0);

  for i := 0 to 7 do
  begin
    c := scrollText[(index + i) mod scrollLength];
    TM1630Ac.ShiftOut(soLsbFirst, c);

    if c <> 0 then
      TM1630Ac.ShiftOut(soLsbFirst, 1)
    else
      TM1630Ac.ShiftOut(soLsbFirst, 0);
  end;

  TM1630Ac.Stop();
  index := (index + 1) mod (scrollLength shl 1);

  if index = 0 then
    Result := 1
  else
    Result := 0;
end;

procedure Buttons();
var
  textStartPos, position, buttons, mask: Byte;

const
  promptText: array [0..31] of Byte =
     (*P*) (*r*) (*E*) (*S*) (*S*) (* *) (* *) (* *)
    ($73, $50, $79, $6d, $6d, $00, $00, $00,
     (* *) (* *) (* *) (* *) (* *) (* *) (* *) (* *)
     $00, $00, $00, $00, $00, $00, $00, $00,
     (*b*) (*u*) (*t*) (*t*) (*o*) (*n*) (*S*) (* *)
     $7c, $1c, $78, $78, $5c, $54, $6d, $00,
     (* *) (* *) (* *) (* *) (* *) (* *) (* *) (* *)
     $00, $00, $00, $00, $00, $00, $00, $00);

  block: Byte = 0;
begin
  textStartPos := (block div 4) shl 3;

  for position := 0 to 7 do
  begin
    TM1630Ac.SendCommand($44);
    TM1630Ac.Start();
    TM1630Ac.ShiftOut(soLsbFirst, $C0 + (position << 1));
    TM1630Ac.ShiftOut(soLsbFirst, promptText[textStartPos + position]);
    TM1630Ac.Stop();
  end;

  block := (block + 1) mod 16;

  buttons := TM1630Ac.ReadButtons();

  for position := 0 to 7 do
  begin
    mask := $1 shl position;

    if buttons and mask = 0 then
      TM1630Ac.SetLed(0, position)
    else
      TM1630Ac.SetLed(1, position);
  end;
end;

procedure Setup();
begin
  {Let's create a console window again but this time on the left side of the screen}
  Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  {To prove that worked let's output some text on the console window}
  ConsoleWindowWriteLn(Handle, 'TM1638 demo 2');

  try
    TM1630Ac := TTM1630.Create;
  except
    on E: Exception do
    begin
      TM1630Ac := nil;
      ConsoleWindowWriteLn(Handle, 'Setup() error: ' + E.Message);
    end;
  end;
end;

const
  COUNTING_MODE = 0;
  SCROLL_MODE = 1;
  BUTTON_MODE = 2;


procedure Loop();
const
  mode: Byte = COUNTING_MODE;
begin
  try
    case mode of
      COUNTING_MODE: Inc(mode, Counting());
      SCROLL_MODE: Inc(mode, Scroll());
      BUTTON_MODE: Buttons();
    end;

    Sleep(200);
  except
    on E: Exception do
    begin
      TM1630Ac.Free;
      TM1630Ac := nil;
      ConsoleWindowWriteLn(Handle, 'Loop() error: ' + E.Message);
    end;
  end;
end;

begin
  Setup();

  while Assigned(TM1630Ac) do
    Loop();

  ConsoleWindowWriteLn(Handle, '');
  ConsoleWindowWriteLn(Handle, 'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

