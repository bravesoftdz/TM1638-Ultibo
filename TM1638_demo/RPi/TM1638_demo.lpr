program TM1638_demo;

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
  index:  Byte = 0;
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
  ConsoleWindowWriteLn(Handle, 'TM1638 demo');

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

