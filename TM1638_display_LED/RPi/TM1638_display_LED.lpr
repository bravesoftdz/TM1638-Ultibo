program TM1638_display_LED;

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

  procedure Setup();
  begin
    {Let's create a console window again but this time on the left side of the screen}
    Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

    {To prove that worked let's output some text on the console window}
    ConsoleWindowWriteLn(Handle, 'TM1638 Display LED');

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

procedure Loop();
begin
  try
    TM1630Ac.SendCommand($44);  // set single address

    TM1630Ac.Start();
    TM1630Ac.ShiftOut(soLsbFirst, $c0); // 1st digit
    TM1630Ac.ShiftOut(soLsbFirst, $ff);
    TM1630Ac.Stop();

    TM1630Ac.Start();
    TM1630Ac.ShiftOut(soLsbFirst, $c5); // 3rd LED
    TM1630Ac.ShiftOut(soLsbFirst, $01);
    TM1630Ac.Stop();

    TM1630Ac.Start();
    TM1630Ac.ShiftOut(soLsbFirst, $cb); // 6th LED
    TM1630Ac.ShiftOut(soLsbFirst, $01);
    TM1630Ac.Stop();

    TM1630Ac.Start();
    TM1630Ac.ShiftOut(soLsbFirst, $ce); // last digit
    TM1630Ac.ShiftOut(soLsbFirst, $ff);
    TM1630Ac.Stop();
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

