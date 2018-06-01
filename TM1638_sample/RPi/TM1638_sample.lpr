program TM1638_sample;

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
    ConsoleWindowWriteLn(Handle, 'TM1638 sample');

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
var
  i: Byte;
begin
  try
    i := 0;

    while i <= 15 do
    begin
      TM1630Ac.Start();
      TM1630Ac.ShiftOut(soLsbFirst, $C1 + i); // led on
      TM1630Ac.ShiftOut(soLsbFirst, 1);
      TM1630Ac.Stop();
      Sleep(100);
      TM1630Ac.Start();
      TM1630Ac.ShiftOut(soLsbFirst, $C1 + i); // led off
      TM1630Ac.ShiftOut(soLsbFirst, 0);
      TM1630Ac.Stop();
      Sleep(10);
      Inc(i, 2);
    end;
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

