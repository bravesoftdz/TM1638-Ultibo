unit TM1638;

interface

uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo;

type
  TShiftOrder = (soLsbFirst = 0, soMsbFirst = 1);

  { TTM1630 }

  TTM1630 = class
  private
    strobe_: Integer;
    clock_: Integer;
    data_: Integer;
    procedure Setup();
    procedure Reset();
    procedure ShiftOut(data, clock: LongWord; order: TShiftOrder; output: Byte);
    function ShiftIn(data, clock: LongWord; order: TShiftOrder): Byte;
  public
    constructor Create; overload;
    constructor Create(strobe, clock, data: Integer); overload;
    procedure ShiftOut(order: TShiftOrder; output: Byte);
    function ShiftIn(order: TShiftOrder): Byte;
    procedure SendCommand(value: Byte);
    function ReadButtons: Byte;
    procedure SetLed(value, position: Byte);
    procedure Start();
    procedure Stop();
  end;

implementation

const
  strobePin = GPIO_PIN_23;
  clockPin = GPIO_PIN_22;
  dataPin = GPIO_PIN_17;

procedure TTM1630.ShiftOut(data, clock: LongWord; order: TShiftOrder; output: Byte);
var
  i, mask: word;
begin
  if order = soLsbFirst then
    for i := 7 downto 0 do
    begin
      mask := 1 shl i; // calculate bitmask
      GPIOOutputSet(clock, GPIO_LEVEL_LOW);  // set clock to 0

      // Send one bit on the data pin
      if (output and mask) <> 0 then
        GPIOOutputSet(data, GPIO_LEVEL_HIGH)
      else
        GPIOOutputSet(data, GPIO_LEVEL_LOW);

      GPIOOutputSet(clock, GPIO_LEVEL_HIGH);  // set clock to 1
    end
  else
    for i := 0 to 7 do
    begin
      mask := 1 shl i; // calculate bitmask
      GPIOOutputSet(clock, GPIO_LEVEL_LOW);  // set clock to 0

      // Send one bit on the data pin
      if (output and mask) <> 0 then
        GPIOOutputSet(data, GPIO_LEVEL_HIGH)
      else
        GPIOOutputSet(data, GPIO_LEVEL_LOW);

      GPIOOutputSet(clock, GPIO_LEVEL_HIGH);  // set clock to 1
    end;
end;

function TTM1630.ShiftIn(data, clock: LongWord; order: TShiftOrder): Byte;
var
  i, mask, v: word;
begin
  Result := 0;

  if order = soLsbFirst then
    for i := 7 downto 0 do
    begin
      GPIOOutputSet(clock, GPIO_LEVEL_LOW);  // set clock to 0
      v := GPIOInputGet(data);

      if v <> 0 then
      begin
        mask := 1 shl i; // calculate bitmask
        Result := Result or mask;
      end;

      GPIOOutputSet(clock, GPIO_LEVEL_HIGH);  // set clock to 1
    end
  else
    for i := 0 to 7 do
    begin
      GPIOOutputSet(clock, GPIO_LEVEL_LOW);  // set clock to 0
      v := GPIOInputGet(data);

      if v <> 0 then
      begin
        mask := 1 shl i; // calculate bitmask
        Result := Result or mask;
      end;

      GPIOOutputSet(clock, GPIO_LEVEL_HIGH);  // set clock to 1
    end;
end;

procedure TTM1630.SendCommand(value: Byte);
begin
  GPIOOutputSet(strobe_, GPIO_LEVEL_LOW);
  ShiftOut(data_, clock_, soLsbFirst, value);
  GPIOOutputSet(strobe_, GPIO_LEVEL_HIGH);
end;

procedure TTM1630.Reset();
var
  i: Byte;
begin
  SendCommand($40); // set auto increment mode
  SendCommand($c0); // set starting address to 0

  for i := 0 to 15 do
    ShiftOut(data_, clock_, soLsbFirst, $00);

  GPIOOutputSet(strobe_, GPIO_LEVEL_HIGH);
end;

constructor TTM1630.Create;
begin
  inherited Create;

  strobe_ := strobePin;
  clock_ := clockPin;
  data_ := dataPin;
end;

constructor TTM1630.Create(strobe, clock, data: Integer);
begin
  inherited Create;

  strobe_ := strobe;
  clock_ := clock;
  data_ := data;
end;

procedure TTM1630.ShiftOut(order: TShiftOrder; output: Byte);
begin
  ShiftOut(data_, clock_, order, output);
end;

function TTM1630.ShiftIn(order: TShiftOrder): Byte;
begin
  Result := ShiftIn(data_, clock_, order);
end;

procedure TTM1630.Setup();
begin
  GPIOPullSelect(strobe_, GPIO_PULL_NONE);
  GPIOFunctionSelect(strobe_, GPIO_FUNCTION_OUT);

  GPIOPullSelect(clock_, GPIO_PULL_NONE);
  GPIOFunctionSelect(clock_, GPIO_FUNCTION_OUT);

  GPIOPullSelect(data_, GPIO_PULL_NONE);
  GPIOFunctionSelect(data_, GPIO_FUNCTION_OUT);

  SendCommand($40);
  SendCommand($8f);  // activate

  Reset();

  SendCommand($44);  // set single address
end;

function TTM1630.ReadButtons: Byte;
var
  i, v, buttons: Byte;
begin
  buttons := 0;

  GPIOOutputSet(strobe_, GPIO_LEVEL_LOW);
  ShiftOut(data_, clock_, soLsbFirst, $42);

  GPIOFunctionSelect(data_, GPIO_FUNCTION_IN);

  for i := 0 to 3 do
  begin
    v := ShiftIn(data_, clock_, soLsbFirst) shl i;
    buttons := buttons or v;
  end;

  GPIOFunctionSelect(data_, GPIO_FUNCTION_OUT);

  GPIOOutputSet(strobe_, GPIO_LEVEL_HIGH);
  Result := buttons;
end;

procedure TTM1630.SetLed(value, position: Byte);
begin
  GPIOFunctionSelect(data_, GPIO_FUNCTION_OUT);
  SendCommand($44);
  GPIOOutputSet(strobe_, GPIO_LEVEL_LOW);
  ShiftOut(data_, clock_, soLsbFirst, $C1 + (position shl 1));
  ShiftOut(data_, clock_, soLsbFirst, value);
  GPIOOutputSet(strobe_, GPIO_LEVEL_HIGH);
end;

procedure TTM1630.Start;
begin
  GPIOOutputSet(strobe_, GPIO_LEVEL_LOW);
end;

procedure TTM1630.Stop;
begin
  GPIOOutputSet(strobe_, GPIO_LEVEL_HIGH);
end;

end.

