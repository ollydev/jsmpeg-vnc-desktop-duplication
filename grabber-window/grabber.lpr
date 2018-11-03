library grabber;

{$mode objfpc}{$H+}

uses
  classes, desktopduplication, windows;

type
  TRGB32 = packed record B, G, R, A: UInt8; end;
  PRGB32 = ^TRGB32;

var
  Wrapper: TDesktopDuplicationWrapper;

var
  Desktop: record
    Buffer: PRGB32;
    Width: Int32;
    Height: Int32;
  end;

function GetWindowRect(Window: HWND): TRect;
var
  Rect: TRect;
begin
  GetClientRect(Window, Rect);

  Result.TopLeft := Rect.TopLeft;
  Result.BottomRight := Rect.BottomRight;

  ClientToScreen(Window, Result.TopLeft);
  ClientToScreen(Window, Result.BottomRight);
end;

function grabber_grab(Window: HWND; Buffer: PRGB32; Width, Height: Int32): Int32; cdecl;
var
  Rect: TRect;
  Y: Int32;
begin
  Result := 0;

  if IsWindow(Window) and Wrapper.Capture(Desktop.Buffer) then
  begin
    Rect := GetWindowRect(Window);

    if (Rect.Left < 0) then
      Rect.Left := 0;
    if (Rect.Top < 0) then
      Rect.Top := 0;
    if (Rect.Right > Desktop.Width) then
      Rect.Right := Desktop.Width;
    if (Rect.Bottom > Desktop.Height) then
      Rect.Bottom := Desktop.Height;

    for Y := 0 to Min(Height - 1, Rect.Height - 1) do
      Move(Desktop.Buffer[(Y + Rect.Top) * Desktop.Width + Rect.Left], Buffer[Y * Width], Min(Width, Rect.Width) * 4);

    Result := 1;
  end;
end;

procedure grabber_create(Window: HWND; var Width, Height: Int32); cdecl;
var
  Rect: TRect;
begin
  Rect := GetWindowRect(Window);

  Width := Rect.Width;
  Height := Rect.Height;

  Wrapper := TDesktopDuplicationWrapper.Create();

  Desktop.Width := GetSystemMetrics(SM_CXSCREEN);
  Desktop.Height := GetSystemMetrics(SM_CYSCREEN);
  Desktop.Buffer := GetMemory(Desktop.Width * Desktop.Height * 4);
end;

procedure grabber_destroy; cdecl;
begin
  Wrapper.Free();

  FreeMemory(Desktop.Buffer);
end;

exports grabber_create;
exports grabber_destroy;
exports grabber_grab;

begin
  WriteLn('Using desktop duplication grabber (window mode)');
  WriteLn('');
end.

