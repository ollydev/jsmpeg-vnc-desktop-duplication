library grabber;

{$mode objfpc}{$H+}

uses
  classes, desktopduplication, windows;

var
  Wrapper: TDesktopDuplicationWrapper;

function grabber_grab(Window: HWND; Buffer: Pointer; Width, Height: Int32): Int32; cdecl;
begin
  Result := 0;

  if Wrapper.Capture(Buffer) then
    Result := 1;
end;

procedure grabber_create(Window: HWND; var Width, Height: Int32); cdecl;
begin
  Wrapper := TDesktopDuplicationWrapper.Create();

  Width := GetSystemMetrics(SM_CXSCREEN);
  Height := GetSystemMetrics(SM_CYSCREEN);
end;

procedure grabber_destroy; cdecl;
begin
  Wrapper.Free();
end;

exports grabber_create;
exports grabber_destroy;
exports grabber_grab;

begin
  WriteLn('Using desktop duplication grabber (desktop mode)');
  WriteLn('');
end.


