library grabber;

{$mode objfpc}{$H+}

uses
  classes, desktopduplication, windows, syncobjs, sysutils;

var
  Wrapper: TDesktopDuplicationWrapper;

procedure grabber_grab(Buffer: Pointer; Width, Height: Int32); cdecl;
begin
  Wrapper.Capture(Buffer);

  // WriteLn('Desktop Duplication FPS: ', Wrapper.FPS);
end;

procedure grabber_create(Window: HWND; var Width, Height: Int32); cdecl;
begin
  Width := GetSystemMetrics(SM_CXSCREEN);
  Height := GetSystemMetrics(SM_CYSCREEN);

  Wrapper := TDesktopDuplicationWrapper.Create();
end;

procedure grabber_destroy; cdecl;
begin
  Wrapper.Free();
end;

exports grabber_create;
exports grabber_destroy;
exports grabber_grab;

begin
end.


