unit desktopduplication;

// based on https://github.com/tothpaul/Delphi/tree/master/DesktopDuplicationAPI

interface

uses
  Windows,
  DX12.D3D11,
  DX12.D3DCommon,
  DX12.DXGI,
  DX12.DXGI1_2;

type
  TDesktopDuplicationWrapper = class
  private
    FError: HRESULT;
    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FFeatureLevel: TD3D_FEATURE_LEVEL;
    FOutput: TDXGI_OUTPUT_DESC;
    FDuplicate: IDXGIOutputDuplication;
    FFrames: UInt64;
    FStartTime: Double;

    function GetFPS: Int32;
  public
    property Error: HRESULT read FError;
    property Frames: UInt64 read FFrames;
    property FPS: Int32 read GetFPS;

    procedure Capture(Buffer: Pointer);

    constructor Create;
  end;

implementation

function PerformanceTimer: Double;
var
  frequency, counter: Int64;
begin
  if QueryPerformanceFrequency(frequency) and QueryPerformanceCounter(counter) then
    Result := counter / frequency * 1000
  else
    Result := GetTickCount();
end;

constructor TDesktopDuplicationWrapper.Create;
var
  GI: IDXGIDevice;
  GA: IDXGIAdapter;
  GO: IDXGIOutput;
  O1: IDXGIOutput1;
begin
  FError := D3D11CreateDevice(
    nil,
    D3D_DRIVER_TYPE_HARDWARE,
    0,
    Ord(D3D11_CREATE_DEVICE_SINGLETHREADED),
    nil, 0,
    D3D11_SDK_VERSION,
    FDevice,
    FFeatureLevel,
    FContext
  );
  if Failed(FError) then
    Exit;

  FError := FDevice.QueryInterface(IID_IDXGIDevice, GI);
  if Failed(FError) then
    Exit;

  FError := GI.GetParent(IID_IDXGIAdapter, Pointer(GA));
  if Failed(FError) then
    Exit;

  FError := GA.EnumOutputs(0, GO);
  if Failed(FError) then
    Exit;

  FError := GO.GetDesc(FOutput);
  if Failed(FError) then
    Exit;

  FError := GO.QueryInterface(IID_IDXGIOutput1, O1);
  if Failed(FError) then
    Exit;

  FError := O1.DuplicateOutput(FDevice, FDuplicate);
  if Failed(FError) then
    Exit;
end;

function TDesktopDuplicationWrapper.GetFPS: Int32;
begin
  if (FStartTime = 0) then
    FStartTime := PerformanceTimer()
  else
    Result := Round(1000 / ((PerformanceTimer() - FStartTime) / FFrames));
end;

procedure TDesktopDuplicationWrapper.Capture(Buffer: Pointer);
var
  FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
  Resource: IDXGIResource;
  Texture: ID3D11Texture2D;

  procedure UpdateBuffer;
  var
    Desc: TD3D11_TEXTURE2D_DESC;
    Copy: ID3D11Texture2D;
    Resource: TD3D11_MAPPED_SUBRESOURCE;
  begin
    Texture.GetDesc(Desc);

    Desc.BindFlags := 0;
    Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ) or Ord(D3D11_CPU_ACCESS_WRITE);
    Desc.Usage := D3D11_USAGE_STAGING;
    Desc.MiscFlags := 0;

    FError := FDevice.CreateTexture2D(@Desc, nil, Copy);
    if Failed(FError) then
      Exit;

    FContext.CopyResource(Copy, Texture);
    FContext.Map(Copy, 0, D3D11_MAP_READ_WRITE, 0, Resource);

    Move(Resource.PData^, Buffer^, Desc.Width * Desc.Height * 4);
  end;

begin
  FError := FDuplicate.AcquireNextFrame(16, FrameInfo, Resource);

  if Succeeded(FError) then
  begin
    if (FrameInfo.TotalMetadataBufferSize > 0) and (FrameInfo.LastPresentTime.HighPart > 0) then // no to mouse updates.
    begin
      Resource.QueryInterface(IID_ID3D11Texture2D, Texture);

      UpdateBuffer();

      Inc(FFrames);
    end;

    FDuplicate.ReleaseFrame();
  end;
end;

end.
