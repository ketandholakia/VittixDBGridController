unit Vittix.DBGrid.Styles;

{$I Vittix.DBGrid.Defines.inc}

interface

uses
  Winapi.Windows,
  System.UITypes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Themes;

/// <summary>Effective pixels-per-inch for AControl (its font) or the screen
/// when nil. Never returns a non-positive value.</summary>
function CurrentPPI(AControl: TWinControl = nil): Integer;

/// <summary>Scales a 96-DPI pixel value to AControl's current PPI.</summary>
function ScaleForPPI(APixels: Integer; AControl: TWinControl = nil): Integer;

type
  /// <summary>
  /// Style-aware color resolution for the Vittix grid UI surfaces (footer
  /// panel, filter popup, export dialog). Every accessor resolves a system
  /// color through the active VCL style service. When no custom style is
  /// active the system color is returned unchanged, so unstyled rendering
  /// stays pixel-identical to the previous hard-coded values.
  /// </summary>
  TVittixStylePalette = record
  public
    /// <summary>True when a custom (non-system) VCL style is active for the
    /// given control, or globally when AControl is nil.</summary>
    class function IsCustomStyleActive(AControl: TWinControl = nil): Boolean; static;

    /// <summary>The style service to resolve colors with: per-control when a
    /// control is given, else the globally active style.</summary>
    class function ServicesFor(AControl: TWinControl = nil): TCustomStyleServices; static;

    /// <summary>Resolves ASystemColor through AServices; nil services return
    /// the color unchanged. Overload for tests and callers that already hold
    /// a style service.</summary>
    class function ColorOf(AServices: TCustomStyleServices;
      ASystemColor: TColor): TColor; static;

    /// <summary>3D highlight edge (clBtnHighlight).</summary>
    class function HighlightColor(AControl: TWinControl = nil): TColor; static;

    /// <summary>3D shadow edge (clBtnShadow).</summary>
    class function ShadowColor(AControl: TWinControl = nil): TColor; static;

    /// <summary>Button face background (clBtnFace).</summary>
    class function FaceColor(AControl: TWinControl = nil): TColor; static;

    /// <summary>Text color (clBtnText).</summary>
    class function TextColor(AControl: TWinControl = nil): TColor; static;

    /// <summary>Editable-surface background (clWindow).</summary>
    class function WindowColor(AControl: TWinControl = nil): TColor; static;

    /// <summary>Validation error color (clRed).</summary>
    class function ErrorColor(AControl: TWinControl = nil): TColor; static;
  end;

implementation

function CurrentPPI(AControl: TWinControl): Integer;
var
  DC: HDC;
begin
  Result := 0;
  if Assigned(AControl) and AControl.HandleAllocated then
  begin
    DC := GetDC(AControl.Handle);
    try
      Result := GetDeviceCaps(DC, LOGPIXELSY);
    finally
      ReleaseDC(AControl.Handle, DC);
    end;
  end;
  if Result <= 0 then
    Result := Screen.PixelsPerInch;
  if Result <= 0 then
    Result := 96;
end;

function ScaleForPPI(APixels: Integer; AControl: TWinControl): Integer;
begin
  Result := MulDiv(APixels, CurrentPPI(AControl), 96);
end;

class function TVittixStylePalette.ServicesFor(
  AControl: TWinControl): TCustomStyleServices;
begin
{$IFDEF VX_STYLESERVICES_CONTROL}
  if Assigned(AControl) then
    Result := StyleServices(AControl)
  else
    Result := StyleServices;
{$ELSE}
  // Delphi 10.3: styles are application-wide; no per-control lookup exists.
  Result := StyleServices;
{$ENDIF}
end;

class function TVittixStylePalette.IsCustomStyleActive(
  AControl: TWinControl): Boolean;
var
  Services: TCustomStyleServices;
begin
  Services := ServicesFor(AControl);
  Result := Assigned(Services) and Services.Enabled and
    not Services.IsSystemStyle;
end;

class function TVittixStylePalette.ColorOf(AServices: TCustomStyleServices;
  ASystemColor: TColor): TColor;
begin
  if Assigned(AServices) then
    Result := AServices.GetSystemColor(ASystemColor)
  else
    Result := ASystemColor;
end;

class function TVittixStylePalette.HighlightColor(AControl: TWinControl): TColor;
begin
  Result := ColorOf(ServicesFor(AControl), clBtnHighlight);
end;

class function TVittixStylePalette.ShadowColor(AControl: TWinControl): TColor;
begin
  Result := ColorOf(ServicesFor(AControl), clBtnShadow);
end;

class function TVittixStylePalette.FaceColor(AControl: TWinControl): TColor;
begin
  Result := ColorOf(ServicesFor(AControl), clBtnFace);
end;

class function TVittixStylePalette.TextColor(AControl: TWinControl): TColor;
begin
  Result := ColorOf(ServicesFor(AControl), clBtnText);
end;

class function TVittixStylePalette.WindowColor(AControl: TWinControl): TColor;
begin
  Result := ColorOf(ServicesFor(AControl), clWindow);
end;

class function TVittixStylePalette.ErrorColor(AControl: TWinControl): TColor;
begin
  Result := ColorOf(ServicesFor(AControl), clRed);
end;

end.
