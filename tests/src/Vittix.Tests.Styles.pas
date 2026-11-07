unit Vittix.Tests.Styles;

interface

uses
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Themes,
  DUnitX.TestFramework,
  Vittix.DBGrid.Styles;

type
  [TestFixture]
  TVittixStylesTests = class
  public
    [Test]
    procedure SystemStyleResolvesSystemColorsIdentically;
    [Test]
    procedure NilServicesReturnColorsUnchanged;
    [Test]
    procedure DefaultResolutionMatchesSystemColorsWhenNoCustomStyle;
    [Test]
    procedure CustomStyleActiveIsFalseWithoutCustomStyles;
    [Test]
    procedure ControlScopedResolutionMatchesGlobalResolution;
    [Test]
    procedure PaletteCoversFooterAndDialogSurfaces;
  end;

implementation

{ TVittixStylesTests }

procedure TVittixStylesTests.SystemStyleResolvesSystemColorsIdentically;
begin
  // The system style resolves every system color to the live GetSysColor
  // value; GDI paints the symbolic constant as exactly that RGB, so this is
  // what keeps unstyled rendering pixel-identical to the old hard-coded
  // pen/brush colors.
  Assert.AreEqual(Integer(GetSysColor(COLOR_BTNFACE)),
    Integer(TVittixStylePalette.ColorOf(TStyleManager.SystemStyle, clBtnFace)));
  Assert.AreEqual(Integer(GetSysColor(COLOR_BTNHIGHLIGHT)),
    Integer(TVittixStylePalette.ColorOf(TStyleManager.SystemStyle, clBtnHighlight)));
  Assert.AreEqual(Integer(GetSysColor(COLOR_BTNSHADOW)),
    Integer(TVittixStylePalette.ColorOf(TStyleManager.SystemStyle, clBtnShadow)));
  Assert.AreEqual(Integer(GetSysColor(COLOR_BTNTEXT)),
    Integer(TVittixStylePalette.ColorOf(TStyleManager.SystemStyle, clBtnText)));
  Assert.AreEqual(Integer(GetSysColor(COLOR_WINDOW)),
    Integer(TVittixStylePalette.ColorOf(TStyleManager.SystemStyle, clWindow)));
end;

procedure TVittixStylesTests.NilServicesReturnColorsUnchanged;
begin
  Assert.AreEqual(Integer(clBtnFace),
    Integer(TVittixStylePalette.ColorOf(nil, clBtnFace)));
end;

procedure TVittixStylesTests.DefaultResolutionMatchesSystemColorsWhenNoCustomStyle;
begin
  if TVittixStylePalette.IsCustomStyleActive then
    // A custom style got linked into this binary after all; the fallback
    // assertion would not hold. Skip instead of failing.
    Assert.Pass('Custom style active; system-color fallback not applicable')
  else
  begin
    // Without a custom style the palette resolves through the system style,
    // i.e. to the live GetSysColor values the unstyled GDI drawing uses.
    Assert.AreEqual(Integer(GetSysColor(COLOR_BTNFACE)),
      Integer(TVittixStylePalette.FaceColor));
    Assert.AreEqual(Integer(GetSysColor(COLOR_BTNHIGHLIGHT)),
      Integer(TVittixStylePalette.HighlightColor));
    Assert.AreEqual(Integer(GetSysColor(COLOR_BTNSHADOW)),
      Integer(TVittixStylePalette.ShadowColor));
    Assert.AreEqual(Integer(GetSysColor(COLOR_BTNTEXT)),
      Integer(TVittixStylePalette.TextColor));
    Assert.AreEqual(Integer(GetSysColor(COLOR_WINDOW)),
      Integer(TVittixStylePalette.WindowColor));
    // clRed is a literal RGB, not a system-color constant: the system style
    // passes it through unchanged.
    Assert.AreEqual(Integer(clRed), Integer(TVittixStylePalette.ErrorColor));
  end;
end;

procedure TVittixStylesTests.CustomStyleActiveIsFalseWithoutCustomStyles;
begin
  if TStyleManager.StyleNames = nil then
    Assert.Pass('No styles registered at all')
  else
    // The test binary links no .vsf resources, so only the system style
    // exists and no custom style can be active for the fixture form or
    // globally.
    Assert.IsFalse(TVittixStylePalette.IsCustomStyleActive);
end;

procedure TVittixStylesTests.ControlScopedResolutionMatchesGlobalResolution;
var
  Form: TForm;
begin
  Form := TForm.CreateNew(nil);
  try
    if TVittixStylePalette.IsCustomStyleActive(Form) then
      Assert.Pass('Custom style active; per-control check not applicable')
    else
    begin
      Assert.AreEqual(Integer(TVittixStylePalette.FaceColor),
        Integer(TVittixStylePalette.FaceColor(Form)),
        'per-control face color matches the global resolution');
      Assert.AreEqual(Integer(TVittixStylePalette.TextColor),
        Integer(TVittixStylePalette.TextColor(Form)),
        'per-control text color matches the global resolution');
      Assert.AreSame(TVittixStylePalette.ServicesFor,
        TVittixStylePalette.ServicesFor(Form),
        'the same style service instance is used');
    end;
  finally
    Form.Free;
  end;
end;

procedure TVittixStylesTests.PaletteCoversFooterAndDialogSurfaces;
begin
  // Every accessor the styled surfaces rely on must return a real color
  // (not clNone) whether or not a custom style is active.
  Assert.AreNotEqual(Integer(clNone), Integer(TVittixStylePalette.FaceColor));
  Assert.AreNotEqual(Integer(clNone), Integer(TVittixStylePalette.HighlightColor));
  Assert.AreNotEqual(Integer(clNone), Integer(TVittixStylePalette.ShadowColor));
  Assert.AreNotEqual(Integer(clNone), Integer(TVittixStylePalette.TextColor));
  Assert.AreNotEqual(Integer(clNone), Integer(TVittixStylePalette.WindowColor));
  Assert.AreNotEqual(Integer(clNone), Integer(TVittixStylePalette.ErrorColor));
end;

end.
