unit Vittix.Tests.FilterOperators;

interface

uses
  System.SysUtils,
  System.Variants,
  Vcl.Forms,
  DUnitX.TestFramework,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Filter.Engine,
  Vittix.DBGrid.Filter.Popup;

type
  /// <summary>
  /// Regression coverage for the shared filter operator table (roadmap A3):
  /// parse, generation and popup restoration must all come from the one
  /// authoritative definition in Vittix.DBGrid.Filter.Engine.
  /// </summary>
  [TestFixture]
  TVittixFilterOperatorTableTests = class
  private
    FOwnerForm: TForm;
    procedure AssertParsed(const AText: string; AExpectedIndex: Integer;
      const AExpectedValue: string);
  public
    [Test]
    procedure TableDefinesSixteenOperatorsInStableOrder;
    [Test]
    procedure EveryOperatorPrefixParsesBackToItself;
    [Test]
    procedure EveryOperatorRoundTripsPrefixModePrefix;
    [Test]
    procedure EveryGeneratedOperatorRestoresInPopup;
    [Test]
    procedure UnknownTextParsesAsContains;
    [Test]
    procedure WordOperatorsAreLengthDelimited;
    [Test]
    procedure NotBetweenPrefixWinsOverExclamation;
    [Test]
    procedure LongerPrefixesWinOverShorterOnes;
    [Test]
    procedure EmptyTextParsesAsContainsWithEmptyValue;
    [Test]
    procedure NotContainsAndNotEqualsModesAreDistinct;
    [Test]
    procedure IndexByPrefixReturnsContainsForUnknownPrefix;
  end;

implementation

{ TVittixFilterOperatorTableTests }

procedure TVittixFilterOperatorTableTests.AssertParsed(const AText: string;
  AExpectedIndex: Integer; const AExpectedValue: string);
var
  OperatorIndex: Integer;
  Value: string;
begin
  Assert.IsTrue(VittixFilterTryParseOperatorText(AText, OperatorIndex, Value),
    'parse succeeded for "' + AText + '"');
  Assert.AreEqual(AExpectedIndex, OperatorIndex,
    'operator index for "' + AText + '"');
  Assert.AreEqual(AExpectedValue, Value, 'value for "' + AText + '"');
end;

procedure TVittixFilterOperatorTableTests.TableDefinesSixteenOperatorsInStableOrder;
begin
  // The table order is the popup combo order and the persisted
  // OperatorIndex contract — it must never change.
  Assert.AreEqual(16, VittixFilterOperatorCount);

  Assert.AreEqual(vfmContains, VittixFilterOperatorDefinition(0).Mode);
  Assert.AreEqual('', VittixFilterOperatorDefinition(0).Prefix);
  Assert.AreEqual('Contains', VittixFilterOperatorDefinition(0).DisplayName);

  Assert.AreEqual('>=', VittixFilterOperatorDefinition(7).Prefix);
  Assert.AreEqual('Greater or Equal', VittixFilterOperatorDefinition(7).DisplayName);

  Assert.AreEqual('!..', VittixFilterOperatorDefinition(11).Prefix);
  Assert.AreEqual('Not Between', VittixFilterOperatorDefinition(11).DisplayName);

  Assert.IsTrue(VittixFilterOperatorDefinition(12).IsWordOperator);
  Assert.IsTrue(VittixFilterOperatorDefinition(13).IsWordOperator);
  Assert.IsTrue(VittixFilterOperatorDefinition(14).IsWordOperator);
  Assert.IsTrue(VittixFilterOperatorDefinition(15).IsWordOperator);
  Assert.IsFalse(VittixFilterOperatorDefinition(11).IsWordOperator);
end;

procedure TVittixFilterOperatorTableTests.EveryOperatorPrefixParsesBackToItself;
var
  I: Integer;
  Def: TVittixFilterOperatorDefinition;
  StoredText: string;
  OperatorIndex: Integer;
  Value: string;
begin
  for I := 1 to VittixFilterOperatorCount - 1 do
  begin
    Def := VittixFilterOperatorDefinition(I);
    if Def.IsWordOperator then
    begin
      StoredText := Def.Prefix;
      AssertParsed(StoredText, I, '');
    end
    else
    begin
      StoredText := Def.Prefix + 'Sample';
      AssertParsed(StoredText, I, 'Sample');
    end;
  end;

  // Contains (index 0) has NO prefix: plain text parses as no-operator.
  Assert.IsFalse(
    VittixFilterTryParseOperatorText('Sample', OperatorIndex, Value));
  Assert.AreEqual(0, OperatorIndex);
  Assert.AreEqual('Sample', Value);
end;

procedure TVittixFilterOperatorTableTests.EveryOperatorRoundTripsPrefixModePrefix;
var
  I: Integer;
  Def: TVittixFilterOperatorDefinition;
  ParseText: string;
  OperatorIndex: Integer;
  Value: string;
begin
  // prefix -> mode -> prefix: parsing the generated text must select an
  // operator whose Mode is the definition's Mode. Index 0 (Contains) has no
  // prefix; word operators take no value, so their text is the bare prefix.
  for I := 1 to VittixFilterOperatorCount - 1 do
  begin
    Def := VittixFilterOperatorDefinition(I);
    if Def.IsWordOperator then
      ParseText := Def.Prefix
    else
      ParseText := Def.Prefix + 'X';

    Assert.IsTrue(
      VittixFilterTryParseOperatorText(ParseText, OperatorIndex, Value),
      'parse for operator ' + IntToStr(I));
    Assert.AreEqual(Def.Mode,
      VittixFilterOperatorDefinition(OperatorIndex).Mode,
      'mode round-trip for operator ' + IntToStr(I));
  end;
end;

procedure TVittixFilterOperatorTableTests.EveryGeneratedOperatorRestoresInPopup;
var
  I: Integer;
  Def: TVittixFilterOperatorDefinition;
  Columns: TVittixDBGridColumns;
  Info: TVittixDBGridColumnInfo;
  Popup: TVittixDBGridFilterPopup;
  StoredText: string;
begin
  // The popup restore path (ApplySavedTextToControls) must split stored text
  // exactly like the table: operator combo index + value text.
  FOwnerForm := TForm.CreateNew(nil);
  Columns := TVittixDBGridColumns.Create(nil);
  try
    Info := Columns.Add;
    Info.FieldName := 'Name';

    for I := 0 to VittixFilterOperatorCount - 1 do
    begin
      Def := VittixFilterOperatorDefinition(I);
      if Def.IsWordOperator then
        StoredText := Def.Prefix
      else
        StoredText := Def.Prefix + 'Alpha';

      Info.FilterText := StoredText;

      Popup := TVittixDBGridFilterPopup.CreatePopup(FOwnerForm, Info);
      try
        Assert.AreEqual(I, Popup.OperatorIndex,
          'popup operator for "' + StoredText + '"');
        if I = 14 then
          // Existing UX: an 'empty' filter reloads its value combo as the
          // distinct-list display label for blank.
          Assert.AreEqual('(Blank)', Popup.FilterText,
            'popup value for word operator 14')
        else if Def.IsWordOperator then
          Assert.AreEqual('', Popup.FilterText,
            'popup value for word operator ' + IntToStr(I))
        else
          Assert.AreEqual('Alpha', Popup.FilterText,
            'popup value for "' + StoredText + '"');
      finally
        Popup.Free;
      end;
    end;
  finally
    Columns.Free;
    FOwnerForm.Free;
  end;
end;

procedure TVittixFilterOperatorTableTests.UnknownTextParsesAsContains;
var
  OperatorIndex: Integer;
  Value: string;
begin
  Assert.IsFalse(VittixFilterTryParseOperatorText('zzz unknown', OperatorIndex, Value));
  Assert.AreEqual(0, OperatorIndex);
  Assert.AreEqual('zzz unknown', Value);
end;

procedure TVittixFilterOperatorTableTests.WordOperatorsAreLengthDelimited;
var
  OperatorIndex: Integer;
  Value: string;
begin
  // 'nullity' & friends must stay plain Contains filters (Milestone 1 fix);
  // the regression is now guaranteed by the shared table. No operator means
  // the parse returns False with the full text as value.
  Assert.IsFalse(VittixFilterTryParseOperatorText('nullity', OperatorIndex, Value));
  Assert.AreEqual(0, OperatorIndex);
  Assert.AreEqual('nullity', Value);

  Assert.IsFalse(VittixFilterTryParseOperatorText('emptying', OperatorIndex, Value));
  Assert.IsFalse(VittixFilterTryParseOperatorText('nullable', OperatorIndex, Value));

  // A '!' followed by a null-ish word is Does Not Contain, not Is Not Null.
  AssertParsed('!nullable', 4, 'nullable');

  // Exact word operators still win.
  AssertParsed('null', 12, '');
  AssertParsed('!null', 13, '');
  AssertParsed('empty', 14, '');
  AssertParsed('!empty', 15, '');
end;

procedure TVittixFilterOperatorTableTests.NotBetweenPrefixWinsOverExclamation;
begin
  AssertParsed('!..2|3', 11, '2|3');
  AssertParsed('..2|3', 10, '2|3');
end;

procedure TVittixFilterOperatorTableTests.LongerPrefixesWinOverShorterOnes;
begin
  AssertParsed('>=9', 7, '9');
  AssertParsed('<=9', 9, '9');
  AssertParsed('<>abc', 5, 'abc');
  AssertParsed('>9', 6, '9');
  AssertParsed('<9', 8, '9');
end;

procedure TVittixFilterOperatorTableTests.EmptyTextParsesAsContainsWithEmptyValue;
var
  OperatorIndex: Integer;
  Value: string;
begin
  Assert.IsFalse(VittixFilterTryParseOperatorText('', OperatorIndex, Value));
  Assert.AreEqual(0, OperatorIndex);
  Assert.AreEqual('', Value);

  Assert.IsFalse(VittixFilterTryParseOperatorText('   ', OperatorIndex, Value));
  Assert.AreEqual('', Value);
end;

procedure TVittixFilterOperatorTableTests.NotContainsAndNotEqualsModesAreDistinct;
begin
  // '!' (Does Not Contain) and '<>' (Not Equals) share the table slots they
  // always had — persisted operator indexes stay valid — but they must map
  // to different match modes now: '<>' is an exact-value comparison, '!'
  // remains a substring exclusion.
  Assert.AreEqual(vfmDoesNotContain, VittixFilterOperatorDefinition(4).Mode);
  Assert.AreEqual(vfmNotEquals, VittixFilterOperatorDefinition(5).Mode);
end;

procedure TVittixFilterOperatorTableTests.IndexByPrefixReturnsContainsForUnknownPrefix;
begin
  Assert.AreEqual(0, VittixFilterOperatorIndexByPrefix('~'));
  Assert.AreEqual(0, VittixFilterOperatorIndexByPrefix(''));
  Assert.AreEqual(7, VittixFilterOperatorIndexByPrefix('>='));
  Assert.AreEqual(11, VittixFilterOperatorIndexByPrefix('!..'));
  Assert.AreEqual(14, VittixFilterOperatorIndexByPrefix('empty'));
end;

end.
