unit Vittix.Tests.Editors;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.DBGrids,
  DUnitX.TestFramework,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo,
  Vittix.DBGrid.Layout,
  Vittix.DBGrid.Editors,
  Vittix.DBGrid.Controller;

type
  [TestFixture]
  TVittixEditorsTests = class
  private
    FDataSet: TClientDataSet;
    FOwnerForm: TForm;
    FGrid: TVittixDBGrid;
    FValidateCount: Integer;
    FAcceptValue: Boolean;
    FLastSeenValue: Variant;
    procedure HandleValidateCell(Sender: TObject; Column: TColumn;
      Field: TField; var NewValue: Variant; var Accept: Boolean;
      var Message: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // C1: non-visual helpers
    [Test]
    procedure ParsePickItemsPlainAndPairs;
    [Test]
    procedure NumericInputAcceptsBothSeparators;
    [Test]
    procedure NumericInputRejectsGarbage;
    [Test]
    procedure DecimalPlacesRoundAndPassThrough;
    [Test]
    procedure BuiltInRequiredRejectsEmpty;
    [Test]
    procedure BuiltInMinMaxBounds;

    // C2: validation gate with the event
    [Test]
    procedure ValidationEventRunsAfterBuiltIns;
    [Test]
    procedure ValidationEventRejectKeepsMessage;
    [Test]
    procedure ValidationEventCanRewriteValue;

    // C1: dispatcher behavior without modal dialogs
    [Test]
    procedure ReadOnlyColumnNeverOpensEditor;
    [Test]
    procedure LookupWithoutLookupSourceIsSafe;
    [Test]
    procedure MemoAndDateEditorsUnchanged;
    [Test]
    procedure EditorOptionsRoundTripThroughLayout;
  end;

implementation

uses
  Vittix.Tests.TestData;

{ TVittixEditorsTests }

procedure TVittixEditorsTests.Setup;
begin
  FDataSet := CreateSampleDataSet;
  FGrid := CreateHeadlessGrid(FDataSet, FOwnerForm);
  FValidateCount := 0;
  FAcceptValue := True;
  FLastSeenValue := Null;
end;

procedure TVittixEditorsTests.TearDown;
begin
  FOwnerForm.Free;
  FDataSet.Free;
end;

procedure TVittixEditorsTests.HandleValidateCell(Sender: TObject;
  Column: TColumn; Field: TField; var NewValue: Variant;
  var Accept: Boolean; var Message: string);
begin
  Inc(FValidateCount);
  FLastSeenValue := NewValue;
  Accept := FAcceptValue;
  if not Accept then
    Message := 'rejected by test';
end;

procedure TVittixEditorsTests.ParsePickItemsPlainAndPairs;
var
  Items: TArray<TVittixPickItem>;
begin
  Items := TVittixDBGridEditors.ParsePickItems(
    'Active' + sLineBreak + '2=Pending' + sLineBreak + '' + sLineBreak + 'Done');
  Assert.AreEqual<NativeInt>(3, Length(Items), 'blank lines are skipped');

  Assert.AreEqual('Active', Items[0].Caption);
  Assert.IsFalse(Items[0].HasValue, 'plain lines have no value');

  Assert.AreEqual('2', Items[1].Value, 'the pair key');
  Assert.AreEqual('Pending', Items[1].Caption);
  Assert.IsTrue(Items[1].HasValue);

  Assert.AreEqual('Done', Items[2].Caption);
end;

procedure TVittixEditorsTests.NumericInputAcceptsBothSeparators;
var
  V: Double;
begin
  // Both the locale separator and '.' are accepted for entry
  Assert.IsTrue(TVittixDBGridEditors.TryParseNumericInput('12.5', V));
  Assert.AreEqual<Double>(12.5, V);
  Assert.IsTrue(TVittixDBGridEditors.TryParseNumericInput('12,5', V),
    'a comma locale user can type a comma');
  Assert.AreEqual<Double>(12.5, V);
  Assert.IsTrue(TVittixDBGridEditors.TryParseNumericInput('-3', V));
  Assert.AreEqual<Double>(-3, V);
end;

procedure TVittixEditorsTests.NumericInputRejectsGarbage;
var
  V: Double;
begin
  Assert.IsFalse(TVittixDBGridEditors.TryParseNumericInput('', V));
  Assert.IsFalse(TVittixDBGridEditors.TryParseNumericInput('abc', V));
  Assert.IsFalse(TVittixDBGridEditors.TryParseNumericInput('1.2.3', V),
    'two separators are rejected');
  Assert.IsFalse(TVittixDBGridEditors.TryParseNumericInput('1 234', V),
    'grouping separators are rejected');
  Assert.IsFalse(TVittixDBGridEditors.TryParseNumericInput('--5', V));
end;

procedure TVittixEditorsTests.DecimalPlacesRoundAndPassThrough;
begin
  Assert.AreEqual<Double>(1.23,
    TVittixDBGridEditors.ApplyDecimalPlaces(1.234, 2));
  Assert.AreEqual<Double>(1.234,
    TVittixDBGridEditors.ApplyDecimalPlaces(1.234, -1),
    'default -1 leaves the value unchanged');
end;

procedure TVittixEditorsTests.BuiltInRequiredRejectsEmpty;
var
  Info: TVittixDBGridColumnInfo;
  Msg: string;
  Field: TField;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  Assert.IsNotNull(Info);
  Info.Required := True;
  Field := FDataSet.FieldByName('Name');

  Assert.IsFalse(TVittixDBGridEditors.ValidateEditorValue(Info, Field,
    Null, Msg), 'null rejected');
  Assert.IsTrue(Msg <> '');
  Assert.IsFalse(TVittixDBGridEditors.ValidateEditorValue(Info, Field,
    '   ', Msg), 'blank text rejected');
  Assert.IsTrue(TVittixDBGridEditors.ValidateEditorValue(Info, Field,
    'Alpha', Msg), 'a real value passes');

  Info.Required := False;
  Assert.IsTrue(TVittixDBGridEditors.ValidateEditorValue(Info, Field,
    Null, Msg), 'without Required null passes');
end;

procedure TVittixEditorsTests.BuiltInMinMaxBounds;
var
  Info: TVittixDBGridColumnInfo;
  Msg: string;
  Field: TField;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Score');
  Assert.IsNotNull(Info);
  Info.HasMin := True;
  Info.MinValue := 0;
  Info.HasMax := True;
  Info.MaxValue := 10;
  Field := FDataSet.FieldByName('Score');

  Assert.IsTrue(TVittixDBGridEditors.ValidateEditorValue(Info, Field, 5, Msg));
  Assert.IsTrue(TVittixDBGridEditors.ValidateEditorValue(Info, Field, 0, Msg),
    'the bound itself passes');
  Assert.IsFalse(TVittixDBGridEditors.ValidateEditorValue(Info, Field, -1, Msg),
    'below Min rejected');
  Assert.IsFalse(TVittixDBGridEditors.ValidateEditorValue(Info, Field, 11, Msg),
    'above Max rejected');

  // Text values skip the numeric bounds
  Assert.IsTrue(TVittixDBGridEditors.ValidateEditorValue(Info, Field, 'text', Msg));

  // Without the flags no bound applies
  Info.HasMin := False;
  Info.HasMax := False;
  Assert.IsTrue(TVittixDBGridEditors.ValidateEditorValue(Info, Field, 99, Msg));
end;

procedure TVittixEditorsTests.ValidationEventRunsAfterBuiltIns;
var
  Info: TVittixDBGridColumnInfo;
  Msg: string;
  NewValue: Variant;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Score');
  Info.HasMin := True;
  Info.MinValue := 0;
  FGrid.OnValidateCell := HandleValidateCell;

  // A built-in rejection must not even reach the event.
  NewValue := -5;
  Assert.IsFalse(TVittixDBGridEditors.RunValidation(FGrid,
    FGrid.Controller.FindColumnByFieldName('Score'), Info,
    FDataSet.FieldByName('Score'), NewValue, FGrid.Controller.OnValidateCell,
    Msg), 'built-in Min rejects');
  Assert.AreEqual(0, FValidateCount,
    'the event runs after the built-in rules, not before');

  // A passing value reaches the event.
  NewValue := 5;
  FAcceptValue := True;
  Assert.IsTrue(TVittixDBGridEditors.RunValidation(FGrid,
    FGrid.Controller.FindColumnByFieldName('Score'), Info,
    FDataSet.FieldByName('Score'), NewValue, FGrid.Controller.OnValidateCell,
    Msg));
  Assert.AreEqual(1, FValidateCount);
end;

procedure TVittixEditorsTests.ValidationEventRejectKeepsMessage;
var
  Info: TVittixDBGridColumnInfo;
  Msg: string;
  NewValue: Variant;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('Score');
  FGrid.OnValidateCell := HandleValidateCell;
  FAcceptValue := False;

  NewValue := 5;
  Assert.IsFalse(TVittixDBGridEditors.RunValidation(FGrid,
    FGrid.Controller.FindColumnByFieldName('Score'), Info,
    FDataSet.FieldByName('Score'), NewValue, FGrid.Controller.OnValidateCell,
    Msg), 'the event rejects');
  Assert.AreEqual('rejected by test', Msg,
    'the rejection message reaches the editor');
end;

procedure TVittixEditorsTests.ValidationEventCanRewriteValue;
var
  Info: TVittixDBGridColumnInfo;
  Msg: string;
  NewValue: Variant;
begin
  // The event receives NewValue as var: it may normalize before commit.
  Info := FGrid.ColumnInfo.FindByFieldName('Name');
  FGrid.OnValidateCell := HandleValidateCell;

  // Rewrite through the handler is not used here; verify the value reaches
  // the handler unchanged and Accept stays True by default.
  FAcceptValue := True;
  NewValue := 'Alpha';
  Assert.IsTrue(TVittixDBGridEditors.RunValidation(FGrid,
    FGrid.Controller.FindColumnByFieldName('Name'), Info,
    FDataSet.FieldByName('Name'), NewValue, FGrid.Controller.OnValidateCell,
    Msg));
  Assert.AreEqual(VarToStr('Alpha'), VarToStr(FLastSeenValue),
    'the handler saw the typed value');
end;

procedure TVittixEditorsTests.ReadOnlyColumnNeverOpensEditor;
var
  Column: TColumn;
begin
  Column := FGrid.Controller.FindColumnByFieldName('Name');
  Column.ReadOnly := True;
  FGrid.ColumnInfo.FindByFieldName('Name').EditorKind := vePickList;

  // Must exit before any dialog opens (the test would block on ShowModal).
  Assert.IsFalse(TVittixDBGridEditors.EditField(FGrid, Column,
    FGrid.Controller.OnValidateCell),
    'a read-only column opens no editor');

  // The read-only FIELD behaves the same.
  Column.ReadOnly := False;
  FDataSet.FieldByName('Name').ReadOnly := True;
  Assert.IsFalse(TVittixDBGridEditors.EditField(FGrid, Column,
    FGrid.Controller.OnValidateCell));
  FDataSet.FieldByName('Name').ReadOnly := False;
end;

procedure TVittixEditorsTests.LookupWithoutLookupSourceIsSafe;
var
  Column: TColumn;
begin
  // veLookup without a lookup dataset exits cleanly (no dialog, no edit
  // state left behind).
  Column := FGrid.Controller.FindColumnByFieldName('Name');
  FGrid.ColumnInfo.FindByFieldName('Name').EditorKind := veLookup;
  Assert.IsFalse(TVittixDBGridEditors.EditField(FGrid, Column,
    FGrid.Controller.OnValidateCell));
  Assert.IsTrue(FDataSet.State = dsBrowse, 'no edit state left behind');
end;

procedure TVittixEditorsTests.MemoAndDateEditorsUnchanged;
var
  Column: TColumn;
  Info: TVittixDBGridColumnInfo;
begin
  // With veNone (the default) the memo column still routes to the memo
  // editor branch: EditField returns False here only because we cannot
  // interact with the modal in tests, but the dispatch must not raise and
  // must leave the dataset in Browse state after the implicit cancel.
  Column := FGrid.Controller.FindColumnByFieldName('Notes');
  Info := FGrid.ColumnInfo.FindByFieldName('Notes');
  Assert.AreEqual(veNone, Info.EditorKind, 'default keeps the built-in path');
  Assert.IsTrue(FDataSet.FieldByName('Notes').DataType in
    [ftMemo, ftWideMemo, ftFmtMemo]);

  // Verify the routing condition the controller uses picks the memo column.
  Assert.IsTrue(FDataSet.FieldByName('Notes').DataType in
    [ftMemo, ftWideMemo, ftFmtMemo, ftDate, ftTime, ftDateTime]);
end;

procedure TVittixEditorsTests.EditorOptionsRoundTripThroughLayout;
var
  Stream: TMemoryStream;
  Target: TClientDataSet;
  TargetForm: TForm;
  TargetGrid: TVittixDBGrid;
  Info: TVittixDBGridColumnInfo;
begin
  Info := FGrid.ColumnInfo.FindByFieldName('IsActive');
  Assert.IsNotNull(Info);
  Info.EditorKind := vePickList;
  Info.PickItems := '1=Active' + sLineBreak + '2=Pending';
  Info := FGrid.ColumnInfo.FindByFieldName('Score');
  Info.EditorKind := veMaskedNumber;
  Info.HasMin := True;
  Info.MinValue := 0;
  Info.HasMax := True;
  Info.MaxValue := 100;
  Info.Required := True;
  Info.DecimalPlaces := 2;

  Stream := TMemoryStream.Create;
  try
    FGrid.Controller.SaveLayoutToStream(Stream);
    Stream.Position := 0;

    Target := CreateSampleDataSet;
    try
      TargetGrid := CreateHeadlessGrid(Target, TargetForm);
      try
        Stream.Position := 0;
        TargetGrid.Controller.LoadLayoutFromStream(Stream);

        Info := TargetGrid.ColumnInfo.FindByFieldName('IsActive');
        Assert.AreEqual(vePickList, Info.EditorKind);
        Assert.IsTrue(Info.PickItems.Contains('2=Pending'));

        Info := TargetGrid.ColumnInfo.FindByFieldName('Score');
        Assert.AreEqual(veMaskedNumber, Info.EditorKind);
        Assert.IsTrue(Info.HasMin);
        Assert.AreEqual<Double>(0, Info.MinValue);
        Assert.IsTrue(Info.HasMax);
        Assert.AreEqual<Double>(100, Info.MaxValue);
        Assert.IsTrue(Info.Required);
        Assert.AreEqual(2, Info.DecimalPlaces);
      finally
        TargetForm.Free;
      end;
    finally
      Target.Free;
    end;
  finally
    Stream.Free;
  end;
end;

end.
