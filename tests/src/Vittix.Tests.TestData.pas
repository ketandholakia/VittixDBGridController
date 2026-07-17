unit Vittix.Tests.TestData;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Datasnap.DBClient,
  Data.DB,
  Vcl.Forms,
  Vcl.DBGrids,
  Vittix.DBGrid,
  Vittix.DBGrid.ColumnInfo;

function CreateSampleDataSet: TClientDataSet;
function CreateLargeDataSet(ARowCount: Integer): TClientDataSet;
function CreateWideDataSet(AFieldCount: Integer): TClientDataSet;
function CreateMatchingColumns(ADataSet: TDataSet): TVittixDBGridColumns;
function CreateHeadlessGrid(ADataSet: TDataSet; out AOwnerForm: TForm): TVittixDBGrid;
function CountVisibleRecords(ADataSet: TDataSet): Integer;

implementation

function CreateSampleDataSet: TClientDataSet;
begin
  Result := TClientDataSet.Create(nil);
  with Result.FieldDefs do
  begin
    Add('ID', ftInteger);
    Add('Name', ftString, 50);
    Add('Amount', ftCurrency);
    Add('Score', ftFloat);
    Add('Notes', ftMemo);
    Add('Created', ftDateTime);
    Add('IsActive', ftBoolean);
  end;

  Result.CreateDataSet;
  Result.Open;

  Result.Append;
  Result.FieldByName('ID').AsInteger := 1;
  Result.FieldByName('Name').AsString := 'Alpha';
  Result.FieldByName('Amount').AsCurrency := 100.50;
  Result.FieldByName('Score').AsFloat := 3.5;
  Result.FieldByName('Notes').AsString := 'first, "quoted"' + sLineBreak + 'second line';
  Result.FieldByName('Created').AsDateTime := EncodeDate(2026, 1, 10);
  Result.FieldByName('IsActive').AsBoolean := True;
  Result.Post;

  Result.Append;
  Result.FieldByName('ID').AsInteger := 2;
  Result.FieldByName('Name').AsString := 'beta';
  Result.FieldByName('Amount').AsCurrency := 200.00;
  Result.FieldByName('Score').AsFloat := 7.25;
  Result.FieldByName('Notes').Clear;
  Result.FieldByName('Created').AsDateTime := EncodeDate(2026, 2, 15);
  Result.FieldByName('IsActive').AsBoolean := False;
  Result.Post;

  Result.Append;
  Result.FieldByName('ID').AsInteger := 3;
  Result.FieldByName('Name').AsString := 'Gamma';
  Result.FieldByName('Amount').Clear;
  Result.FieldByName('Score').AsFloat := 1.0;
  Result.FieldByName('Notes').AsString := 'gamma notes';
  Result.FieldByName('Created').AsDateTime := EncodeDate(2026, 3, 1);
  Result.FieldByName('IsActive').AsBoolean := True;
  Result.Post;

  Result.Append;
  Result.FieldByName('ID').AsInteger := 4;
  Result.FieldByName('Name').AsString := 'Alpha';
  Result.FieldByName('Amount').AsCurrency := 50.25;
  Result.FieldByName('Score').AsFloat := 9.9;
  Result.FieldByName('Notes').AsString := 'dup name';
  Result.FieldByName('Created').AsDateTime := EncodeDate(2025, 12, 25);
  Result.FieldByName('IsActive').AsBoolean := True;
  Result.Post;

  Result.Append;
  Result.FieldByName('ID').AsInteger := 5;
  Result.FieldByName('Name').AsString := 'Delta';
  Result.FieldByName('Amount').AsCurrency := 400.00;
  Result.FieldByName('Score').AsFloat := 4.4;
  Result.FieldByName('Notes').AsString := 'unicode <tag> & "quote" ₹ અમદાવાદ';
  Result.FieldByName('Created').AsDateTime := EncodeDate(2026, 4, 20);
  Result.FieldByName('IsActive').AsBoolean := False;
  Result.Post;

  Result.First;
end;

function CreateLargeDataSet(ARowCount: Integer): TClientDataSet;
var
  I: Integer;
begin
  Result := TClientDataSet.Create(nil);
  Result.FieldDefs.Add('ID', ftInteger);
  Result.FieldDefs.Add('Name', ftString, 50);
  Result.CreateDataSet;
  Result.Open;

  for I := 1 to ARowCount do
    Result.AppendRecord([I, Format('Row %d', [I])]);

  Result.First;
end;

function CreateWideDataSet(AFieldCount: Integer): TClientDataSet;
var
  I: Integer;
begin
  Result := TClientDataSet.Create(nil);
  for I := 1 to AFieldCount do
    Result.FieldDefs.Add(Format('Col%.2d', [I]), ftInteger);

  Result.CreateDataSet;
  Result.Open;
  Result.Append;
  for I := 1 to AFieldCount do
    Result.Fields[I - 1].AsInteger := I;
  Result.Post;
  Result.First;
end;

function CreateMatchingColumns(ADataSet: TDataSet): TVittixDBGridColumns;
var
  I: Integer;
  Info: TVittixDBGridColumnInfo;
begin
  Result := TVittixDBGridColumns.Create(nil);
  for I := 0 to ADataSet.Fields.Count - 1 do
  begin
    Info := Result.Add;
    Info.FieldName := ADataSet.Fields[I].FieldName;
  end;
end;

function CreateHeadlessGrid(ADataSet: TDataSet; out AOwnerForm: TForm): TVittixDBGrid;
var
  DataSource: TDataSource;
  I: Integer;
  Column: TColumn;
begin
  AOwnerForm := TForm.CreateNew(nil);
  AOwnerForm.Visible := False;

  DataSource := TDataSource.Create(AOwnerForm);
  DataSource.DataSet := ADataSet;

  Result := TVittixDBGrid.Create(AOwnerForm);
  Result.Parent := AOwnerForm;
  Result.DataSource := DataSource;

  Result.Columns.BeginUpdate;
  try
    Result.Columns.Clear;
    for I := 0 to ADataSet.Fields.Count - 1 do
    begin
      Column := Result.Columns.Add;
      Column.FieldName := ADataSet.Fields[I].FieldName;
      Column.Title.Caption := ADataSet.Fields[I].FieldName;
      Column.Width := 100;
    end;
  finally
    Result.Columns.EndUpdate;
  end;
end;

function CountVisibleRecords(ADataSet: TDataSet): Integer;
begin
  Result := 0;
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    Inc(Result);
    ADataSet.Next;
  end;
end;

end.
