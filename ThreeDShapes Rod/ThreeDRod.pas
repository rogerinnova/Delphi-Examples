unit ThreeDRod;
{ usage
  The TThreeDRod is a derivative of TCustomMesh the data mesh is constructed of
  straight or bent sections of rod.
  The sections used are specified by a list of strings with each line
  representing a segment. By editing ConstructionText and following simple
  rules it is possible to build complex rods.


  A straight section requires radius, length, and angle
  Model       Flag=s, Rod Radius, Section Length,  Angle in Degrees
  Example     'S,0.2,0.1,90.0'

  To get a contiguous rod the angle and radius must match the final angle of
  the previous segment. Each segment commences from where the last one ends.
  Straight sections that either start or finish a sequence will have that end
  filled in.


  A Bend Section requires rod radius, bend radius, bend angle
  Model       Flag=B, Rod Radius, Bend Radius (To Centre) , Bend Angle in Degrees
  Example     'B,0.2,0.5,-90.0'

  A bend aligns itself to the angle of the previous section and applies the angle
  from that reference.


  Outstanding  Issues
  The current code does slightly rotate the ends from the YZ plane. I assume it
  is because the MovePath method is too simplistic but I do not yet understand why.
  For my purposes it is acceptable but it becomes problematic as the rod radius and
  the offset increases.

}
{$IFDEF VER270}
  {$DEFINE ISXE6_DELPHI}
{$ENDIF}

{$IFDEF VER280}
  {$DEFINE ISXE6_DELPHI}
  {$DEFINE ISXE7_DELPHI}
{$ENDIF}

{$IFDEF VER290}
  {$DEFINE ISXE6_DELPHI}
  {$DEFINE ISXE7_DELPHI}
  {$DEFINE ISXE8_DELPHI}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE ISXE6_DELPHI}
  {$DEFINE ISXE7_DELPHI}
  {$DEFINE ISXE8_DELPHI}
  {$DEFINE ISD10S_DELPHI}
{$ENDIF}

{$IFDEF VER310}
  {$DEFINE ISXE6_DELPHI}
  {$DEFINE ISXE7_DELPHI}
  {$DEFINE ISXE8_DELPHI}
  {$DEFINE ISD10S_DELPHI}
  {$DEFINE ISD101B_DELPHI}
{$ENDIF}


interface

uses SysUtils, FMX.Objects3D, FMX.Types3D, Classes, System.Math, System.Types,
  FMX.Types , FMX.Utils , ISLibFmxUtils, System.Math.Vectors;

Type

  TRodSectionBase = Class(TObject)
  Private
    FPrev, FNext: TRodSectionBase;
    FStartLocal, FEndLocal: TPoint3D;
    FStartAngle, FEndAngle: Single;
    FRadius: Single;
  Protected
    VBuffer: TVertexBuffer;
    IBuffer: TIndexBuffer;
    Procedure SetFltValueFrmArrayString(Var AFloat: Single;
      Var AString: AnsiString);
    Function RadiusSections: Integer;
    Procedure AddSq(ASq: TSqArray; Var AVOffset: Integer;
      Var AIOffset: Integer);
    Procedure AddCylinder(AR1, AX1, AY1, AZ1, AAlpha1, AR2, AX2, AY2, AZ2,
      AAlpha2: Real; Var AVOffset: Integer; Var AIOffset: Integer;
      AStartFill: Boolean = false; AEndFill: Boolean = false);
    Procedure BuildCylinder(RadiusArray1, RadiusArray2: TPathArray;
      Var AVOffset: Integer; Var AIOffset: Integer);
    Procedure BuildCylinderEnd(RadiusArray: TPathArray; AReverse: Boolean;
      Var AVOffset: Integer; Var AIOffset: Integer);
  Public
    Class Function CreateFrmTxt(AText: AnsiString): TRodSectionBase;
    Destructor Destroy; override;
    Function DeltaX: Real; virtual; abstract;
    Function DeltaY: Real; virtual; abstract;
    Function NoOfVertexes: Integer; virtual;
    Function NoOfIndexes: Integer; virtual;
    Procedure AddSection(ANewSection: TRodSectionBase);
    Procedure AddData(Var NxtData, NxtIndex: Integer); virtual;
    Property Radius: Single read FRadius write FRadius;
  End;

  TRodStraight = Class(TRodSectionBase)
  Private
    FLength, FAngleX: Single;
    FStartFill, FEndFill: Boolean;
  Public
    Constructor Create;
    Constructor CreateFrmTxt(AText: AnsiString);
    Function DeltaX: Real; override;
    Function DeltaY: Real; override;
    Function NoOfVertexes: Integer; override;
    Function NoOfIndexes: Integer; override;
    Procedure AddData(Var NxtData, NxtIndex: Integer); override;
    Property AngleX: Single read FAngleX write FAngleX;
    Property Length: Single read FLength write FLength;
  End;

  TRodBend = Class(TRodSectionBase)
  Private
    FBendRadius, FAngleRotateX: Single;
    FSegments10Deg: Integer;
    Function GetDeltaXY(AStartAngle, ADeltaAngle: Single): TPointF;
  Public
    Constructor CreateFrmTxt(AText: AnsiString);
    Function DeltaX: Real; override;
    Function DeltaY: Real; override;
    Function NoOfVertexes: Integer; override;
    Function NoOfIndexes: Integer; override;
    Procedure AddData(Var NxtData, NxtIndex: Integer); override;
  End;

  TThreeDRod = Class(TCustomMesh)
  Private
    FSections: TRodSectionBase;
    FConstructionText: TStrings;
    procedure SetConstructionCode(const Value: TStrings);
  protected
    procedure ReadState(Reader: TReader); override;
  Public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure ResetSections;
    Procedure AddSection(ANewSection: TRodSectionBase);
    Procedure AddSectionFrmText(AText: String);
    Procedure RebuildMesh;
  Published
    property MaterialSource;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    //property ShowContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
    Property ConstructionText: TStrings Read FConstructionText
      write SetConstructionCode;
  End;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Innova Solutions', [TThreeDRod]);
end;

const
  QuadPoints = 8;

function GetTokenXE3(var S: AnsiString; Separators: AnsiString; Stop: AnsiString = ''): AnsiString;
var
  I, len: Integer;
  CopyS: AnsiString;
begin
  Result := '';
  CopyS := S;
  len := Length(CopyS);
  for I := 1 to len do
  begin
    if Pos(CopyS[I], Stop) > 0 then
      Break;
    Delete(S, 1, 1);
    if Pos(CopyS[I], Separators) > 0 then
    begin
      Result := Result;
      Break;
    end;
    Result := Result + CopyS[I];
  end;
  Result := Trim(Result);
  S := Trim(S);
end;


  { TThreeDRod }

procedure TThreeDRod.AddSection(ANewSection: TRodSectionBase);
begin
  if ANewSection = nil then
    exit;

  ANewSection.VBuffer := Data.VertexBuffer;
  ANewSection.IBuffer := Data.IndexBuffer;
  if FSections = nil then
    FSections := ANewSection
  else
    FSections.AddSection(ANewSection);
end;

procedure TThreeDRod.AddSectionFrmText(AText: String);
Var
  NewSection: TRodSectionBase;
begin
  if AText = '' then
    exit;
  NewSection := TRodSectionBase.CreateFrmTxt(AText);
  AddSection(NewSection);
end;

constructor TThreeDRod.Create(AOwner: TComponent);
begin
  inherited;
{$IfDEF ISXE8_DELPHI}
  WrapMode:=TMeshWrapMode.Original;
{$ENDIF}
  FConstructionText := TStringList.Create;
  // FConstructionText.Add('S,0.2,0.1,0.0');
  FConstructionText.Add('B,0.2,0.5,340.0');
  // FConstructionText.Add('S,0.2,0.3,-90.0');
  RebuildMesh;
end;

destructor TThreeDRod.Destroy;
begin
  FSections.Free;
  FConstructionText.Free;
  inherited;
end;

procedure TThreeDRod.ReadState(Reader: TReader);
begin
  inherited;
  FreeAndNil(FSections);
  RebuildMesh;
end;

procedure TThreeDRod.RebuildMesh;
var
  NxtData, NxtIndex: Integer;
  I: Integer;
begin
  if FSections = nil then
  Begin
    if FConstructionText.Count > 0 then
      for I := 0 to FConstructionText.Count - 1 do
        AddSectionFrmText(FConstructionText[I]);
    if FSections = nil then
      exit;
  End;

  { MeshRender uses D3DPT_TRIANGLELIST
    From http://www.directxtutorial.com/tutorial9/b-direct3dbasics/dx9B4.aspx#still
    [Table 4.3 - D3DPRIMITIVETYPE Values]
    Value 	Description
    D3DPT_POINTLIST 	Shows a series of points.
    D3DPT_LINELIST 	Shows a series of separated lines.
    D3DPT_LINESTRIP 	Shows a series of connected lines.
    D3DPT_TRIANGLELIST 	Shows a series of separated triangles.
    D3DPT_TRIANGLESTRIP 	Shows a series of connected triangles.
    D3DPT_TRIANGLEFAN 	Shows a series of triangles with one shared corner.
  }

  NxtData := 0;
  NxtIndex := 0;
  Data.VertexBuffer.Length := FSections.NoOfVertexes;
  Data.IndexBuffer.Length := FSections.NoOfIndexes;
  FSections.AddData(NxtData, NxtIndex);


  //XE2 Data.CalcNormals;
    Data.CalcFaceNormals;
  //Data.CalcSmoothNormals;
  //Data.CalcTangentBinormals;

end;

procedure TThreeDRod.ResetSections;
begin
  FreeAndNil(FSections);
end;

procedure TThreeDRod.SetConstructionCode(const Value: TStrings);
begin
  FConstructionText.Assign(Value);
  if FConstructionText.Count > 0 then
  begin
    FreeAndNil(FSections);
    RebuildMesh;
  end;
end;

{ TRodStraight }

procedure TRodStraight.AddData(Var NxtData, NxtIndex: Integer);

begin
  FEndLocal := MovePoint(FStartLocal, DeltaX, DeltaY, 0.0);
  FStartAngle := FAngleX;
  FEndAngle := FAngleX;
  AddCylinder(FRadius, FStartLocal.X, FStartLocal.Y, FStartLocal.Z, FAngleX,
    FRadius, FEndLocal.X, FEndLocal.Y, FEndLocal.Z, FAngleX, NxtData, NxtIndex,
    FStartFill, FEndFill);
  inherited;
end;

constructor TRodStraight.Create;
begin
  inherited;
  FLength := 1;
  FRadius := 1;
end;

constructor TRodStraight.CreateFrmTxt(AText: AnsiString);
Var
  Flag: AnsiString;
  AngleXAsDegrees: Single;
begin
  inherited Create;
  Flag := GetTokenXE3(AText, ',()');
  if uppercase(Flag) <> 'S' then
    raise Exception.Create('TRodStraight.CreateFrmTxt::' + AText);
  // Model       Flag,Rod Radius, Section Length, Angle in Degrees
  // Example     'S,0.2,0.1,0.0'
  AngleXAsDegrees := 0.0;
  SetFltValueFrmArrayString(FRadius, AText);
  SetFltValueFrmArrayString(FLength, AText);
  SetFltValueFrmArrayString(AngleXAsDegrees, AText);
  if AngleXAsDegrees <> 0.0 then
    FAngleX := AngleXAsDegrees / 180 * Pi;
end;

function TRodStraight.DeltaX: Real;
begin
  Result := FLength * Cos(FAngleX);
end;

function TRodStraight.DeltaY: Real;
begin
  Result := FLength * Sin(FAngleX);
end;

function TRodStraight.NoOfIndexes: Integer;
begin
  FEndFill := FNext = nil;
  FStartFill := FPrev = nil;
  // 6 Lines Indexs per Square
  // X Squares per Length
  Result := RadiusSections * 6 + Inherited;
  if FEndFill then
    Result := Result + QuadPoints * 4 * 3;
  if FStartFill then
    Result := Result + QuadPoints * 4 * 3;
end;

function TRodStraight.NoOfVertexes: Integer;
begin
  FEndFill := FNext = nil;
  FStartFill := FPrev = nil;
  // 4 Vertexes per Square
  // X Squares per Length
  Result := RadiusSections * 4 + Inherited;
  if FEndFill then
    Result := Result + 1 + QuadPoints * 4 * 2;
  if FStartFill then
    Result := Result + 1 + QuadPoints * 4 * 2;
end;

{ TRodSectionBase }

procedure TRodSectionBase.AddCylinder(AR1, AX1, AY1, AZ1, AAlpha1, AR2, AX2,
  AY2, AZ2, AAlpha2: Real; var AVOffset, AIOffset: Integer;
  AStartFill: Boolean = false; AEndFill: Boolean = false);
Var
  RadiusArray1: TPathArray;
  RadiusArray2: TPathArray;
begin
  RadiusArray1 := RadiusArray(AR1, AX1, AY1, AZ1, AAlpha1, QuadPoints);
  RadiusArray2 := RadiusArray(AR2, AX2, AY2, AZ2, AAlpha2, QuadPoints);
  If AStartFill then
    BuildCylinderEnd(RadiusArray1, false, AVOffset, AIOffset);
  BuildCylinder(RadiusArray1, RadiusArray2, AVOffset, AIOffset);
  If AEndFill then
    BuildCylinderEnd(RadiusArray2, True, AVOffset, AIOffset);
end;

procedure TRodSectionBase.AddData(var NxtData, NxtIndex: Integer);
begin
  if FNext <> nil then
  begin
    FNext.FStartLocal := FEndLocal;
    FNext.FStartAngle := FEndAngle;
    FNext.AddData(NxtData, NxtIndex);
  end;
end;

procedure TRodSectionBase.AddSection(ANewSection: TRodSectionBase);
begin
  if FNext <> nil then
    FNext.AddSection(ANewSection)
  else
  begin
    FNext := ANewSection;
    ANewSection.FPrev := self; // no need for pop???
  end;
end;

procedure TRodSectionBase.AddSq(ASq: TSqArray; var AVOffset, AIOffset: Integer);
begin
  begin
    VBuffer.Vertices[AVOffset] := ASq[0];
    VBuffer.Vertices[AVOffset + 1] := ASq[1];;
    VBuffer.Vertices[AVOffset + 2] := ASq[2];;
    VBuffer.Vertices[AVOffset + 3] := ASq[3];;
    IBuffer.Indices[AIOffset] := AVOffset + 3;
    IBuffer.Indices[AIOffset + 1] := AVOffset + 2;
    IBuffer.Indices[AIOffset + 2] := AVOffset + 1;
    IBuffer.Indices[AIOffset + 3] := AVOffset + 1;
    IBuffer.Indices[AIOffset + 4] := AVOffset + 0;
    IBuffer.Indices[AIOffset + 5] := AVOffset + 3;
    Inc(AVOffset, 4);
    Inc(AIOffset, 6);
  end;
end;

procedure TRodSectionBase.BuildCylinder(RadiusArray1, RadiusArray2: TPathArray;
  var AVOffset, AIOffset: Integer);
Var
  I, sz: Integer;
begin
  sz := high(RadiusArray1);
  if high(RadiusArray2) <> sz then
    raise Exception.Create('BuildCylinder');

  I := 0;
  while I < sz do
  begin
    AddSq(SqArray(RadiusArray1[I + 1], RadiusArray1[I], RadiusArray2[I],
      RadiusArray2[I + 1]), AVOffset, AIOffset);
    Inc(I);
  end;
  AddSq(SqArray(RadiusArray1[0], RadiusArray1[sz], RadiusArray2[sz],
    RadiusArray2[0]), AVOffset, AIOffset);
end;

procedure TRodSectionBase.BuildCylinderEnd(RadiusArray: TPathArray;
  AReverse: Boolean; var AVOffset, AIOffset: Integer);
Var
  I, sz, CtrOffset: Integer;
  Cntr: TPoint3D;

begin
  sz := high(RadiusArray);
  if sz < 4 then
    raise Exception.Create('BuildCylinderEnd');
  Cntr := Average3D(RadiusArray[0], RadiusArray[sz div 2]);
  VBuffer.Vertices[AVOffset] := Cntr;
  CtrOffset := AVOffset;
  //VBuffer.Diffuse[CtrOffset] := claYellow;
  Inc(AVOffset);

  I := 0;
  while I <= sz do
  begin
    VBuffer.Vertices[AVOffset] := RadiusArray[I];
    if I = sz then
      VBuffer.Vertices[AVOffset + 1] := RadiusArray[0]
    else
      VBuffer.Vertices[AVOffset + 1] := RadiusArray[I + 1];
    //VBuffer.Diffuse[AVOffset] := claBlue;
    //VBuffer.Diffuse[AVOffset + 1] := claRed;
    if AReverse then
    begin
      IBuffer.Indices[AIOffset] := AVOffset + 1;
      IBuffer.Indices[AIOffset + 1] := CtrOffset;
      IBuffer.Indices[AIOffset + 2] := AVOffset + 0;
    end
    else
    begin
      IBuffer.Indices[AIOffset] := AVOffset + 0;
      IBuffer.Indices[AIOffset + 1] := CtrOffset;
      IBuffer.Indices[AIOffset + 2] := AVOffset + 1;
    end;
    Inc(AVOffset, 2);
    Inc(AIOffset, 3);
    Inc(I);
  end;
end;

class function TRodSectionBase.CreateFrmTxt(AText: AnsiString): TRodSectionBase;
begin
  Result := nil;
  if Length(AText) < 5 then
    exit;

  case AText[1] of
    'b', 'B':
      begin
        Result := TRodBend.CreateFrmTxt(AText);
      end;
    's', 'S':
      begin
        Result := TRodStraight.CreateFrmTxt(AText);
      end;
  end;
end;

destructor TRodSectionBase.Destroy;
begin
  FNext.Free;
  inherited;
end;

function TRodSectionBase.NoOfIndexes: Integer;
begin
  if FNext = nil then
    Result := 0
  Else
    Result := FNext.NoOfIndexes;
end;

function TRodSectionBase.NoOfVertexes: Integer;
begin
  if FNext = nil then
    Result := 0
  Else
    Result := FNext.NoOfVertexes;
end;

function TRodSectionBase.RadiusSections: Integer;
begin
  Result := QuadPoints * 4;
end;

procedure TRodSectionBase.SetFltValueFrmArrayString(var AFloat: Single;
  var AString: AnsiString);
Var
  Val: Single;
begin
  Try
    Val := StrToFloat(GetTokenXE3(AString, ',()'), USFormatSettings);
  Except
    Val := 0.0;
  end;
  if Val <> 0.0 then
    AFloat := Val;
end;

{ TRodBend }

procedure TRodBend.AddData(var NxtData, NxtIndex: Integer);
Var
  DeltaAlpha, LastAlpha, NxtAlpha: Single;
  NxtLocal, PrvLocal: TPoint3D;
  Delta: TPointF;
  I: Integer;
begin
  FEndLocal := MovePoint(FStartLocal, DeltaX, DeltaY, 0.0);
  FEndAngle := FStartAngle + FAngleRotateX;
  if FSegments10Deg < 1 then
    FSegments10Deg := 1;
  DeltaAlpha := FAngleRotateX / FSegments10Deg;
  LastAlpha := FStartAngle;
  I := 1;
  PrvLocal := FStartLocal;
  While I < FSegments10Deg do
  Begin
    NxtAlpha := LastAlpha + DeltaAlpha;
    Delta := GetDeltaXY(LastAlpha, DeltaAlpha);
    NxtLocal.X := PrvLocal.X + Delta.X;
    NxtLocal.Y := PrvLocal.Y + Delta.Y;
    NxtLocal.Z := PrvLocal.Z;
    AddCylinder(FRadius, PrvLocal.X, PrvLocal.Y, PrvLocal.Z, LastAlpha, FRadius,
      NxtLocal.X, NxtLocal.Y, NxtLocal.Z, NxtAlpha, NxtData, NxtIndex,
      false, false);
    LastAlpha := NxtAlpha;
    PrvLocal := NxtLocal;
    Inc(I);
  End;
  AddCylinder(FRadius, PrvLocal.X, PrvLocal.Y, PrvLocal.Z, LastAlpha, FRadius,
    FEndLocal.X, FEndLocal.Y, FEndLocal.Z, FEndAngle, NxtData, NxtIndex,
    false, false);
  inherited;
end;

constructor TRodBend.CreateFrmTxt(AText: AnsiString);
Var
  Flag: AnsiString;
  AngleXAsDegrees: Single;
begin
  inherited Create;
  Flag := GetTokenXE3(AText, ',()');
  if uppercase(Flag) <> 'B' then
    raise Exception.Create('TRodBend.CreateFrmTxt::' + AText);

  // Model       Flag, Rod Radius, Bend Radius (To Center) , Bend Angle in Degrees
  // Example     'B,0.2,0.5,-90.0'
  SetFltValueFrmArrayString(FRadius, AText);
  SetFltValueFrmArrayString(FBendRadius, AText);
  SetFltValueFrmArrayString(AngleXAsDegrees, AText);
  if AngleXAsDegrees <> 0.0 then
    FAngleRotateX := AngleXAsDegrees / 180 * Pi;
end;

function TRodBend.DeltaX: Real;
begin
  Result := GetDeltaXY(FStartAngle, FAngleRotateX).X;
end;

function TRodBend.DeltaY: Real;
begin
  Result := GetDeltaXY(FStartAngle, FAngleRotateX).Y;
end;

function TRodBend.GetDeltaXY(AStartAngle, ADeltaAngle: Single): TPointF;
Var
  CordLen: Single;
  HalfAngle, BaseAngle: Single;
begin
  HalfAngle := ADeltaAngle / 2;
  CordLen := FBendRadius * 2 * Sin(HalfAngle);
  BaseAngle := AStartAngle + HalfAngle;
  Result.X := CordLen * Cos(BaseAngle);
  Result.Y := CordLen * Sin(BaseAngle);
  if ADeltaAngle < 0.0 then
  Begin
    Result.X := -Result.X;
    Result.Y := -Result.Y;
  End;

end;

function TRodBend.NoOfIndexes: Integer;
begin
  // 6 Lines Indexs per Square
  // FSegments10Deg Squares per Length
  FSegments10Deg := Abs(Round(FAngleRotateX / (Pi / 18)));

  Result := RadiusSections * 6 * FSegments10Deg + Inherited;
  Result := Result;
end;

function TRodBend.NoOfVertexes: Integer;
begin
  // 4 Vertexes per Square
  // FSegments10Deg Squares per Length
  FSegments10Deg := Abs(Round(FAngleRotateX / (Pi / 18)));
  Result := RadiusSections * 4 * FSegments10Deg + Inherited;
  Result := Result;
end;

end.
