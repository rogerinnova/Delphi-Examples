unit ThreeDUtils;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  System.Math,
  FMX.Dialogs, System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  System.UIConsts,
  FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layers3D,
  FMX.Ani;

type
  TGunsight = class(TObject)
  private
    Top, Bottom, Left, Right: TCone;
    fOwner: TControl3D;
    function NewCone(AAngle: Double): TCone;
  public
    Material: TLightMaterialSource;
    constructor Create(AOwner: TControl3D);
    destructor Destroy; override;
  end;

  TProjectileType = (TracerL, TracerR, Rocket);

  TProjectile = Class(TDummy)
  private
    Material, Explode, Flames: TLightMaterialSource;
    Front, Back, Flame, Flash: TCustomMesh;
    Offset:TDummy;
    FSpeedTimer: TTimer;
    FSpeed, // in units per second
    FFlashGrow: Real;
    FLife, FExplodeLife: Integer;
    FType: TProjectileType;
    FBankAngle: single; // Angle of bank of wing from Camera gimble
    FLengthTailEnd: single; // Length To End Of Tail
    Procedure SpeedTimer(Sender: TObject);
    Procedure SetTracer(ALeftWing: Boolean);
    Procedure SetRocket;
    Procedure ExplodeFlash;
  public
    constructor Create(AOwner: TControl3D; AForm: TForm3D; ABankAngle: single;
      AType: TProjectileType);
    destructor Destroy; override;
  End;

Procedure MoveControlAlongLocalZAxis(AControl:TControl3D; Var APositionPoint: TPoint3D;
  ADistance: Real);
Procedure ScaleObj(AObj: TCustomMesh; AX, Ay, AZ: single);
Procedure GrowPositionByRatio(Var APtRec: TPoint3D; AGrowBy: single);
Procedure GrowObj(AObj: TCustomMesh; AGrowBy: single);
Procedure AlignPos(ADest: TPosition3D; ASource:Tpoint3D);
Procedure Fire(AOwner: TControl3D; AForm: TForm3D; ABankAngle: single;  AType: TProjectileType);
Procedure StartLocationTrackingAnimation(ATarget: TFMXObject;
  Const ALocationProperty: String; AEnd: TPoint3D; ADuration: single);
Procedure StopLocationTrackingAnimation(ATarget: TFMXObject;
  Const ALocationProperty: String);
function ToDegree(AAsRad: single): single;
Function AngleToToOrigin(APos: TPoint3D): TPoint3D;

Const
  AnimationTimerPeriod = 1000; // One Second

implementation

function ToDegree(AAsRad: single): single;
begin
  Result := RadToDeg(AAsRad);
  While Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;

Function AngleToToOrigin(APos: TPoint3D): TPoint3D;
Var
  SinA, CosA: Double;
  CurPoint, CurYPt, CurXpt, CurAngle: TPoint3D;
  Hyp, HypR1: single;

begin
  CurPoint := APos;
  Hyp := CurPoint.Length;
  if CurPoint.Z <= 0 then
  // Align to X axis then dip
  begin
    CurXpt := CurPoint;
    CurXpt.X := 0;
    HypR1 := CurXpt.Length;
    CurAngle.X := ToDegree(ArcSin(CurPoint.Y / HypR1));
    CurAngle.Y := ToDegree(ArcSin(-CurPoint.X / Hyp));
    CurAngle.Z := 0;
  end
  else
  begin
    CurXpt := CurPoint;
    CurXpt.X := 0;
    HypR1 := CurXpt.Length;
    CurAngle.X := ToDegree(Pi - ArcSin(CurPoint.Y / HypR1));
    CurAngle.Y := ToDegree(ArcSin(-CurPoint.X / Hyp));
    CurAngle.Z := 0;
  end;
  Result := CurAngle;
end;

Procedure MoveControlAlongLocalZAxis(AControl:TControl3D; Var APositionPoint: TPoint3D;
  ADistance: Real);
Var
  Offset, OtherSolution: TPoint3D;

begin
  Offset:=  TPoint3D.Create(0,0,ADistance);
  APositionPoint:=AControl.LocalToAbsolute3D(Offset);
end;

Procedure ScaleObj(AObj: TCustomMesh; AX, Ay, AZ: single);
begin
  if AObj = nil then
    Exit;

  AObj.Scale.X := AX;
  AObj.Scale.Y := Ay;
  AObj.Scale.Z := AZ;
end;

Procedure GrowPositionByRatio(Var APtRec: TPoint3D; AGrowBy: single);
Begin
  APtRec.X := APtRec.X * AGrowBy;
  APtRec.Y := APtRec.Y * AGrowBy;
  APtRec.Z := APtRec.Z * AGrowBy;
End;

Procedure GrowObj(AObj: TCustomMesh; AGrowBy: single);
Var
  LPoint: TPoint3D;
begin
  if AObj = nil then
    Exit;
  LPoint := AObj.Scale.point;
  GrowPositionByRatio(LPoint, AGrowBy);
  AObj.Scale.point := LPoint;
end;

Procedure AlignPos(ADest: TPosition3D; ASource:Tpoint3D);
Begin
  ADest.X := ASource.X;
  ADest.Y := ASource.Y;
  ADest.Z := ASource.Z;
End;

Procedure Fire(AOwner: TControl3D; AForm: TForm3D; ABankAngle: single; AType: TProjectileType);
Var
  Obj: TProjectile;
Begin
  Obj := TProjectile.create(AOwner, AForm, ABankAngle, AType);
End;

Procedure StartLocationTrackingAnimation(ATarget: TFMXObject;
  Const ALocationProperty: String; AEnd: TPoint3D; ADuration: single);
Var
  LPopNameX, LPopNameY, LPopNameZ: String;
Begin
  LPopNameX := ALocationProperty + '.X';
  LPopNameY := ALocationProperty + '.Y';
  LPopNameZ := ALocationProperty + '.Z';
  ATarget.AnimateFloat(LPopNameX, AEnd.X, ADuration, TAnimationType.InOut);
  ATarget.AnimateFloat(LPopNameY, AEnd.Y, ADuration, TAnimationType.InOut);
  ATarget.AnimateFloat(LPopNameZ, AEnd.Z, ADuration, TAnimationType.InOut);
End;

Procedure StopLocationTrackingAnimation(ATarget: TFMXObject;
  Const ALocationProperty: String);
Var
  LPopNameX, LPopNameY, LPopNameZ: String;
Begin
  LPopNameX := ALocationProperty + '.X';
  LPopNameY := ALocationProperty + '.Y';
  LPopNameZ := ALocationProperty + '.Z';
  TAnimator.StopPropertyAnimation(ATarget, LPopNameX);
  TAnimator.StopPropertyAnimation(ATarget, LPopNameY);
  TAnimator.StopPropertyAnimation(ATarget, LPopNameZ);
end;

{ Gunsight }

constructor TGunsight.Create(AOwner: TControl3D);
begin
  fOwner := AOwner;
  Material := TLightMaterialSource.create(fOwner);
  Material.Ambient := claNull;
  Material.Diffuse := claNull;
  Material.Emissive := claCrimson;

  Left := NewCone(90);
  Right := NewCone(-90);
  Top := NewCone(180);
  Bottom := NewCone(0);
  // Bottom.RotationAngle.Z:=0;
end;

destructor TGunsight.Destroy;
begin
  Left.Free;
  Right.Free;
  Top.Free;
  Bottom.Free;
  Material.Free;
  inherited;
end;

function TGunsight.NewCone(AAngle: Double): TCone;
Var
  Rads, dx, dy: Double;
begin
  Rads := DegToRad(AAngle);
  SinCos(Rads, dx, dy);
  Result := TCone.create(fOwner);
  Result.Parent := fOwner;
  Result.Scale.X := 0.03;
  Result.Scale.Z := 0.03;
  Result.Scale.Y := 0.08;
  Result.RotationAngle.Z := AAngle;
  Result.Position.Y := 0.1 * dy;
  Result.Position.X := -0.1 * dx;
  // Result.Position.Z:=-0.8;

  Result.MaterialSource := Material;
  Result.Visible := true;
  Result.Projection := TProjection.Camera;
  // Result.ZWrite:=true;
  Result.Repaint;
end;

{ TProjectile }
constructor TProjectile.create(AOwner: TControl3D; AForm: TForm3D;
  ABankAngle: single; AType: TProjectileType);

Var
  MvTo,A: TPoint3D;
  B:TVector3D;
begin
  if AForm = nil then
    raise Exception.create('TProjectile must have a 3D Form');

  if AOwner = nil then
    raise Exception.create('TProjectile must have a Owner Control');
  FType := AType;

  Inherited create(AForm);
  Parent := AForm;
  A:= AOwner.LocalToAbsolute3D(TPoint3D.Create(0,0,0));
  AlignPos(Position,A);
  B:=AOwner.LocalToAbsoluteDirection(TPoint3D.Create(0,0,5));
  //AlignPos(RotationAngle,TPoint3D(B));
  AlignPos(RotationAngle,AOwner.RotationAngle.Point);
  FBankAngle := ABankAngle;
  FSpeedTimer := TTimer.create(nil);
  FSpeedTimer.Interval := AnimationTimerPeriod; // millisecs
  FSpeedTimer.OnTimer := SpeedTimer;
  Material := TLightMaterialSource.create(Self);
  Material.Ambient := claCrimson;
  Material.Diffuse := claCrimson;
  Material.Emissive := claNull;

  case AType of
    TracerL, TracerR:
      SetTracer(AType=TracerL);
    Rocket:
      SetRocket;
  end;

  MvTo := Position.point;
  MoveControlAlongLocalZAxis(Self,MvTo,2 * FLengthTailEnd);
  Position.point := MvTo;
  Visible := true;
  Repaint;
  SpeedTimer(nil);
end;

destructor TProjectile.Destroy;
begin
  try
    Material.Free;
    Explode.Free;
    Flames.Free;
    Front.Free;
    Back.Free;
    Flame.Free;
    Flash.Free;
    FSpeedTimer.Free;
  finally
    inherited;
  end;
end;

procedure TProjectile.ExplodeFlash;
Var
  FlashSzNow, FlashSzThen: TPoint3D;
begin
  if Flash = nil then
    Exit;

  FlashSzNow := Flash.Scale.point;
  FlashSzThen := FlashSzNow;
  GrowPositionByRatio(FlashSzThen, FFlashGrow);
  Flash.Visible := true;
  StartLocationTrackingAnimation(Flash, 'Scale', FlashSzThen,
    FSpeedTimer.Interval / 1000);
end;

procedure TProjectile.SetRocket;
begin
  FLengthTailEnd := 0.5; // Length To End Of Tail
end;

procedure TProjectile.SetTracer(ALeftWing: Boolean);
Var
  SinBank, CosBank: single;
begin
  FreeAndNil(Front);
  FreeAndNil(Flame);
  SinCos(FBankAngle / 180 * Pi, SinBank, CosBank);

  FSpeed := 1;
  FFlashGrow := 2;
  FLife := 10; // x AnimationTimerPeriod = seconds
  FExplodeLife := 3;

  Offset:= TDummy.Create(Self);
  Offset.Parent:=Self;
  Offset.Visible:=true;

  Front := TSphere.create(Self);
  Front.Parent := Offset;
  Front.Position.Z := -1.3;

  if ALeftWing then
  begin
    Offset.Position.X := -0.3 * CosBank;
    Offset.Position.Y := -0.3 * SinBank;
  end
  else
  begin
    Offset.Position.X := +0.3 * CosBank;
    Offset.Position.Y := +0.3 * SinBank;
  end;
  Front.MaterialSource := Material;
  ScaleObj(Front, 0.1, 0.1, 0.9);
  Front.Visible := true;

  if Flames = nil then
  begin
    Flames := TLightMaterialSource.create(Self);
    Flames.Emissive := claChartreuse;
    Flames.Ambient := claChartreuse;
    Flames.Diffuse := claChartreuse;
    Flames.Specular := claWhite;
    Flames.Shininess := 30;
  end;

  Flame := TSphere.create(Self);
  Flame.Parent := Offset;
  Flame.MaterialSource := Flames;
  Flame.Position.Z := -1.9;
  ScaleObj(Flame, 0.05, 0.05, 1);
  Flame.Visible := true;

  Flash := TSphere.create(Self);
  Flash.Parent := Offset;
  Flash.MaterialSource := Flames;
  Flash.Position.Z := 0;
  ScaleObj(Flash, 0.1, 0.1, 0.1);
  Flash.Visible := False;

  FLengthTailEnd := 1.9 + 0.1 * 0.5; // Length To End Of Tail
end;

procedure TProjectile.SpeedTimer(Sender: TObject);
var
  PosNow, PosThen,OffsetThen: TPoint3D;
begin
  Dec(FLife);
  if FLife > 0 then
  Begin
    PosNow := Position.point;
    PosThen := PosNow;
    OffsetThen := Offset.Position.Point;
    OffsetThen.X:=OffsetThen.X-OffsetThen.X/FLife;
    OffsetThen.Y:=OffsetThen.Y-OffsetThen.Y/FLife;
    MoveControlAlongLocalZAxis(Self, PosThen,
      FSpeed * 1000 / AnimationTimerPeriod);

    StartLocationTrackingAnimation(Self, 'Position', PosThen,
      FSpeedTimer.Interval / 1000);
    StartLocationTrackingAnimation(Offset,'Position',OffsetThen,
      FSpeedTimer.Interval / 1000);

  End
  else
  Begin
    Dec(FExplodeLife);
    if FExplodeLife < 1 then
      Visible := False;

    if FExplodeLife < 0 then
      FreeAndNil(Self)
    else
      ExplodeFlash;
  End;
end;

end.
