unit FlyingCamera;
// function AssignFlyingCameraToForm(A3dForm: TForm3D; AX,AY,AZ:Single): TFlyingCamera;
//  Adds a Camera at AX,AY,AZ and points it to the point 0,0,0
// Sets Form Camera Property
// Sets Forms UseDesign Camera  to false
// Allocates the OnKeyDown Event (if Unassigned)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layers3D,
  FMX.Ani, ThreeDUtils;

Type
  TFlyingCamera = Class(TDummy)
  private
    F3dForm: TForm3D;
    FForwardView: TCamera;
    FGunsight: TGunsight;
    FArtHorizon: TCube;
    FAHMaterial: TLightMaterialSource;
    FHeadsUp: TDummy;
    FLocText, FSpeedText, FAngleText: TText3D;
    FTextMaterial: TLightMaterialSource;
    FSpeedTimer: TTimer;
    FAngleInc: Real; // In degress
    FSpeed, // in units per second
    FSpeedInc: Real;
    FStepFormat:Boolean;
    FBankAngle:Single;
    Procedure SetBankAngle( Value:Single);
    Procedure MoveForward(ADistance: Real);
    procedure FmCamera3DKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState); Virtual;
    Procedure SpeedTimer(Sender: TObject);
    Procedure SetText(ATxt: TText3D);
    Procedure ShowIt(AObj: TControl3D);
    procedure SetSpeed(const Value: Real);
    Procedure AlignCammeraToOrigin;
    Procedure Bank(ALeft:Boolean);
    Procedure CreateArtHorizon;
    procedure SetStepFormat(const Value: Boolean);
    Property  BankAngle:Single Read FBankAngle write SetBankAngle;
  Public
    Constructor Create(A3dForm: TComponent); override;
    destructor Destroy; override;
    Procedure UpdateHeadUpDisplay;
    Property Speed: Real read FSpeed write SetSpeed; // units per second
    Property StepFormat: Boolean read FStepFormat write SetStepFormat; // units per second
  End;

function AssignFlyingCameraToForm(A3dForm: TForm3D; AX:Single=0;AY:Single=0;AZ:Single=0): TFlyingCamera;

implementation
const
  cRateOfTurn = 0.01; //Turn Degrees per Bank Degree per Second

Var
  CameraStartPos:TPoint3D;

function AssignFlyingCameraToForm(A3dForm: TForm3D; AX,AY,AZ:Single): TFlyingCamera;
begin
  if (AX<>0) or (AY<>0) or (AZ<>0) then
    CameraStartPos:=TPoint3D.Create(Ax,Ay,Az);
  Result := TFlyingCamera.Create(A3dForm);
end;

{ TFlyingCamera }
procedure TFlyingCamera.AlignCammeraToOrigin;
begin
   RotationAngle.Point:=AngleToToOrigin(Position.Point);
   UpdateHeadUpDisplay;
end;

procedure TFlyingCamera.Bank(ALeft: Boolean);
begin
 if FStepFormat then
  case ALeft of
    True:RotationAngle.Y:=RotationAngle.Y - FAngleInc;
    False:RotationAngle.Y:=RotationAngle.Y + FAngleInc;
  end
  Else
  case ALeft of
    True: BankAngle:=FBankAngle + 5;
    False: BankAngle:=FBankAngle - 5;
  end
end;

constructor TFlyingCamera.Create(A3dForm: TComponent);

begin
  if not(A3dForm is TForm3D) then
    raise Exception.Create('Must have a 3D Form');
  F3dForm := A3dForm as TForm3D;
  Inherited Create(A3dForm);
  FAngleInc := 1.0;
  FSpeedInc := 0.01;
  FSpeed := 0;//0.1;

  Parent := A3dForm as TFmxObject;
  if CameraStartPos<>TPoint3D.Zero then
   Position.Point:= CameraStartPos
  Else
   Position.Point:=TPoint3D.Create(0,-20*Sin(DegToRad(20)),-20*Cos(DegToRad(20)));

  F3dForm.OnKeyDown := FmCamera3DKeyDown;

  FSpeedTimer := TTimer.Create(Self);
  FSpeedTimer.Interval := 100; // 1000 millisecs = 1 Second
  FSpeedTimer.OnTimer := SpeedTimer;

  FForwardView := TCamera.Create(Self);
  FForwardView.Parent := Self;
  FForwardView.Position.Z:=0;
  FHeadsUp :=TDummy.Create(self);
  FHeadsUp.Parent:=FForwardView;
  //Gimble the TCamera RotationAngle.Z Leave The Base Dummy RotationAngle.Z:=0
  FHeadsUp.Position.point:=TPoint3D.Create(0,0,1.2);
  FHeadsUp.Scale.Point:= TPoint3D.Create(0.4,0.4,0.4);
  FHeadsUp.Visible:=True;
  FGunsight := TGunsight.Create(FHeadsUp);

  FLocText := TText3D.Create(FHeadsUp);
  FLocText.Position.X := -1.2;
  FLocText.Position.Y := 1.1;
  SetText(FLocText);

  FSpeedText := TText3D.Create(FHeadsUp);
  FSpeedText.Position.X := 1.2;
  FSpeedText.Position.Y := 1.1;
  FSpeedText.Parent:=FHeadsUp;
  SetText(FSpeedText);

  FAngleText := TText3D.Create(FHeadsUp);
  FAngleText.Position.X := -1.2;
  FAngleText.Position.Y := 1.2;
  SetText(FAngleText);

  F3dForm.Camera := FForwardView;
  F3dForm.UsingDesignCamera := false;
  AlignCammeraToOrigin;
  Visible := true;
  Repaint;
end;

procedure TFlyingCamera.CreateArtHorizon;
begin
  if FArtHorizon<>nil then Exit;

  FArtHorizon:=TCube.Create(Self);
  FArtHorizon.Parent:=self;

  FArtHorizon.Scale.Point:=TPoint3D.Create(0.2,0.003,0.05);
  FArtHorizon.Position.Point:= TPoint3D.Create(0.0,0.04,FHeadsUp.Position.Point.Z+0.005);
  FAHMaterial:= TLightMaterialSource.Create(Self);
  FAHMaterial.Ambient := claNull;
  FAHMaterial.Diffuse := claNull;
  FAHMaterial.Emissive := claYellow;
  FArtHorizon.MaterialSource:=FAHMaterial;
  FArtHorizon.Visible:=true;
end;

destructor TFlyingCamera.Destroy;
begin
  // Freed BY SELF FreeandNil(FSpeedTimer);
  FreeandNil(FGunsight);
  inherited;
end;

procedure TFlyingCamera.FmCamera3DKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    0:
      begin
        case KeyChar of
          'W', 'w':
            MoveForward(0.3);
          'S', 's':
            MoveForward(-0.3);
          '+':
            FSpeed := FSpeed + FSpeedInc;
           '-':
            FSpeed := FSpeed - FSpeedInc;
          ' ':
            Begin
            Fire(Self, F3dForm, FBankAngle, TracerL);
            Fire(Self, F3dForm, FBankAngle, TracerR);
            End;
        end;
      end;
    vkLeft:
      Bank(True);
    vkUp:
      RotationAngle.X := (RotationAngle.X + FAngleInc);
    vkRight:
      Bank(False);
    vkDown:
      RotationAngle.X := (RotationAngle.X - FAngleInc);
    vkNext:
      FAngleInc := FAngleInc * 2;
    vkPrior:
      FAngleInc := FAngleInc / 2;
    vkInsert:
      FSpeedInc := FSpeedInc * 2;
    vkDelete:
      FSpeedInc := FSpeedInc / 2;
    vkAdd:
      FSpeed := FSpeed + FSpeedInc;
    vkSubtract:
      FSpeed := FSpeed - FSpeedInc;
    vkSpace:
            Begin
            Fire(Self, F3dForm, FBankAngle, TracerL);
            Fire(Self, F3dForm, FBankAngle, TracerR);
            End;
  end;
  UpdateHeadUpDisplay
end;

procedure TFlyingCamera.MoveForward(ADistance: Real);

Var
  LPositionPoint: TPoint3D;
begin
  LPositionPoint := Position.Point;
  MoveControlAlongLocalZAxis(Self, LPositionPoint, ADistance);
  StartLocationTrackingAnimation(Self, 'Position', LPositionPoint,
    FSpeedTimer.Interval / 1000);
  UpdateHeadUpDisplay;
end;

procedure TFlyingCamera.SetBankAngle(Value: Single);
begin
  FBankAngle:= Value;
    While FBankAngle < 180 do
    FBankAngle := FBankAngle + 360;
  while FBankAngle > 180 do
    FBankAngle := FBankAngle - 360;
  FForwardView.RotationAngle.Z:= FBankAngle;
end;

procedure TFlyingCamera.SetSpeed(const Value: Real);
begin
  if Not FStepFormat then
     if FSpeed<0 then
        raise Exception.Create('Cannot Reverse in Gimble Mode');
  FSpeed := Value;
  if (FSpeed < 0.00001)and(FSpeed > -0.00001)  then
    Begin
      FSpeedTimer.Enabled:=false;
      StopLocationTrackingAnimation(Self,'Position');
    End
    Else
      FSpeedTimer.Enabled:=True;
end;


procedure TFlyingCamera.SetStepFormat(const Value: Boolean);
begin
  FStepFormat := Value;
  UpdateHeadUpDisplay;
end;

procedure TFlyingCamera.SetText(ATxt: TText3D);
begin
  if FTextMaterial = nil then
  begin
    FTextMaterial := TLightMaterialSource.Create(Self);
    FTextMaterial.Emissive := claChartreuse;
    FTextMaterial.Ambient := claChartreuse;
    FTextMaterial.Diffuse := claChartreuse;
    FTextMaterial.Specular := claWhite;
    FTextMaterial.Shininess := 30;
  end;
  ATxt.MaterialBackSource := FTextMaterial;
  ATxt.MaterialShaftSource := FTextMaterial;
  ATxt.MaterialSource := FTextMaterial;
  ATxt.WordWrap := false;
  ATxt.Scale.X := 0.2;
  ATxt.Scale.Y := 0.03;
  ATxt.Scale.Z := 0.02;
  ATxt.Text := 'AAAAAAAA';
  ATxt.WrapMode := TMeshWrapMode.Resize;
  ATxt.Width := 4;
  ShowIt(ATxt);

end;

procedure TFlyingCamera.ShowIt(AObj: TControl3D);
begin
  AObj.Parent := AObj.Owner as TFmxObject;
  AObj.Visible := true;
  AObj.Repaint;
end;

procedure TFlyingCamera.SpeedTimer(Sender: TObject);
begin
  if FSpeedTimer.Interval > 0 then
    MoveForward(FSpeed * FSpeedTimer.Interval / 1000);

  if FStepFormat then
   FSpeedTimer.Interval:=1000 // 1000 millisecs = 1 Second
  else
   FSpeedTimer.Interval:=100; // 100 millisecs = 0.1 Second
  if not FStepFormat and (FSpeed >0) then
   if (FBankAngle>4)or(FBankAngle<4) then
     RotationAngle.Y:= RotationAngle.Y - FBankAngle * cRateOfTurn
         * FSpeedTimer.Interval / 1000;
end;

procedure TFlyingCamera.UpdateHeadUpDisplay;
begin
  if FSpeedText=nil then Exit;
  if FLocText=nil then Exit;
  if FAngleText=nil then Exit;

  FSpeedText.Text := 'Speed::' + FormatFloat('0.000', FSpeed);
  FLocText.Text := 'X::' + FormatFloat('0.000', Position.X) + ' Y::' +
    FormatFloat('0.000', Position.Y) + '  Z::' + FormatFloat('0.000',
    Position.Z);
  FAngleText.Text := 'Ax::' + FormatFloat('##0.0', RotationAngle.X) + ' Ay::' +
    FormatFloat('##0.0', RotationAngle.Y) + '  Bank::' +
    FormatFloat('##0.0', FBankAngle);

  if (FArtHorizon=nil) and not FStepFormat then
     CreateArtHorizon;

  if FArtHorizon<>nil then
       FArtHorizon.Visible:=not FStepFormat;
end;

end.
