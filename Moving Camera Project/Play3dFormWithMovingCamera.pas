unit Play3dFormWithMovingCamera;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layers3D,
  FMX.Ani, FlyingCamera;
//Add Camera
//Set camera Property
//Unset UseDesign Cammera

type
  TForm1 = class(TForm3D)
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    Sphere1: TSphere;
    Timer1: TTimer;
    Cone1: TCone;
    LightMaterialSource2: TLightMaterialSource;
    Cube2: TCube;
    Cylinder1: TCylinder;
    Disk1: TDisk;
    RoundCube1: TRoundCube;
    Cube1: TCube;
    Cube3: TCube;
    Cube4: TCube;
    Cube5: TCube;
    Sphere2: TSphere;
    procedure Form3DCreate(Sender: TObject);
  private
    { Private declarations }
    FlyCam:TFlyingCamera;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses ThreeDUtils;

procedure TForm1.Form3DCreate(Sender: TObject);
begin
 FlyCam:=AssignFlyingCameraToForm(Self,5,26,-29);
 //FlyCam:=AssignFlyingCameraToForm(Self,-5,26,20);
 FlyCam.StepFormat:=true;
 //FlyCam.Speed:=0.1;
end;

end.
