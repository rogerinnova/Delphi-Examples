unit BitMap3DForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, UsefulBmpConcepts, System.Math.Vectors, FMX.Controls3D,
  FMX.MaterialSources, FMX.Objects3D, FMX.Menus, FMX.Ani;

type
  TDemoForm3DShapesWithTexture = class(TForm3D)
    Cube1: TCube;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    Light2: TLight;
    Cylinder1: TCylinder;
    LightMaterialSource2: TLightMaterialSource;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    Cone1: TCone;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    FloatAnimation5: TFloatAnimation;
    FloatAnimation6: TFloatAnimation;
    Sphere1: TSphere;
    FloatAnimation7: TFloatAnimation;
    FloatAnimation8: TFloatAnimation;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DemoForm3DShapesWithTexture: TDemoForm3DShapesWithTexture;

implementation

{$R *.fmx}

procedure TDemoForm3DShapesWithTexture.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  LightMaterialSource1.Texture:=ColorTransparentBitMap(360,360);
  LightMaterialSource2.Texture:=ColorTransparentBitMap(360,360);
//  Cube1.MaterialSource:= LightMaterialSource1;
  cube1.Repaint;
  Cone1.Repaint;
  Cylinder1.Repaint;
end;

end.
