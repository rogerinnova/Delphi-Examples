unit Rod3DForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Dialogs, FMX.Objects3D, FMX.Types3D,
  ThreeDRod, FMX.MaterialSources,FMX.Controls3D, FMX.Ani, System.Math.Vectors;

type
  TFormRodDemo = class(TForm3D)
    ThreeDRod1: TThreeDRod;
    ThreeDRod2: TThreeDRod;
    ThreeDRod3: TThreeDRod;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    LightMaterialSource2: TLightMaterialSource;
    FloatAnimation1: TFloatAnimation;
    ThreeDRod4: TThreeDRod;
    ThreeDRod5: TThreeDRod;
    TextureMaterialSource1: TTextureMaterialSource;
    BitmapAnimation1: TBitmapAnimation;
    TextureMaterialSource2: TTextureMaterialSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRodDemo: TFormRodDemo;

implementation

{$R *.fmx}

end.
