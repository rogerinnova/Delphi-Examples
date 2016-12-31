unit MathsForm3D;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects3D, FMX.Types3D,
  FMX.Layouts, FMX.Layers3D, System.Math.Vectors, FMX.Controls3D,FMX.Graphics,
  FMX.Forms3D, System.UIConsts,
  FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TMathsForm = class(TForm3D)
    Camera1: TCamera;
    Layer3D1: TLayer3D;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Layout3D1: TLayout3D;
    GridXY: TGrid3D;
    GridXZ: TGrid3D;
    GridYZ: TGrid3D;
    Mesh1: TMesh;
    Text3D1: TText3D;
    Light1: TLight;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GenerateMesh(Func : Integer);
  end;

var
  MathsForm: TMathsForm;

implementation

{$R *.fmx}

uses Math;

var
  Down: TPointF;

var
  MinF : Double = 2000;
  MaxF : Double = -2000;

function f(Func : Integer; x,z : Double) : Double;
var
  temp : Double;
begin
  temp := x*x+z*z;
  if temp < Epsilon then
    temp := Epsilon;

  case Func of
    1 : Result := -2000*Sin(temp/180*Pi)/temp;
    2 : Result := -5*Exp(Sin(x/10)-Cos(z/10));
    3 : Result := -5*Exp(Sin(x/10)+Cos(z/10));
    4 : Result := -5*Exp(Sin(Sqrt(Abs(x)))+Cos(Sqrt(Abs(z))));
    5 : Result := -7*Exp(ArcTan2(x*x,z*z));
  end;

  if Result < MinF then
    MinF := Result;
  if Result > MaxF then
    MaxF := Result;
end;

procedure TMathsForm.GenerateMesh(Func : Integer);
const
  MaxX = 30;
  MaxZ = 30;
var
  u, v : Double;
  P : array [0..3] of TPoint3D;
  d : Double;
  NP, NI : Integer;
  BMP : TBitmap;
  BmpData:TBitmapData;
  k,j : Integer;
  ColTexture:TTextureMaterialSource;
begin
  Mesh1.Data.Clear;
  Mesh1.WrapMode:=TMeshWrapMode.Original;

  d := 0.5;

  NP := 0;
  NI := 0;

  // We have to set these up front. The buffers are cleared every time Length is set.
  Mesh1.Data.VertexBuffer.Length := Round(2*MaxX*2*MaxZ/d/d)*4;
  Mesh1.Data.IndexBuffer.Length := Round(2*MaxX*2*MaxZ/d/d)*6;

  BMP := TBitmap.Create(20,360);
  Try
  if Bmp.Map(TMapAccess.ReadWrite,BmpData)then
  for k := 0 to 359 do
   for j:=0 to 19 do
    BmpData.SetPixel(j,k, CorrectColor(HSLtoRGB(k/360,0.75,0.5)));

  Bmp.Unmap(BmpData);
  u := -MaxX;
  while u < MaxX do begin
    v := -MaxZ;
    while v < MaxZ do begin
      // Set up the points in the XZ plane
      P[0].x := u;
      P[0].z := v;
      P[1].x := u+d;
      P[1].z := v;
      P[2].x := u+d;
      P[2].z := v+d;
      P[3].x := u;
      P[3].z := v+d;

      // Calculate the corresponding function values for Y = f(X,Z)
      P[0].y := f(Func,P[0].x,P[0].z);
      P[1].y := f(Func,P[1].x,P[1].z);
      P[2].y := f(Func,P[2].x,P[2].z);
      P[3].y := f(Func,P[3].x,P[3].z);

      with Mesh1.Data do begin
        // Set the points
        with VertexBuffer do begin
          Vertices[NP+0] := P[0];
          Vertices[NP+1] := P[1];
          Vertices[NP+2] := P[2];
          Vertices[NP+3] := P[3];
        end;

        // Map the colors
        with VertexBuffer do begin
          TexCoord0[NP+0] := PointF(0,(P[0].y+35)/45);
          TexCoord0[NP+1] := PointF(0,(P[1].y+35)/45);
          TexCoord0[NP+2] := PointF(0,(P[2].y+35)/45);
          TexCoord0[NP+3] := PointF(0,(P[3].y+35)/45);
        end;

        // Map the triangles
        IndexBuffer[NI+0] := NP+1;
        IndexBuffer[NI+1] := NP+2;
        IndexBuffer[NI+2] := NP+3;
        IndexBuffer[NI+3] := NP+3;
        IndexBuffer[NI+4] := NP+0;
        IndexBuffer[NI+5] := NP+1;
      end;

      NP := NP+4;
      NI := NI+6;

      v := v+d;
    end;
    u := u+d;
  end;
  ColTexture:= TTextureMaterialSource.Create(Mesh1);
  ColTexture.Parent:=Mesh1;
  ColTexture.Texture.Assign(Bmp);
  Finally
    BMp.Free;
  End;
  Mesh1.MaterialSource:=ColTexture;
  Mesh1.Visible:=true;
end;

procedure TMathsForm.Button1Click(Sender: TObject);
begin
  Text3D1.Text := Button1.Text;
  GenerateMesh(1);
end;

procedure TMathsForm.Button2Click(Sender: TObject);
begin
  Text3D1.Text := Button2.Text;
  GenerateMesh(2);
end;

procedure TMathsForm.Button3Click(Sender: TObject);
begin
  Text3D1.Text := Button3.Text;
  GenerateMesh(3);
end;

procedure TMathsForm.Button4Click(Sender: TObject);
begin
  Text3D1.Text := Button4.Text;
  GenerateMesh(4);
end;

procedure TMathsForm.Button5Click(Sender: TObject);
begin
  Text3D1.Text := Button5.Text;
  GenerateMesh(5);
end;

Procedure TMathsForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Down := PointF(X, Y);
end;

procedure TMathsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if (ssLeft in Shift) then
  begin
    { rotate Z }
    Layout3D1.RotationAngle.Z := Layout3D1.RotationAngle.Z + ((X - Down.X) * 0.3);
    { rotate X }
    Layout3D1.RotationAngle.Y := Layout3D1.RotationAngle.Y + ((Y - Down.Y) * 0.3);
    Down := PointF(X, Y);
  end;
end;

procedure TMathsForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  //Camera1.Position.Vector := Vector3DAdd(Camera1.Position.Vector, Vector3DScale(Vector3D(0, 0, 1), (WheelDelta / 120) * 0.3));
  Camera1.Position.Vector := (Camera1.Position.Vector + TVector3D.Create(0, 0, (WheelDelta / 120) * 0.3));
end;

end.
