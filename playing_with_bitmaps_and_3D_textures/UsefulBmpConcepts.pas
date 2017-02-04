unit UsefulBmpConcepts;

interface
uses System.RTLConsts, SysUtils, FMX.MaterialSources, System.UIConsts,
  FMX.Objects3D, FMX.Types3D, Classes, System.Math, System.Types, System.UITypes,
  FMX.Graphics,
  FMX.Types, FMX.Ani, FMX.Utils, System.Math.Vectors;

Procedure DefaultTexture(AControl: TCustomMesh);
Procedure DefaultLandscapeMaterial(AControl: TCustomMesh);
Procedure DefaultMatTexture(AControl: TCustomMesh);
Procedure DefaultMaterial(AControl: TCustomMesh);
Function ColorBitMap(Ax, Ay: Integer;
      Const ASat:Single=0.75;Const ALum:Single=0.5;
      AxCont:Boolean=False;AyCont:Boolean=False): TBitMap;
Function ColorLumBitMap(Ax, Ay: Integer;Const ASat:Single=0.75):TBitMap;
Function ColorSatBitMap(Ax, Ay: Integer;Const ALum:Single=0.5):TBitMap;
Function ColorTransparentBitMap(Ax, Ay: Integer;Const ASat:Single=0.75;Const ALum:Single=0.5):TBitMap;
Function LandscapeBitMap(Ax, Ay: Integer; AFormat:Integer; Const ASat:Single=0.75;
      Const ALum:Single=0.5;Const AOpacity:Single=0.25):TBitMap;

implementation

Procedure DefaultLandscapeMaterial(AControl: TCustomMesh);
var
  mat: TLightMaterialSource;
begin
// DefaultTexture(AControl);

  mat := TLightMaterialSource.Create(AControl);
  mat.Parent := AControl;
//  mat.Ambient := claDarkgreen;
  mat.Ambient := claBlack;
  mat.Diffuse := claWhite;
  mat.Emissive := claNull;//claTomato;
  mat.Texture := LandscapeBitMap(10, 360,2,0.75,0.5,0.3);
  AControl.MaterialSource := mat;{}
end;

Procedure DefaultMatTexture(AControl: TCustomMesh);
var
  mat: TLightMaterialSource;
begin
  mat := TLightMaterialSource.Create(AControl);
  mat.Parent := AControl;
  mat.Ambient := claBlack;
  mat.Diffuse := claViolet;
  mat.Emissive := claTomato;
  mat.Texture := ColorBitMap(10, 360);
  AControl.MaterialSource := mat;
end;

Procedure DefaultMaterial(AControl: TCustomMesh);
var
  mat: TLightMaterialSource;
begin
  mat := TLightMaterialSource.Create(AControl);
  mat.Parent := AControl;
  mat.Ambient := claBlack;
  mat.Diffuse := claViolet;
  mat.Emissive := claTomato;
  AControl.MaterialSource := mat;
end;

Function ColorBitMap(Ax, Ay: Integer;
      Const ASat,ALum:Single;
      AxCont,AyCont:Boolean): TBitMap;
Var
  Bmp: TBitMap;
  BmpData: TBitmapData;
  k, j: Integer;
begin
  Bmp := TBitMap.Create(Ax, Ay);
  if Bmp.Map(TMapAccess.ReadWrite, BmpData) then
    for k := 0 to Ay - 1 do
      for j := 0 to Ax - 1 do
        if ((j mod 60 = 0) And AxCont) or ((k Mod 60 = 0)And AyCont) then
          BmpData.SetPixel(j, k, claBlack)
        else
          BmpData.SetPixel(j, k, CorrectColor(HSLtoRGB( { (Ak-k) } k / Ay,
            ASat, ALum)));
  Bmp.Unmap(BmpData);
  Result := Bmp;
end;

Function ColorLumBitMap(Ax, Ay: Integer;Const ASat:Single):TBitMap;
Var
  Bmp: TBitMap;
  BmpData: TBitmapData;
  k, j: Integer;
begin
  Bmp := TBitMap.Create(Ax, Ay);
  if Bmp.Map(TMapAccess.ReadWrite, BmpData) then
    for k := 0 to Ay - 1 do
      for j := 0 to Ax - 1 do
          BmpData.SetPixel(j, k, CorrectColor(HSLtoRGB( { (Ak-k) } k / Ay,
            ASat, j/ax)));
  Bmp.Unmap(BmpData);
  Result := Bmp;
end;

Function ColorSatBitMap(Ax, Ay: Integer;Const ALum:Single):TBitMap;
Var
  Bmp: TBitMap;
  BmpData: TBitmapData;
  k, j: Integer;
begin
  Bmp := TBitMap.Create(Ax, Ay);
  if Bmp.Map(TMapAccess.ReadWrite, BmpData) then
    for k := 0 to Ay - 1 do
      for j := 0 to Ax - 1 do
          BmpData.SetPixel(j, k, CorrectColor(HSLtoRGB( { (Ak-k) } k / Ay,
             j/ax, ALum)));
  Bmp.Unmap(BmpData);
  Result := Bmp;
end;

Function ColorTransparentBitMap(Ax, Ay: Integer;Const ASat:Single=0.75;Const ALum:Single=0.5):TBitMap;
Var
  Bmp: TBitMap;
  PixCol: TAlphaColor;
  BmpData: TBitmapData;
  k, j: Integer;
begin
  Bmp := TBitMap.Create(Ax, Ay);
  if Bmp.Map(TMapAccess.ReadWrite, BmpData) then
    for k := 0 to Ay - 1 do
      for j := 0 to Ax - 1 do
        Begin                                //0 to 1
          PixCol:= CorrectColor(HSLtoRGB( { (Ak-k) } k / Ay,
            ASat, ALum));
          BmpData.SetPixel(j, k, MakeColor(PixCol,1-j/ax) );
        End;
  Bmp.Unmap(BmpData);
  Result := Bmp;
end;

Function LandscapeBitMap(Ax, Ay: Integer; AFormat:Integer;Const ASat:Single;
     Const ALum:Single;Const AOpacity:Single):TBitMap;

{sub} function FormatColor(aVert,aHorz:Single; AColNo:Integer; AColRng:Single):TAlphaColor;
  Var
   PercentInCols:Single;
  begin
   Result:=claBrown;
   case AFormat of
     0: Result:=  CorrectColor(HSLtoRGB( aHorz,
            ASat, ALum));
     1: Result:=MakeColor(Round(255*0.5{aj/ax}),125,Round(255*aHorz));
     2: case AColNo of
         0..3: Result:=MakeColor(145,Round(250*(1-aHorz)),0);
         4..5: Begin
           PercentInCols:=(aVert-4*AColRng)/AColRng/2;
           if PercentInCols<0.0 then
             PercentInCols:=0.0;
           Result:=MakeColor(Round(145+(255-145)*PercentInCols),
             Round(250*(1-aHorz*(1-PercentInCols))),0);
             //>> Yellow
         End;
         6..7: Result:=MakeColor(
              MakeColor(Round(100*aHorz),0,Round(100+100*aHorz)),0.70);
              //Blue
         else
          Result:=claYellow;
        end;

     3: Result:=MakeColor(145,Round(220*aVert),Round(180*aHorz));
//     3: Result:=MakeColor(Round(255*aj/ax),Round(255*ak/ay),125);
     4: Result:=MakeColor(Round(255*aVert),255,Round(255*aHorz));
     5: Result:=MakeColor(Round(255*aVert),Round(255*aHorz),255);
     6: Result:=MakeColor(255,Round(255*aVert),Round(255*aHorz));
     7: Result:=MakeColor(255,Round(255*aVert),Round(255*aHorz));
     8: Result:=MakeColor(Round(255*aVert),Round(255*aHorz),255);
     9: Result:=MakeColor(Round(255*aVert),Round(255*aHorz),255-Round(255*aHorz));
     10: Result:=MakeColor(Result,aVert);
   end;
   Result:=MakeColor(result,AOpacity);
  end;

Var
  Bmp: TBitMap;
  PixCol: TAlphaColor;
  BmpData: TBitmapData;
  k, j, ColNo, ColDiv: Integer;
  JFract,KFract:single;
  ColRng:Single;
begin
  Bmp := TBitMap.Create(Ax, Ay);
  ColDiv := ax div 7;
  ColRng := ColDiv/ax; //as % of whole
  if Bmp.Map(TMapAccess.ReadWrite, BmpData) then
    for k := 0 to Ay - 1 do
     begin
      KFract:= k/ay;     //0 to 1
      for j := 0 to Ax - 1 do
        Begin
          JFract:= J/ax;     //0 to 1
          ColNo:=  j div ColDiv;
          PixCol:= FormatColor(JFract,KFract,ColNo,ColRng);
          BmpData.SetPixel(j, k, PixCol);
        End;
    End;
  Bmp.Unmap(BmpData);
  Result := Bmp;
end;


Procedure DefaultTexture(AControl: TCustomMesh);
var
  Txr: TTextureMaterialSource;
begin
  Txr := TTextureMaterialSource.Create(AControl);
  Txr.Parent := AControl;
  Txr.Texture := ColorBitMap(1, 360);
  AControl.MaterialSource := Txr;
end;
{

  http://edn.embarcadero.com/article/42007

  procedure TForm1.GenerateMesh;
  const
  MaxX = 30;
  MaxZ = 30;
  var
  u, v : Double;
  P : array [0..3] of TPoint3D;
  d : Double;
  NP, NI : Integer;
  BMP : TBitmap;
  k : Integer;
  begin
  Mesh1.Data.Clear;

  d := 0.5;

  NP := 0;
  NI := 0;

  Mesh1.Data.VertexBuffer.Length := Round(2*MaxX*2*MaxZ/d/d)*4;
  Mesh1.Data.IndexBuffer.Length := Round(2*MaxX*2*MaxZ/d/d)*6;

  BMP := TBitmap.Create(1,360);
  for k := 0 to 359 do
  BMP.Pixels[0,k] := CorrectColor(HSLtoRGB(k/360,0.75,0.5));

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

  Mesh1.Material.Texture := BMP;
  end; }

end.
