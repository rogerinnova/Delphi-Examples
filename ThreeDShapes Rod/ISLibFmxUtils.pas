unit ISLibFmxUtils;
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
uses FMX.Types3D,System.SysUtils,System.Types,FMX.types,
{$IFDEF ISXE6_DELPHI}
System.Math.Vectors;
{$Else}
System.Math;
{$Endif}

type
 TSqArray =array [0..3] of TPoint3D;
 TPathArray = Array of TPoint3D;

Function SqArray(ca,cb,cc,cd:TPoint3D):TSqArray;
Function RadiusArray(ARadius,Ax0,Ay0,Az0,AxAlpha:Single; AQuadPoints:integer):TPathArray;
Function Average3D(AP1,AP2:TPoint3D):TPoint3D;
Function MovePoint(v:TPoint3D;Ax0,Ay0,Az0:Single):TPoint3D;
Procedure  MovePath(APath:TPathArray;Ax0,Ay0,Az0:Single);


implementation

Function SqArray(ca,cb,cc,cd:TPoint3D):TSqArray;
   begin
     Result[0]:= ca;
     Result[1]:= cb;
     Result[2]:= cc;
     Result[3]:= cd;
   end;

Function QuadArray(ARadius,AxAlpha:Single; AQuadPoints:integer):TPathArray;
Var
  DeltaX,DeltaY,
  RadiusSq,XsqYsq:Single;
  I:Integer;
begin
  if AQuadPoints<1 then
     raise Exception.Create('QuadArray');

  SetLength(Result,AQuadPoints+1);
  //Result[0].X:= ARadius*Sin(AxAlpha+pi/2);
  //Result[0].Y:= ARadius*Cos(AxAlpha+pi/2);
  Result[0].X:= ARadius*Sin(AxAlpha);
  Result[0].Y:= -ARadius*Cos(AxAlpha);
  Result[0].Z:= 0;
  Result[AQuadPoints].X:=  0;
  Result[AQuadPoints].Y:=  0;
  Result[AQuadPoints].Z:= ARadius;
  if AQuadPoints<2 then Exit;

  DeltaX:=Result[0].X/AQuadPoints;
  DeltaY:=Result[0].Y/AQuadPoints;
  RadiusSq:=ARadius*ARadius;
  for I := 1 to AQuadPoints-1 do
     begin
       Result[AQuadPoints-i].X:= {Sin(DeltaAlpha*i) ;//}DeltaX*I;
       Result[AQuadPoints-i].Y:= {Cos(DeltaAlpha*i) ; //}DeltaY*I;
       XsqYsq:=Result[AQuadPoints-i].Y*Result[AQuadPoints-i].Y+
           Result[AQuadPoints-i].X*Result[AQuadPoints-i].X;
       if XsqYsq<RadiusSq then
           Result[AQuadPoints-i].Z:= Sqrt(RadiusSq-XsqYsq)
           else
           Result[AQuadPoints-i].Z:=0;
     end;
end;


Function NegateAll(v:TPoint3D):TPoint3D;
begin
  Result:=v;
  Result.X:=-v.X;
  Result.Y:=-v.Y;
  Result.Z:=-v.Z;
end;

Function NegateXY(v:TPoint3D):TPoint3D;
begin
  Result:=v;
  Result.X:=-v.X;
  Result.Y:=-v.Y;
end;

Function NegateX(v:TPoint3D):TPoint3D;
begin
  Result:=v;
  Result.X:=-v.X;
end;

Function NegateY(v:TPoint3D):TPoint3D;
begin
  Result:=v;
  Result.Y:=-v.Y;
end;

Function NegateZ(v:TPoint3D):TPoint3D;
begin
  Result:=v;
  Result.Z:=-v.Z;
end;

Function MovePoint(v:TPoint3D;Ax0,Ay0,Az0:Single):TPoint3D;
begin
  Result.Z:=v.Z+Az0;
  Result.X:=v.X+Ax0;
  Result.Y:=v.Y+Ay0;
end;

Procedure  MovePath(APath:TPathArray;Ax0,Ay0,Az0:Single);
Var
  I:Integer;
begin
  for I := Low(APath) to High(APath) do
    APath[I]:= MovePoint(APath[I],Ax0,Ay0,Az0);
end;


Function RadiusArray(ARadius,Ax0,Ay0,Az0,AxAlpha:Single; AQuadPoints:integer):TPathArray;
Var
  QArray:TPathArray;
  I:Integer;
begin
  QArray:=QuadArray(ARadius,AxAlpha,AQuadPoints);
  SetLength(Result,AQuadPoints*4);
  Result[0]:=QArray[0];
  Result[AQuadPoints*2]:=NegateXY(QArray[0]);
  Result[AQuadPoints]:=QArray[AQuadPoints];
  Result[AQuadPoints*3]:=NegateZ(QArray[AQuadPoints]);

  for I := 0 to AQuadPoints-2 do
    begin
      Result[i+1]:= QArray[i+1];
      Result[AQuadPoints*2-i-1]:= NegateXY(QArray[i+1]);
      Result[AQuadPoints*2+i+1]:= NegateAll(QArray[i+1]);
      Result[AQuadPoints*4-i-1]:= NegateZ(QArray[i+1]);
    end;

  MovePath(Result,Ax0,Ay0,Az0);

end;

Function Average3D(AP1,AP2:TPoint3D):TPoint3D;
begin
  Result.X:=(AP1.X+AP2.X)/2;
  Result.Y:=(AP1.Y+AP2.Y)/2;
  Result.Z:=(AP1.Z+AP2.Z)/2;
end;

end.
