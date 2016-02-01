#Some Notes On Flying Camera


This looks at a first try at a first person shooter game in Delphi 3D

FlyingCammera.pas
contains a Camera Object which you can "fly" or
"step" arround your 3D Sceen.

To Implement
 1.  Add  FlyingCamera to the uses clause
 2.  Simply add a TFlyingCamera "FlyCam" in the private section of your 3D form,
 3.  Add the following to the Forms OnCreate Event

```Delphi
procedure TForm1.Form3DCreate(Sender: TObject);
begin
 FlyCam:=AssignFlyingCameraToForm(Self,5,26,-29);
 FlyCam.StepFormat:=true;
 //FlyCam.Speed:=0.1;
end;
```

The Function
AssignFlyingCameraToForm(A3dForm: TForm3D; AX,AY,AZ:Single): TFlyingCamera;
1. Adds a Camera at AX,AY,AZ and points it to the point 0,0,0
2. Sets A3dForm Camera Property
3. Sets Forms UseDesign Camera  to false
4. Allocates the OnKeyDown Event (if Unassigned)

```
The Cammera Controls then become
Key Controls
  'W', 'w': MoveForward(0.3);
  'S', 's': MoveForward(-0.3);
  '+':      FSpeed := FSpeed + FSpeedInc;
  '-':      FSpeed := FSpeed - FSpeedInc;
  ' ':      Begin
            Fire(Self, F3dForm, FBankAngle, TracerL);
            Fire(Self, F3dForm, FBankAngle, TracerR);
            End;
  vkLeft:   Bank(True) or Left Turn
  vkUp:     Rotate Up
  vkRight:  Bank(False) or Right Turn;
  vkDown:   Rotate Down
  vkNext:   Increase Rotation Angle
  vkPrior:  Decrease Rotation Angle
  vkInsert: Increase Spead Increment
  vkDelete: Decrease Spead Increment
  vkAdd:    Increase Spead;
  vkSubtract: Decrease Spead;
```


ThreeDLibs.pas
A place for 3D utility functions andObjects used in FlyingCamera



TGunsight
An Implemntation of a "HeadsUp" gunsight of 4 Opposing Cones
Typically place in front of the camera at X=0 Y=0


TProjectile
A first cut implemenation of a tracer or rocket

Tracer fires from each "wing" and converge after x seconds to
the camera ray (center of Gunsight)

Rocket as yet no implemntation



Current Problems
There seems to be errors in the projectile setup which results in bad direction
when angle X and angle Y changes.


