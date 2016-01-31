program Camera;

uses
  FMX.Forms,
  Play3dFormWithMovingCamera in 'Play3dFormWithMovingCamera.pas' {Form1},
  FlyingCamera in 'ThreeDLibs\FlyingCamera.pas',
  ThreeDUtils in 'ThreeDLibs\ThreeDUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
