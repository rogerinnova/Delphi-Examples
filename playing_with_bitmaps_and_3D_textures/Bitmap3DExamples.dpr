program Bitmap3DExamples;

uses
  System.StartUpCopy,
  FMX.Forms,
  BitMap3DForm in 'BitMap3DForm.pas' {DemoForm3DShapesWithTexture},
  UsefulBmpConcepts in 'UsefulBmpConcepts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDemoForm3DShapesWithTexture, DemoForm3DShapesWithTexture);
  Application.Run;
end.
